#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/format
         racket/list)
(require "../../../base.rkt"
         "../../../flonum.rkt"
         "../../../bigfloat.rkt")

(define-syntax (b stx)
  (syntax-case stx (nonan)
    [(b bff argnr nonan)
     (with-syntax ([(args ...)(build-list (syntax-e #'argnr) (λ (i)(format-id stx "z~a" i)))])
       #'(λ (args ...)
           (if (ormap flnan? (list args ...))
               +nan.0+nan.0i
               (bigfloat->real (bff (bf args) ...)))))]
    [(b bff argnr)
     (with-syntax ([(args ...)(build-list (syntax-e #'argnr) (λ (i)(format-id stx "z~a" i)))])
       #'(λ (args ...)
           (bigfloat->real (bff (bf args) ...))))]))

(define SAMPLES (make-parameter 1000))
(define LEVELS (make-parameter (list .5 1 2 5 10 100 100 1000)))
(define RATIONAL? (make-parameter #f))
(define PRINTLIMIT (make-parameter #f))

(define (fl-tester flts-to-test
                   flt-for-check
                   argnr
                   #:samples [samples (SAMPLES)]
                   #:levels [levels (LEVELS)]
                   #:names [names (map ~a flts-to-test)]
                   #:rational? [r? (RATIONAL?)]
                   #:print-limit [print-limit (PRINTLIMIT)])
  (define LS (sort (remove-duplicates (cons 0 (cons +inf.0 levels))) <))
  (define len (length flts-to-test))

  (for/fold ([Hs (make-list len #hash())]
             #:result (map cons names Hs))
            ([_ (in-range samples)])

    (define zs (build-list argnr (λ (_)(random-flonum #:rational? r?))))
    (define b (apply flt-for-check zs))
    (define as (map (λ (flt) (apply flt zs)) flts-to-test))
    (define es (map (λ (z) (flulp-error z b)) as))
    
    (define pl (if print-limit
                   (if (list? print-limit)
                       (if (equal? (length print-limit) len)
                           print-limit
                           (raise-argument-error 'fl-tester "same length as fltcs-to-check" print-limit))
                       (make-list len print-limit))
                   (make-list len #f)))
    
    (for/list ([H (in-list Hs)]
               [e (in-list es)]
               [pl (in-list pl)])
      (when (and pl (<= pl e))
        (println (list zs (fl b) (map cons as es)))
        (define b* (apply flt-for-check zs))
        (when (not (equal? b b*)) (printf "***mpfr failed: ~a***\n" (fl b*))))
      
      (define H*
        (if (and (not (empty? (cdr es))) (< e (apply min (remove e es))))
            (hash-update H 'best add1 0)
            H))
      
      (for/first ([l- (in-list LS)]
                  [l+ (in-list (cdr LS))]
                  #:when (<= l- e l+))
        (hash-update H* (cons l- l+) add1 0)))))

;; random flonum,
;; favor   0 (2%)
;;       nan (2%)
;;       inf (4%)
;; subnormal (6%)
(define (random-flonum #:rational? [r? #f])
  (case (random (if r? 3 0) 50)
    [(0) +nan.0]
    [(1) +inf.0]
    [(2) -inf.0]
    [(3) (* (if (= (random 2) 0) 1 -1) 0.0)]
    [(4 5 6) ;subnormal
     (* (if (= (random 2) 0) 1 -1)
        (floating-point-bytes->real (integer->integer-bytes (random-bits 52) 8 #f #f) #f))]
    [else;non nan/inf number
     (* (if (= (random 2) 0) 1 -1)
        (floating-point-bytes->real (integer->integer-bytes (random-bits 62) 8 #f #f) #f))]))

(define (print-a-line . rst)
  (displayln (apply string-append (add-between (map (λ (x)(~a x #:width 10 #:align 'right)) rst) " |"))))
(define (print-header)
  (print-a-line "name" "best" "0-.5" ".5-1" "1-2" "2-5" "5-10" "10-100" "100-1000" "1000-+inf"))
(define (print-it Hs)
  (for ([l (in-list Hs)])
    (define H (cdr l))
    (print-a-line (car l)
                  (hash-ref H 'best 0)
                  (hash-ref H '(0 . .5) 0)
                  (hash-ref H '(.5 . 1) 0)
                  (hash-ref H '(1 . 2) 0)
                  (hash-ref H '(2 . 5) 0)
                  (hash-ref H '(5 . 10) 0)
                  (hash-ref H '(10 . 100) 0)
                  (hash-ref H '(100 . 1000) 0)
                  (hash-ref H '(1000 . +inf.0) 0))))

(module+ main
  (SAMPLES 1e6)
  (RATIONAL? #t)
  (print-header)
  )

#;(module+ main
  "printing from err>1 for fl functions"
  "addition"
  (print-it (fl-tester (list + fl+) (b bf+ 2) 2 #:names '(+_rat fl+_rat) #:rational? #t #:print-limit '(#f 1)))
  (print-it (fl-tester (list + fl+) (b bf+ 2) 2 #:names '(+_all fl+_all) #:rational? #f #:print-limit '(#f 1)))
  "addition×4"
  (print-it (fl-tester (list + (λ xs (flsum xs))) (b bf+ 4) 4 #:names '(+_rat flsum_rat) #:rational? #t #:print-limit '(#f 1)))
  (print-it (fl-tester (list + (λ xs (flsum xs))) (b bf+ 4) 4 #:names '(+_all flsum_all) #:rational? #f #:print-limit '(#f 1)))
  )

#;(module+ main
  "subtraction"
  (print-it (fl-tester (list - fl-) (b bf- 2) 2 #:names '(-_rat fl-_rat) #:rational? #t #:print-limit '(#f 1)))
  (print-it (fl-tester (list - fl-) (b bf- 2) 2 #:names '(-_all fl-_all) #:rational? #f #:print-limit '(#f 1)))
  ;"subtraction×4"
  ;(print-it (fl-tester (list - fl-) (b bf- 4) 4 #:names '(-_rat fl-_rat) #:rational? #t #:print-limit '(#f 1)))
  ;(print-it (fl-tester (list - fl-) (b bf- 4) 4 #:names '(-_all fl-_all) #:rational? #f #:print-limit '(#f 1)))
  )

#;(module+ main
  "multiplication"
  (print-it (fl-tester (list * fl*) (b bf* 2) 2 #:names '(*_rat fl*_rat) #:rational? #t #:print-limit '(#f 1)))
  (print-it (fl-tester (list * fl*) (b bf* 2) 2 #:names '(*_all fl*_all) #:rational? #f #:print-limit '(#f 1)))
  ;"multiplication×3"
  ;(print-it (fl-tester (list * fl*) (b bf* 3) 3 #:names '(*_rat fl*_rat) #:rational? #t #:print-limit '(#f 1)))
  ;(print-it (fl-tester (list * fl*) (b bf* 3) 3 #:names '(*_all fl*_all) #:rational? #f #:print-limit '(#f 1)))
  ;"multiplication×4"
  ;(print-it (fl-tester (list * fl*) (b bf* 4) 4 #:names '(*_rat fl*_rat) #:rational? #t #:print-limit '(#f 1)))
  ;(print-it (fl-tester (list * fl*) (b bf* 4) 4 #:names '(*_all fl*_all) #:rational? #f #:print-limit '(#f 1)))
  ;"multiplication×5"
  ;(print-it (fl-tester (list * fl*) (b bf* 5) 5 #:names '(*_rat fl*_rat) #:rational? #t #:print-limit '(#f 1)))
  ;(print-it (fl-tester (list * fl*) (b bf* 5) 5 #:names '(*_all fl*_all) #:rational? #f #:print-limit '(#f 1)))
  )

#;(module+ main
  "division"
  (print-it (fl-tester (list / fl/) (b bf/ 2) 2 #:names '(/_rat fl/_rat) #:rational? #t #:print-limit '(#f 1)))
  (print-it (fl-tester (list / fl/) (b bf/ 2) 2 #:names '(/_all fl/_all) #:rational? #f #:print-limit '(#f 1)))
  
  "inversion"
  (print-it (fl-tester (list / fl/) (b bf/ 1) 1 #:names '(1/_rat fl1/_rat) #:rational? #t #:print-limit '(#f 1)))
  (print-it (fl-tester (list / fl/) (b bf/ 1) 1 #:names '(1/_all fl1/_all) #:rational? #f #:print-limit '(#f 1)))
  )

#;(module+ main
  (define (log* a)(if (< a 0) +nan.0 (log a)))
  "exp"
  (print-it (fl-tester (list exp flexp) (b bfexp 1) 1 #:names '(exp_rat flexp_rat) #:rational? #t #:print-limit '(#f 1)))
  (print-it (fl-tester (list exp flexp) (b bfexp 1) 1 #:names '(exp_all flexp_all) #:rational? #f #:print-limit '(#f 1)))
  "log"
  (print-it (fl-tester (list log* fllog) (b bflog 1) 1 #:names '(log_rat fllog_rat) #:rational? #t #:print-limit '(#f 1)))
  (print-it (fl-tester (list log* fllog) (b bflog 1) 1 #:names '(log_all fllog_all) #:rational? #f #:print-limit '(#f 1)))
  )

#;(module+ main
  (define (sqrt* a)(if (< a 0) +nan.0 (sqrt a)))
  (define (expt* a b)(define c (expt a b))(if (real? c) c +nan.0))
  "expt"
  (print-it (fl-tester (list expt* flexpt) (b bfexpt 2) 2 #:names '(expt_rat flexpt_rat) #:rational? #t #:print-limit '(#f 1)))
  (print-it (fl-tester (list expt* flexpt) (b bfexpt 2) 2 #:names '(expt_all flexpt_all) #:rational? #f #:print-limit '(#f 1)))
  "sqr"
  (print-it (fl-tester (list sqr) (b bfsqr 1) 1 #:names '(sqr_rat) #:rational? #t #:print-limit '(#f)))
  (print-it (fl-tester (list sqr) (b bfsqr 1) 1 #:names '(sqr_all) #:rational? #f #:print-limit '(#f)))
  "sqrt"
  (print-it (fl-tester (list sqrt* flsqrt) (b bfsqrt 1) 1 #:names '(sqrt_rat flsqrt_rat) #:rational? #t #:print-limit '(#f 1)))
  (print-it (fl-tester (list sqrt* flsqrt) (b bfsqrt 1) 1 #:names '(sqrt_all flsart_all) #:rational? #f #:print-limit '(#f 1)))
  )

#;(module+ main
  "sin";; 10/1e6 errors in 0.5~1
  (print-it (fl-tester (list sin flsin) (b bfsin 1) 1 #:names '(sin_rat flsin_rat) #:rational? #t #:print-limit '(#f 1)))
  (print-it (fl-tester (list sin flsin) (b bfsin 1) 1 #:names '(sin_all flsin_all) #:rational? #f #:print-limit '(#f 1)))
  "cos";; 10/1e6 errors in 0.5~1
  (print-it (fl-tester (list cos flcos) (b bfcos 1) 1 #:names '(cos_rat flcos_rat) #:rational? #t #:print-limit '(#f 1)))
  (print-it (fl-tester (list cos flcos) (b bfcos 1) 1 #:names '(cos_all flcos_all) #:rational? #f #:print-limit '(#f 1)))
  "tan"
  (print-it (fl-tester (list tan fltan) (b bftan 1) 1 #:names '(tan_rat fltan_rat) #:rational? #t #:print-limit '(#f 1)))
  (print-it (fl-tester (list tan fltan) (b bftan 1) 1 #:names '(tan_all fltan_all) #:rational? #f #:print-limit '(#f 1)))
  )

#;(module+ main
  (define (asin* a)(define b (asin a))(if (real? b) b +nan.0))
  (define (acos* a)(define b (acos a))(if (real? b) b +nan.0))
  (define (atan* a)(define b (atan a))(if (real? b) b +nan.0))
  "asin"
  (print-it (fl-tester (list asin* flasin) (b bfasin 1) 1 #:names '(asin_rat flasin_rat) #:rational? #t #:print-limit '(#f 1)))
  (print-it (fl-tester (list asin* flasin) (b bfasin 1) 1 #:names '(asin_all flasin_all) #:rational? #t #:print-limit '(#f 1)))
  "acos"
  (print-it (fl-tester (list acos* flacos) (b bfacos 1) 1 #:names '(acos_rat flacos_rat) #:rational? #t #:print-limit '(#f 1)))
  (print-it (fl-tester (list acos* flacos) (b bfacos 1) 1 #:names '(acos_all flacos_all) #:rational? #f #:print-limit '(#f 1)))
  "atan"
  (print-it (fl-tester (list atan* flatan) (b bfatan 1) 1 #:names '(atan_rat flatan_rat) #:rational? #t #:print-limit '(#f 1)))
  (print-it (fl-tester (list atan* flatan) (b bfatan 1) 1 #:names '(atan_all flatan_all) #:rational? #f #:print-limit '(#f 1)))
  )

#;(module+ main
    
  "sinh";; 9940/1e6 errors in 0.5~2.5
  (print-it (fl-tester (list sinh flsinh) (b bfsinh 1) 1 #:names '(sinh_rat flsinh_rat) #:rational? #t #:print-limit '(#f 1)))
  (print-it (fl-tester (list sinh flsinh) (b bfsinh 1) 1 #:names '(sinh_all flsinh_all) #:rational? #f #:print-limit '(#f 1)))
  "cosh";; 422/1e6 errors in 0.5~1
  (print-it (fl-tester (list cosh flcosh) (b bfcosh 1) 1 #:names '(cosh_rat flcosh_rat) #:rational? #t #:print-limit '(#f 1)))
  (print-it (fl-tester (list cosh flcosh) (b bfcosh 1) 1 #:names '(cosh_all flcosh_all) #:rational? #f #:print-limit '(#f 1)))
  "tanh (using err>3)"
  (print-it (fl-tester (list tanh fltanh) (b bftanh 1) 1 #:names '(tanh_rat fltanh_rat) #:rational? #t #:print-limit '(#f 3)))
  (print-it (fl-tester (list tanh fltanh) (b bftanh 1) 1 #:names '(tanh_all fltanh_all) #:rational? #f #:print-limit '(#f 3)))
  )

(module+ main
  (define (flasinh_old x)
    (cond [(x . fl< . 0.0)  (- (flasinh (- x)))]
          [(x . fl< . 2e-8)  x]
          [(x . fl< . 0.00018)
           ;; Taylor series order 3
           (fl* x (fl+ 1.0 (fl* (fl* #i-1/6 x) x)))]
          [(x . fl< . 1.0)
           ;; Standard definition, rearranged to preserve digits
           (fllog1p (fl+ x (flsqrt1pm1 (fl* x x))))]
          [(x . fl< . 3e3)
           ;; Standard definition
           (fllog (fl+ x (flsqrt (fl+ (fl* x x) 1.0))))]
          [(x . fl< . 1e307)
           ;; Laurent series in 1/x at 0+ order from -1 to 1
           (fl+ (fllog (fl* x 2.0)) (fl/ 1.0 (fl* (fl* 4.0 x) x)))]
          [else
           ;; Laurent series, rearranged to not overflow
           (fl+ (fllog x) (fllog 2.0))]))
  ;; Inverse hyperbolic functions are all defined in math and based on fl...version so not much sense in testing both
  ;"asinh"
  ;(print-it (fl-tester (list flasinh_old flasinh) (b bfasinh 1) 1 #:names '(flasinh_rat flasinh*_rat) #:rational? #t #:print-limit '(#f 1)))
  ;(print-it (fl-tester (list flasinh_old flasinh) (b bfasinh 1) 1 #:names '(flasinh_all flasinh*_all) #:rational? #f #:print-limit '(#f 1)))
  "acosh";; 134/1e6 in 0.5~2
  (print-it (fl-tester (list flacosh) (b bfacosh 1) 1 #:names '(flacosh_rat) #:rational? #t #:print-limit 1))
  (print-it (fl-tester (list flacosh) (b bfacosh 1) 1 #:names '(flacosh_all) #:rational? #f #:print-limit 1))
  ;"atanh (using err>2)";; 3350/1e6 in 0.5~2
  ;(print-it (fl-tester (list atanh flatanh) (b bfatanh 1) 1 #:names '(atanh_rat flatanh_rat) #:rational? #t #:print-limit '(#f 2)))
  ;(print-it (fl-tester (list atanh flatanh) (b bfatanh 1) 1 #:names '(atanh_all flatanh_all) #:rational? #f #:print-limit '(#f 2)))
  )
