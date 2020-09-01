#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/format
         racket/list)
(require "../../../base.rkt"
         "../../../flonum.rkt"
         "../../../bigcomplex.rkt")
(require "../flocom-base.rkt"
         "../flocom-goniometric.rkt")

(define-syntax (b stx)
  (syntax-case stx (nonan)
    [(b bcf argnr nonan)
     (with-syntax ([(args ...)(build-list (syntax-e #'argnr) (λ (i)(format-id stx "z~a" i)))])
       #'(λ (args ...)
           (if (ormap fcnan? (list args ...))
               +nan.0+nan.0i
               (bigcomplex->float-complex (bcf (bc args) ...)))))]
    [(b bcf argnr)
     (with-syntax ([(args ...)(build-list (syntax-e #'argnr) (λ (i)(format-id stx "z~a" i)))])
       #'(λ (args ...)
           (bigcomplex->float-complex (bcf (bc args) ...))))]))

(define SAMPLES (make-parameter 1000))
(define LEVELS (make-parameter (list 2 5 10 100 100 1000)))
(define RATIONAL? (make-parameter #f))
(define PRINTLIMIT (make-parameter #f))

(define (fc-tester fcts-to-test
                   fct-for-check
                   argnr
                   #:samples [samples (SAMPLES)]
                   #:levels [levels (LEVELS)]
                   #:names [names (map ~a fcts-to-test)]
                   #:rational? [r? (RATIONAL?)]
                   #:print-limit [print-limit (PRINTLIMIT)])
  (define LS (sort (remove-duplicates (cons 0 (cons +inf.0 levels))) <))
  (define len (length fcts-to-test))
  (for/fold ([Hs (make-list len #hash())]
             #:result (map cons names Hs))
            ([_ (in-range samples)])
    (define zs (build-list argnr (λ (_)(random-float-complex #:rational? r?))))
    (define b (apply fct-for-check zs))
    (define as (map (λ (fct) (apply fct zs)) fcts-to-test))
    (define es (map (λ (z)
                      (define e (fculp-error z b))
                      (if (flnan? e) +inf.0 e))
                    as))
    (for/list ([H (in-list Hs)]
               [e (in-list es)])
      (when (and print-limit (<= print-limit e)) (println (list zs b (map cons as es))))
      
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

;; random float-complex
;; favor (10%) where real and imag part are in the same magnitude
(define (random-float-complex #:rational? [r? #f])
  (case (random 10)
    [(0) (make-polar (random-flonum #:rational? r?)
                     (* (random) 2 pi))];± 10% where real and imag part have similar magnitude
    [else (make-rectangular (random-flonum #:rational? r?)
                            (random-flonum #:rational? r?))]))

(define (print-a-line . rst)
  (displayln (apply string-append (add-between (map (λ (x)(~a x #:width 10 #:align 'right)) rst) " |"))))
(define (print-header)
  (print-a-line "name" "best" "0-2" "2-5" "5-10" "10-100" "100-1000" "1000-+inf"))
(define (print-it Hs)
  (for ([l (in-list Hs)])
    (define H (cdr l))
    (print-a-line (car l)
                  (hash-ref H 'best 0)
                  (hash-ref H '(0 . 2) 0)
                  (hash-ref H '(2 . 5) 0)
                  (hash-ref H '(5 . 10) 0)
                  (hash-ref H '(10 . 100) 0)
                  (hash-ref H '(100 . 1000) 0)
                  (hash-ref H '(1000 . +inf.0) 0))))

(module+ main
  (SAMPLES 1e3)
  (RATIONAL? #t)
  (print-header)
  )

#;(module+ main
  ;; all ok, fc+ slightly better on ×4
  "addition"
  (print-it (fc-tester (list + fc+) (b bc+ 2) 2 #:names '(+_rat fc+_rat) #:rational? #t))
  (print-it (fc-tester (list + fc+) (b bc+ 2) 2 #:names '(+_all fc+_all) #:rational? #f))
  "addition×4"
  (print-it (fc-tester (list + fc+) (b bc+ 4) 4 #:names '(+_rat fc+_rat) #:rational? #t))
  (print-it (fc-tester (list + fc+) (b bc+ 4) 4 #:names '(+_all fc+_all) #:rational? #f))
  )

#;(module+ main
  ;; all ok, fc+ slightly better on ×4
  "subtraction"
  (print-it (fc-tester (list - fc-) (b bc- 2) 2 #:names '(-_rat fc-_rat) #:rational? #t))
  (print-it (fc-tester (list - fc-) (b bc- 2) 2 #:names '(-_all fc-_all) #:rational? #f))
  "subtraction×4"
  (print-it (fc-tester (list - fc-) (b bc- 4) 4 #:names '(-_rat fc-_rat) #:rational? #t))
  (print-it (fc-tester (list - fc-) (b bc- 4) 4 #:names '(-_all fc-_all) #:rational? #f))
  )

#;(module+ main
  ;; fc not perfect, for some values (±3 per 100000)
  ;; For inf/nan values, bc* does strange things...
  (define (mybc* . nrs)
    (define a
      (cond
        [(empty? nrs) +1.0+0.0i]
        [(empty? (cdr nrs)) (car nrs)]
        [else
         (define z1 (car nrs))
         (define z2 (cadr nrs))
         (cond
           [(or (and (flinfinite? (real-part z1))(flinfinite? (imag-part z1)))
                (and (flinfinite? (real-part z2))(flinfinite? (imag-part z2)))
                (and (flinfinite? (real-part z1))(flinfinite? (real-part z2))))
            (apply mybc*
                   (make-rectangular (- (* (real-part z1)(real-part z2))
                                        (* (imag-part z1)(imag-part z2)))
                                     (+ (* (imag-part z1)(real-part z2))
                                        (* (real-part z1)(imag-part z2))))
                   (cddr nrs))]
           [else
            (apply mybc* ((b bc* 2 nonan) z1 z2) (cddr nrs))])]))
    (if (fcnan? a)
        (fcprod nrs)
        a))

  "multiplication"
  (print-it (fc-tester (list * fc*) mybc* 2 #:names '(*_rat fc*_rat) #:rational? #t))
  (print-it (fc-tester (list * fc*) mybc* 2 #:names '(*_all fc*_all) #:rational? #f))
  "multiplication×3"
  (print-it (fc-tester (list * fc*) mybc* 3 #:names '(*_rat fc*_rat) #:rational? #t))
  (print-it (fc-tester (list * fc*) mybc* 3 #:names '(*_all fc*_all) #:rational? #f))
  "multiplication×4"
  (print-it (fc-tester (list * fc*) mybc* 4 #:names '(*_rat fc*_rat) #:rational? #t))
  (print-it (fc-tester (list * fc*) mybc* 4 #:names '(*_all fc*_all) #:rational? #f))
  "multiplication×5"
  (print-it (fc-tester (list * fc*) mybc* 5 #:names '(*_rat fc*_rat) #:rational? #t))
  (print-it (fc-tester (list * fc*) mybc* 5 #:names '(*_all fc*_all) #:rational? #f))
  )

#;(module+ main
  ;; bc/ does strange things for inf and nan nominators
  ;; for the standard implementation 1/ is especially bad, 1+0i/ does a lot better
  (define (mybc/ z1 z2)
    (cond
      [(and (flinfinite? (real-part z1)) (flinfinite? (imag-part z1)))
       (fc/ z1 z2)]
      [(and (or (fcnan? z1) (fcinfinite? z1)) (or (= (real-part z2) 0) (= (imag-part z2) 0)))
       (fc/ z1 z2)]
      [else ((b bc/ 2 nonan) z1 z2)]))

  "division"
  (print-it (fc-tester (list / fc/) mybc/ 2 #:names '(/_rat fc/_rat) #:rational? #t))
  (print-it (fc-tester (list / fc/) mybc/ 2 #:names '(/_all fc/_all) #:rational? #f))
  
  "inversion"
  (print-it (fc-tester (list / (λ (z) (/ 1.+0.i z)) fc/) (λ (z) (mybc/ 1.0+0.0i z)) 1 #:names '(1/_rat 1+0i/_rat fc1/_rat) #:rational? #t))
  (print-it (fc-tester (list / (λ (z) (/ 1.+0.i z)) fc/) (λ (z) (mybc/ 1.0+0.0i z)) 1 #:names '(1/_all 1+0i/_all fc1/_all) #:rational? #f))
  )

(module+ main
  "exp"
  (print-it (fc-tester (list exp) (b bcexp 1) 1 #:names '(log_rat) #:rational? #t))
  (print-it (fc-tester (list exp) (b bcexp 1) 1 #:names '(log_all) #:rational? #f #:print-limit 1000))
  "log"
  (print-it (fc-tester (list log) (b bclog 1) 1 #:names '(log_rat) #:rational? #t #:print-limit 1000))
  (print-it (fc-tester (list log) (b bclog 1) 1 #:names '(log_all) #:rational? #f #:print-limit 1000))
  )

#;(module+ main
  "sin"
  (print-it (fc-tester (list sin fcsin) (b bcsin 1) 1 #:names '(sin_rat fcsin_rat) #:rational? #t))
  (print-it (fc-tester (list sin fcsin) (b bcsin 1) 1 #:names '(sin_all fcsin_all) #:rational? #f))
  "cos"
  (print-it (fc-tester (list cos fccos) (b bccos 1) 1 #:names '(cos_rat fccos_rat) #:rational? #t))
  (print-it (fc-tester (list cos fccos) (b bccos 1) 1 #:names '(cos_all fccos_all) #:rational? #f))
  "tan"
  (print-it (fc-tester (list tan fctan) (b bctan 1) 1 #:names '(tan_rat fctan_rat) #:rational? #t
                       ;#:print-limit 1000
                       ;WARNING bctan can be extremely slow!
                       ;because I probably did something stupid in the ffi,
                       ;this will also block all threads in drracket
                       ;#:samples 50
                       ))
  )

#;(module+ main
  "asin/acos/atan"
  (print-it (fc-tester (list asin) (b bcasin 1) 1 #:names '(asin) #:rational? #t))
  (print-it (fc-tester (list acos) (b bcacos 1) 1 #:names '(acos) #:rational? #t
                       #:samples 1000))
  (print-it (fc-tester (list atan) (b bcatan 1) 1 #:names '(atan) #:rational? #t
                       #:samples 1000))
  )

#;(module+ main
  "sinh"
  (print-it (fc-tester (list sinh fcsinh) (b bcsinh 1) 1 #:names '(sinh_rat fcsinh_rat) #:rational? #t))
  (print-it (fc-tester (list sinh fcsinh) (b bcsinh 1) 1 #:names '(sinh_all fcsinh_all) #:rational? #f))
  "cosh"
  (print-it (fc-tester (list cosh fccosh) (b bccosh 1) 1 #:names '(cosh_rat fccosh_rat) #:rational? #t))
  (print-it (fc-tester (list cosh fccosh) (b bccosh 1) 1 #:names '(cosh_all fccosh_all) #:rational? #f))
  "tanh"
  (print-it (fc-tester (list tanh fctanh) (b bctanh 1) 1 #:names '(tanh_rat fctanh_rat) #:rational? #t))
  (print-it (fc-tester (list tanh fctanh) (b bctanh 1) 1 #:names '(tanh_all fctanh_all) #:rational? #f))
  )

#;(module+ main
  "asinh/acosh/atanh"
  (print-it (fc-tester (list asinh) (b bcasinh 1) 1 #:names '(asinh) #:rational? #t))
  (print-it (fc-tester (list acosh) (b bcacosh 1) 1 #:names '(acosh) #:rational? #t))
  (print-it (fc-tester (list atanh) (b bcatanh 1) 1 #:names '(atanh) #:rational? #t))
  )