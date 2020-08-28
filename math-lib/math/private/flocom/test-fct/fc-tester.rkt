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

(module+ test
  (SAMPLES 1e3)
  (RATIONAL? #f)
  
  (define r? #f)
  "addition"
  (fc-tester (list + fc+) (b bc+ 2) 2)
  "additionx4"
  (fc-tester (list + fc+) (b bc+ 4) 4)
  
  "subtraction"
  (fc-tester (list - fc-) (b bc- 2) 2)
  "subtractionx4"
  (fc-tester (list - fc-) (b bc- 4) 4)
  
  "multiplication"
  (fc-tester (list * fc*) (b bc* 2 nonan) 2)
  "multiplicationx3"
  (fc-tester (list * fc* (λ zs (fcprod_alt zs))) (b bc* 3 nonan) 3)
  "multiplicationx4"
  (fc-tester (list * fc* (λ zs (fcprod_alt zs))) (b bc* 4 nonan) 4)
  "multiplicationx5"
  (fc-tester (list * fc* (λ zs (fcprod_alt zs))) (b bc* 5 nonan) 5)
  
  "division"
  (fc-tester (list / fc/) (b bc/ 2 nonan) 2)
  
  "inversion"
  (fc-tester (list / (λ (z) (/ 1.+0.i z)) fc/) (b bc/ 1) 1)

  "goniometric"
  (fc-tester (list sin fcsin) (b bcsin 1) 1)
  (fc-tester (list cos fccos) (b bccos 1) 1)
  (fc-tester (list tan fctan) (b bctan 1) 1 #:print-limit 1000
             ;WARNING bctan can be extremely slow!
             ;because I probably did something stupid in the ffi,
             ;this will also block all threads in drracket
             #:samples 10)

  (fc-tester (list asin) (b bcasin 1) 1)
  )