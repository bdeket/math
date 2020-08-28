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

(define (fc-tester fcts-to-test
                   fct-for-check
                   argnr
                   #:samples [samples 1000]
                   #:levels [levels (list 2 5 10 100 1000)]
                   #:names [names (map ~a fcts-to-test)]
                   #:print-limit [print-limit #f])
  (define LS (sort (remove-duplicates (cons 0 (cons +inf.0 levels))) <))
  (define len (length fcts-to-test))
  (for/fold ([Hs (make-list len #hash())]
             #:result (map cons names Hs))
            ([_ (in-range samples)])
    (define zs (build-list argnr (λ (_)(random-float-complex))))
    (define b (apply fct-for-check zs))
    (define as (map (λ (fct) (apply fct zs)) fcts-to-test))
    (define es (map (λ (z)
                      (define e (fculp-error z b))
                      (if (flnan? e) +inf.0 e))
                    as))
    (for/list ([H (in-list Hs)]
               [e (in-list es)])
      
      (when (and print-limit (<= print-limit e)) (println (list zs b (map cons as es))))
      
      (for/first ([l- (in-list LS)]
                  [l+ (in-list (cdr LS))]
                  #:when (<= l- e l+))
        (hash-update H (cons l- l+) add1 0)))))

;; random flonum,
;; favor   0 (2%)
;;       nan (2%)
;;       inf (4%)
;; subnormal (6%)
(define (random-flonum)
  (case (random 50)
    [(0) (* (if (= (random 2) 0) 1 -1) 0.0)]
    [(1) +nan.0]
    [(2) +inf.0]
    [(3) -inf.0]
    [(4 5 6)
     (* (if (= (random 2) 0) 1 -1)
        (floating-point-bytes->real (integer->integer-bytes (random-bits 52) 8 #f #f) #f))]
    [else
     (floating-point-bytes->real (integer->integer-bytes (random-bits 64) 8 #f #f) #f)]))

;; random float-complex
;; favor (10%) where real and imag part are in the same magnitude
(define (random-float-complex)
  (case (random 10)
    [(0) (make-polar (random-flonum) (random-flonum))];± 10% where real and imag part have similar magnitude
    [else (make-rectangular (random-flonum)(random-flonum))]))

(module+ test
  (define N 1e3)
  "addition"
  (fc-tester (list + fc+) (b bc+ 2) 2 #:samples N)
  "additionx4"
  (fc-tester (list + fc+) (b bc+ 4) 4 #:samples N)
  
  "subtraction"
  (fc-tester (list - fc-) (b bc- 2) 2 #:samples N)
  "subtractionx4"
  (fc-tester (list - fc-) (b bc- 4) 4 #:samples N)
  
  "multiplication"
  (fc-tester (list * fc*) (b bc* 2 nonan) 2 #:samples N)
  "multiplicationx3"
  (fc-tester (list * fc* (λ zs (fcprod_alt zs))) (b bc* 3 nonan) 3 #:samples N)
  "multiplicationx4"
  (fc-tester (list * fc* (λ zs (fcprod_alt zs))) (b bc* 4 nonan) 4 #:samples N)
  "multiplicationx5"
  (fc-tester (list * fc* (λ zs (fcprod_alt zs))) (b bc* 5 nonan) 5 #:samples N)
  
  "division"
  (fc-tester (list / fc/) (b bc/ 2 nonan) 2 #:samples N)
  
  "inversion"
  (fc-tester (list / fc/) (b bc/ 1) 1 #:samples N)

  "goniometric"
  (fc-tester (list sin fcsin) (b bcsin 1) 1 #:samples N)
  ;(fc-tester (list cos fccos) (b bccos 1) 1 #:samples N)
  (fc-tester (list tan fctan) (b bctan 1) 1 #:samples N)
  )