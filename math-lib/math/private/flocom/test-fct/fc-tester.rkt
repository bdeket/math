#lang racket/base

(require "../flocom-base.rkt"
         "../../../bigcomplex.rkt")

(define (fc-tester fct1
                   fct2
                   argnr
                   #:samples [samples 1000]
                   #:levels [levels (list 2 5 10 100)])
  (define LS (sort (remove-duplicates (cons 0 (cons +inf.0 levels))) <))
  (for/fold ([H #hash()])
            ([_ (in-range samples)])
    (define zs (build-list argnr (random-float-complex)))
    (define a (apply fct1 zs))
    (define b (apply fct2 zs))
    (define e (fculp-error a b))
    (for/first ([l- (in-list LS)]
                [l+ (in-list (cdr LS))]
                #:when (<= l- e l+))
      (hash-update H (cons l- l+) add1 0))))

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