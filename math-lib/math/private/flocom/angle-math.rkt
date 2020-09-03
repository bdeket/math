#lang typed/racket/base

(require "flonum-helpers.rkt")
;;**************************************************************************************************
;; Constructor / accessor / conversion
;;**************************************************************************************************
(provide angle-mod angle+ angle- angle* angle/)

(define -pi (- pi))
(define 2pi (* 2 pi))

(define (angle-mod [a : Flonum]) : Flonum
  (cond
    [(fl<=* 0.0 a 2pi) a]
    [else
     (fl- a (fl* (fltruncate (/ a 2pi)) 2pi))]))

(define (angle~mod [a : Flonum]) : (Values Integer Flonum)
  (cond
    [(fl<=* 0.0 a 2pi) (values 0 a)]
    [else
     (define b (fltruncate (/ a 2pi)))
     (values (round (inexact->exact b))
             (fl- a (fl* b 2pi)))]))

;angle+
(: angle+ (case-> (-> Flonum)
                  (Flonum -> Flonum)
                  (Flonum Flonum -> Flonum)
                  (Flonum Flonum * -> Flonum)))
(define angle+
  (case-lambda [()       0.0]
               [(a)      (angle-mod a)]
               [(a b)    (angle-mod (fl+ (angle-mod a) (angle-mod b)))]
               [(a . bs) (foldr
                          (λ ([a : Flonum][b : Flonum])
                            (angle-mod (fl+ a (angle-mod b))))
                          (angle-mod a) bs)]))

;angle-
(: angle- (case-> (Flonum -> Flonum)
                  (Flonum Flonum -> Flonum)
                  (Flonum Flonum * -> Flonum)))
(define angle-
  (case-lambda [(a)      (fl± (angle-mod a))]
               [(a b)    (angle-mod (fl- (angle-mod a) (angle-mod b)))]
               [(a . bs) (foldr
                          (λ ([a : Flonum][b : Flonum])
                            (angle-mod (fl- a (angle-mod b))))
                          (angle-mod a) bs)]))

;angle*
(: angle* (case-> (-> Flonum)
                  (Flonum -> Flonum)
                  (Flonum Flonum -> Flonum)
                  (Flonum Flonum * -> Flonum)))
(define angle*
  (case-lambda [()       1.0]
               [(a)      (angle-mod a)]
               [(a b)
                (define a* (angle-mod a))
                (define b* (angle-mod b))
                (angle-mod (fl+ (fl+ (* a* b*) (* a* 2pi))
                                (fl+ (* b* 2pi) (* 2pi 2pi))))]
               [(a . bs) (foldr angle* a bs)]))

;angle/
(: angle/ (case-> (Flonum -> Flonum)
                  (Flonum Flonum -> Flonum)
                  (Flonum Flonum * -> Flonum)))
(define angle/
  (case-lambda [(a)      (angle-mod (fl/ 1.0 (angle-mod a)))]
               [(a b)    (angle-mod (fl/ (angle-mod a) (angle-mod b)))]
               [(a . bs) (foldr
                          (λ ([a : Flonum][b : Flonum])
                            (angle-mod (fl/ a (angle-mod b))))
                          (angle-mod a) bs)]))
