#lang typed/racket/base

(require math/distributions
         typed/rackunit)

(define T (student-t-dist 2))
(check-=? (pdf T 1)
          (/ 1 (* 3 (sqrt 3)))
          (expt 2 -54))
(check-=? (cdf T 1)
          (+ 1/2 (/ 1 (* 2 (sqrt 3))))
          (expt 2 -54))
(check-=? (inv-cdf T (+ 1/2 (/ 1 (* 2 (sqrt 3)))))
          1
          (expt 2 -51))