#lang typed/racket/base

(require "bigcomplex-struct.rkt"
         "../bigfloat/bigfloat-struct.rkt"
         (only-in "../../number-theory.rkt" bernoulli-number)
         (only-in racket/list take))

(provide bcgamma)

(: bcgamma (Bigcomplex -> Bigcomplex))
(define (bcgamma z)
  (define (inner [z : Bigcomplex]) : Bigcomplex
    (cond
      [(bf< (bcreal-part z) 0.bf)
       (bc/ pi.bc
            (bc* (bcsin (bc* pi.bc z))
                 (inner (bc- 1.bc z))))]
      [else
       (bcexp (stirling z))]))
  
  (bccopy
   (parameterize ([bc-precision (* 2 (bc-precision))])
     (inner z))))

;; calculated in logform
(define (stirling [z : Bigcomplex]) : Bigcomplex
  (define m (bcmagnitude z))
  (define p (bc-precision))
  (define r (ceiling (* 0.2 p)))
;(println (list r p))

  (define (inner [z : Bigcomplex]) : Bigcomplex
    (define z² (bc* z z))
    (define base
      (bc+ (bc- (bc* (bc- z (bc 1/2)) (bclog z))
                z)
           (bc* (bc 1/2) (bclog (bc* (bc 2) pi.bc)))))
    (bc+ base
         (for/fold ([s : (Listof Bigcomplex) '()]
                    [z^ : Bigcomplex z]
                    [ctr : Integer 0]
                    #:result (bcsum s))
                   ([k : Integer (in-range 1 p)]
                    #:break (< 10 ctr))
           (define i (* 2 k))
           (define δ (bc/ (bc (bernoulli-number i))
                          (bc* (bc (- i 1)) (bc i) z^)))
;(println (list k (bcsum s)(foldr bc+ 0.bc s)))
           (values (cons δ s)
                   (bc* z^ z²)
                   (if (bc= base (bc+ base δ (bcsum (take s ctr)))) (+ ctr 1) 0)))))

  (cond
    [(bf< m (bf r))
     (define z+r (bc+ z (bc r)))
     (define lg (inner z+r))
     (bccopy
      (parameterize ([bc-precision (+ p 20)])
        (bc- lg
             (for/fold ([s : Bigcomplex 0.bc]
                        [z+ : Bigcomplex z]
                        #:result s)
                       ([i (in-range r)])
               (values (bc+ s (bclog z+))
                       (bc+ z+ 1.bc))))))]
    [else (inner z)]))
