#lang typed/racket/base

(require "bigcomplex-struct.rkt"
         "../bigfloat/bigfloat-struct.rkt")

(provide bclog1p
         bclog* bclog/ bclog+ bclog-
         bclog1- #;bclogb)

(: bclog1p (Bigcomplex -> Bigcomplex))
(define (bclog1p z)
  (define m (bcmagnitude z))
  (define n (bfnext m))
  (define d (bfexpt (bf- n m) 4.bf))
  (define p (bc-precision))
  (println d)
  (define -z (bc- z))
  (if (bf< m (bf 1e-3))
      ;; Lazy, this should be done better!
      (bcsum
       (reverse
        (parameterize ([bc-precision (* 2 p)])
          (for/fold ([s : (Listof Bigcomplex) (list z)]
                  [z^i : Bigcomplex (bc* z -z)]
                  [t : Bigfloat m]
                  #:result s)
                 ([i (in-naturals 2)]
                  #:break (bf< t d))
         (values (cons (bc/ z^i (bc i)) s)
                 (bc* z^i -z)
                 (bf* t m))))))
      (bclog (bc+ 1.bc z))))

(: bcexpm1 (Bigcomplex -> Bigcomplex))
(define (bcexpm1 z)
  (define m (bcmagnitude z))
  (define n (bfnext m))
  (define d (bfexpt (bf- n m) 4.bf))
  (define p (bc-precision))
  (println d)
  (if (bf< m (bf 1e-3))
      ;; Probably this should be done better!
      (bccopy
       (parameterize ([bc-precision (* 2 p)])
         (bcsum
          (reverse
           (for/fold ([s : (Listof Bigcomplex) (list z)]
                      [z^i : Bigcomplex (bc* z z)]
                      [t : Bigfloat m]
                      [n : Integer 2]
                      #:result s)
                     ([i (in-naturals 3)]
                      #:break (bf< t d))
             (values (cons (bc/ z^i (bc n)) s)
                     (bc* z^i z)
                     (bf* t m)
                     (* n i)))))))
      (bc- (bcexp z) 1.bc)))

(: bclog* (Bigcomplex Bigcomplex -> Bigcomplex))
(define (bclog* log-x log-y) (bc+ log-x log-y))

(: bclog/ (Bigcomplex Bigcomplex -> Bigcomplex))
(define (bclog/ log-x log-y) (bc- log-x log-y))

(: bclog+ (Bigcomplex Bigcomplex -> Bigcomplex))
(define (bclog+ log-x log-y)
  (let-values ([(log-x log-y)  (if (log-x . bcmagnitude> . log-y)
                                   (values log-x log-y)
                                   (values log-y log-x))])
    (bc+ log-x (bclog1p (bcexp (bc- log-y log-x))))))

(: bclog- (Bigfloat Bigfloat -> Bigfloat))
(define (bclog- log-x log-y)
  (cond [(log-y . bf> . log-x)  +nan.bf]
        [else  (bf+ log-x (bflog1p (bf- (bfexp (bf- log-y log-x)))))]))

(: bclog1- (Bigcomplex -> Bigcomplex))
(define (bclog1- log-x)
  (cond [(log-x . bcmagnitude> . (bclog (bc 0.5)))  (bclog (bc- (bcexpm1 log-x)))]
        [else  (bclog1p (bc- (bcexp log-x)))]))

(: bclogb (Bigcomplex Bigcomplex -> Bigcomplex))
(define (bclogb b x)
  (cond [(bc= x 1.bc)  0.bc]
        [(bc= b 1.bc)  +nan.bc]
        #;[(not (and (bf<= 0.bf b) (bf<= b +inf.bf) (bf<= 0.bf x) (bf<= x +inf.bf)))  +nan.bf]
        #;[(bc= b 0.bc)
         (cond [(bc= x 0.bc)  (bc +inf.bf 0.bf)]
               [(bf= x +inf.bf)  -inf.bf]
               [(bf<= x 1.bf)  0.bf]
               [else  -0.bf])]
        #;[(bf= b +inf.bf)
         (cond [(bf= x 0.bf)  -inf.bf]
               [(bf= x +inf.bf)  +inf.bf]
               [(bf<= 1.bf x)  0.bf]
               [else  -0.bf])]
        #;[(bf= x 0.bf)  (if (bf< b 1.bf) +inf.bf -inf.bf)]
        #;[(bf= x +inf.bf)  (if (bf< b 1.bf) -inf.bf +inf.bf)]
        [else
         (bc/ (bclog x) (bclog b))]))

;#|
(define e (bc "1e-600"))
(bclog (bc+ 1.bc e))
(bclog1p e)

(define E (bc "1e-5+1e-10i"))
(bclog1p E)
(bclog (bc+ 1.bc E))
(bc "9.99995000033333583325333466664809545059334921534931e-6+9.99990000099999000006666666665666689999620004999946e-11i");<=wolfram value
(bcexpm1 (bclog1p E))
(bc- (bcexp (bclog (bc+ 1.bc E))) 1.bc)
;|#