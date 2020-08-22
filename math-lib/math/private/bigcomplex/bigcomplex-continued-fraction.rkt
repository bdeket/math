#lang typed/racket/base

(require "bigcomplex-struct.rkt"
         "../bigfloat/bigfloat-struct.rkt")

(provide bccontinued-fraction)

(define-syntax-rule (bccontinued-fraction a-init a-fun b-init b-fun eps-expr)
  (bccopy
   (parameterize ([bc-precision  (+ (bc-precision) 20)])
     (let: ([eps : Bigfloat  eps-expr])
       (let: loop : Bigcomplex ([i : Bigfloat 0.bf]
                                [a : Bigcomplex a-init]
                                [b : Bigcomplex b-init]
                                [last-n : Bigcomplex 1.bc]
                                [last-d : Bigcomplex 0.bc]
                                [n : Bigcomplex 0.bc]
                                [d : Bigcomplex 1.bc]
                                [x : Bigcomplex 0.bc])
;(printf "a = ~v  b = ~v~n" a b)
         (define next-n (bc+ (bc* a last-n) (bc* b n)))
         (define next-d (bc+ (bc* a last-d) (bc* b d)))
         (define next-x (bc/ next-n next-d))
;(printf "n = ~v  d = ~v  x = ~v~n" next-n next-d next-x)
         (cond [((bcmagnitude (bc- x next-x)) . bf<= . (bf* eps (bcmagnitude next-x)))
;(printf "i = ~v~n" i)
                ;i
                next-x]
               [else
                (let ([i  (bf+ i 1.bf)])
                  (loop i (a-fun i a) (b-fun i b) n d next-n next-d next-x))]))))))
