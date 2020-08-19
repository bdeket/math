#lang typed/racket/base

(require (only-in "mpc.rkt" 1ary-funs 1ary-preds 1ary-bf-funs 1ary2-funs 2ary-funs)
         (only-in "../bigfloat/bigfloat-mpfr.rkt" Bigfloat bf)
         "../base/base-random.rkt"
         "../bigfloat/utils.rkt")

(define-type Rounding-Mode (U 'nearest 'zero 'up 'down))
(define-type Quadrant (U 0 'q++ 'q-- 'q+- 'q-+ 'q0+ 'q0- 'q+0 'q-0))

(require/typed
 "mpc.rkt"
 ;; Parameters
 [bc-rounding-mode  (Parameterof (U Rounding-Mode (Pair Rounding-Mode Rounding-Mode)))]
 [bc-min-precision  Exact-Positive-Integer]
 [bc-max-precision  Exact-Positive-Integer]
 [bc-precision  (Parameterof Integer)]
 ;; Type and predicate
 [opaque Bigcomplex bigcomplex?]
 ;; Accessors
 [bigcomplex-precision    (Bigcomplex -> (Pair Exact-Positive-Integer Exact-Positive-Integer))]
 ;; Conversions from Bigcomplex
 [bigcomplex->float-complex    (Bigcomplex -> Float-Complex)]
 [bigcomplex->integer-complex  (Bigcomplex -> Exact-Number)]
 [bigcomplex->exact-number     (Bigcomplex -> Exact-Number)]
 [bigcomplex->number           (Bigcomplex -> Number)]
 [bigcomplex->string           (Bigcomplex -> String)]
 ;; Conversions to Bigfloat
 [flonum->bigcomplex    (->* (Flonum) ((Option Flonum)) Bigcomplex)]
 [integer->bigcomplex   (->* (Integer) ((Option Integer)) Bigcomplex)]
 [number->bigcomplex    (Number -> Bigcomplex)]
 [bigfloat->bigcomplex  (->* (Bigfloat) ((Option Bigfloat)) Bigcomplex)]
 [string->bigcomplex    (String -> (U #f Bigcomplex))]
 ;; Main constructor
 [bc  (case-> ((U String Number) -> Bigcomplex)
              ((U Real Bigfloat) (U Real Bigfloat) -> Bigcomplex))]
 [bcmake-rectangular ((U Real Bigfloat) (U Real Bigfloat) -> Bigcomplex)]
 [bcmake-polar ((U Real Bigfloat) (U Real Bigfloat) -> Bigcomplex)]
 ;; Functions that will only be provided wrapped
 [bcadd  (Bigcomplex Bigcomplex -> Bigcomplex)]
 [bcsub  (Bigcomplex Bigcomplex -> Bigcomplex)]
 [bcmul  (Bigcomplex Bigcomplex -> Bigcomplex)]
 [bcdiv  (Bigcomplex Bigcomplex -> Bigcomplex)]
 [bcneg  (Bigcomplex -> Bigcomplex)]
 [bcsum  ((Listof Bigcomplex) -> Bigcomplex)]
 [bc=?   (Bigcomplex Bigcomplex -> Boolean)]
 [bcmagnitude=?   (Bigcomplex Bigcomplex -> Boolean)]
 [bcmagnitude<?   (Bigcomplex Bigcomplex -> Boolean)]
 [bcmagnitude<=?  (Bigcomplex Bigcomplex -> Boolean)]
 [bcmagnitude>?   (Bigcomplex Bigcomplex -> Boolean)]
 [bcmagnitude>=?  (Bigcomplex Bigcomplex -> Boolean)]
 [bcmuladd  (Bigcomplex Bigcomplex Bigcomplex -> Bigcomplex)]
 ;; Functions with non-uniform types
 [bcexp-2pii  (Exact-Rational -> Bigcomplex)]
 [bcquadrant (->* (Bigcomplex) (Bigcomplex) Quadrant)]
 [bcin-quadrant? (->* ((U Quadrant (Listof Quadrant)) Bigcomplex) (Bigcomplex) Boolean)]
 [quadrant-A Quadrant]
 [quadrant-B Quadrant]
 [quadrant-C Quadrant]
 [quadrant-D Quadrant]
 [quadrant-R+ Quadrant]
 [quadrant-R- Quadrant]
 [quadrant-I+ Quadrant]
 [quadrant-I- Quadrant]
 [quadrant-R (Listof Quadrant)]
 [quadrant-I (Listof Quadrant)]
 [quadrant-A+ (Listof Quadrant)]
 [quadrant-B+ (Listof Quadrant)]
 [quadrant-C+ (Listof Quadrant)]
 [quadrant-D+ (Listof Quadrant)]
 [quadrant-R<0 (Listof Quadrant)]
 [quadrant-R<=0 (Listof Quadrant)]
 [quadrant-0<R (Listof Quadrant)]
 [quadrant-0<=R (Listof Quadrant)]
 [quadrant-I<0 (Listof Quadrant)]
 [quadrant-I<=0 (Listof Quadrant)]
 [quadrant-0<I (Listof Quadrant)]
 [quadrant-0<=I (Listof Quadrant)]
 ;[bcroot  (Bigfloat Integer -> Bigfloat)]
 ;[bcshift  (Bigfloat Integer -> Bigfloat)]
 ;[bflog-gamma/sign  (Bigfloat -> (Values Bigfloat (U -1 1)))]
 )

(req/prov-uniform-collection "mpc.rkt" 1ary-funs  (Bigcomplex -> Bigcomplex))
(req/prov-uniform-collection "mpc.rkt" 1ary-bf-funs  (Bigcomplex -> Bigfloat))
(req/prov-uniform-collection "mpc.rkt" 1ary-preds (Bigcomplex -> Boolean))
(req/prov-uniform-collection "mpc.rkt" 1ary2-funs (Bigcomplex -> (Values Bigcomplex Bigcomplex)))
(req/prov-uniform-collection "mpc.rkt" 2ary-funs  (Bigcomplex Bigcomplex -> Bigcomplex))

;; Rackety wrappers

(: bc+ (Bigcomplex * -> Bigcomplex))
(define (bc+ . xs)
  (cond [(null? xs)  (bc 0)]
        [else
         (define xs1 (cdr xs))
         (cond [(null? xs1)  (car xs)]
               [else
                (define xs2 (cdr xs1))
                (cond [(null? xs2)  (bcadd (car xs) (car xs1))]
                      [else  (bcsum xs)])])]))

(: bc- (Bigcomplex Bigcomplex * -> Bigcomplex))
(define (bc- x . xs)
  (cond [(null? xs)  (bcneg x)]
        [(null? (cdr xs))  (bcsub x (car xs))]
        [else  (bcneg (apply bc+ (bcneg x) xs))]))

(: bc* (Bigcomplex * -> Bigcomplex))
(define (bc* . xs)
  (cond [(null? xs)  (bc 1)]
        [else  (let loop ([x  (car xs)] [xs  (cdr xs)])
                 (cond [(null? xs)  x]
                       [else  (loop (bcmul x (car xs)) (cdr xs))]))]))

(: bc/ (Bigcomplex Bigcomplex * -> Bigcomplex))
(define (bc/ x . xs)
  (cond [(null? xs)  (bcdiv (bc 1) x)]
        [else  (bcdiv x (apply bc* xs))]))

(: fold-binary-pred (All (A) ((A A -> Boolean) A (Listof A) -> Boolean)))
(define (fold-binary-pred pred? x xs)
  (let loop ([x x] [xs xs])
    (cond [(null? xs)  #t]
          [else  (define fst-xs (car xs))
                 (cond [(pred? x fst-xs)  (loop fst-xs (cdr xs))]
                       [else  #f])])))

(define-syntax-rule (define-nary-pred bfpred? bfpred2?)
  (begin
    (: bfpred? (Bigcomplex Bigcomplex * -> Boolean))
    (define (bfpred? x . xs) (fold-binary-pred bfpred2? x xs))))

(define-nary-pred bc=  bc=?)
(define-nary-pred bcmagnitude=  bcmagnitude=?)
(define-nary-pred bcmagnitude<  bcmagnitude<?)
(define-nary-pred bcmagnitude<= bcmagnitude<=?)
(define-nary-pred bcmagnitude>  bcmagnitude>?)
(define-nary-pred bcmagnitude>= bcmagnitude>=?)

(: bcrandom (-> Bigcomplex))
(define (bcrandom)
  (define bits (bc-precision))
  (bc (bf (random-bits bits) (- bits))
      (bf (random-bits bits) (- bits))))

#|TODO
(: bigfloat->fl2 (Bigfloat -> (Values Flonum Flonum)))
(define (bigfloat->fl2 x)
  (define x2 (bigfloat->flonum x))
  (cond [(rational? x2)
         (let ([x2  (+ x2 (bigfloat->flonum (bf- x (flonum->bigfloat x2))))])
           (cond [(rational? x2)
                  (values x2 (bigfloat->flonum (bf- x (flonum->bigfloat x2))))]
                 [else
                  (values x2 0.0)]))]
        [else  (values x2 0.0)]))

(: fl2->bigfloat (Flonum Flonum -> Bigfloat))
(define (fl2->bigfloat x2 x1)
  (cond [(fl= x1 0.0)  (bf x2)]
        [else  (bf+ (flonum->bigfloat x1) (flonum->bigfloat x2))]))
|#

(provide
 ;; Parameters
 bc-rounding-mode
 bc-min-precision
 bc-max-precision
 bc-precision
 ;; Type and predicate
 Bigcomplex bigcomplex?
 ;; Accessors
 bigcomplex-precision
 bcreal-part
 bcimag-part
 ;; Conversions
 flonum->bigcomplex
 integer->bigcomplex
 number->bigcomplex
 bigfloat->bigcomplex
 string->bigcomplex
 bigcomplex->float-complex
 bigcomplex->integer-complex
 bigcomplex->exact-number
 bigcomplex->number
 bigcomplex->string
 ;; Main constructor
 bc
 bcmake-rectangular
 bcmake-polar
 ; Functions with non-uniform types
 bcrandom
 bcexp-2pii
 ;; Function wrappers with Rackety APIs
 bc+
 bc-
 bc*
 bc/
 bc=
 bcsum
 bcmagnitude=
 bcmagnitude<
 bcmagnitude<=
 bcmagnitude>
 bcmagnitude>=)
