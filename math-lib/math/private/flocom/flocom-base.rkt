#lang typed/racket/base

#|
Things I want
fc+ fc- fc* fc/ fcmagnitude fcangle fcsum
fc= fcquadrant fcmagnitude< fcmagnitude> fcmagnitude=< fcmagnitude=> fcmagnitude-min fcmagnitude-max
fcto-quadrant
fcreal? fcreven? fcrodd?
fcimag? fcieveni fciodd?

fchypot ...?

fcrational? fcinfinite? fcnan? fcinteger?

fcround fcfloor fcceiling fctruncate

fcsin fccos fctan fcasin fcacos fcatan
fccsc fcsec fccot ... ?
fcsinh fccosh fctanh fcasinh fcacosh fcatanh
fcsinpix fccospix fctanpix
fccscpix fcsecpix fccotpix

fclog fcexp fcsqrt
flexpt
fclog1p
fcexpm1
fcexpt1p
fcexpt+
fcexp2
fclog2
fclogb

make-fcexpt
fcsqrt1pm1
fclog1pmx
fcexpsqr
fcgauss
fcexp1p

fcrandom

veneer
------
fcfactorial fcbinomial fcpermutations fcmultinomial
fclogfactorial fclog-binomial fclog-permutations fclog-multinomial

double-double
-------------
fc2 ... speed will be bad...

maybe category:
---------------
fcsingle

fcvector ... ?

lg* lg/ lgprod lg+ lg- lgsum lg1+ lg1-

|#

(require (for-syntax racket/base)
         racket/list)
(require "../../flonum.rkt")

;;**************************************************************************************************
;; Flonum helpers
;;**************************************************************************************************
(define (fl± [a : Flonum]) : Flonum (fl* -1.0 a))
(define (fl*+* [a : Flonum][b : Flonum][c : Flonum][d : Flonum]) : Flonum
  (define ab (* a b))
  (define cd (* c d))
;(println (list ab cd))
  
  (define (normal [ab : Flonum][cd : Flonum])(+ ab cd))
  
  (define (abnormal [a : Flonum][b : Flonum][c : Flonum][d : Flonum][cd : Flonum])
    (if (fl< (flabs a)(flabs b))
        (abnormal0 b a c d cd)
        (abnormal0 a b c d cd)))

  (define (abnormal0 [a : Flonum][b : Flonum][c : Flonum][d : Flonum][cd : Flonum])
    (if (flinfinite? cd)
        (if (fl< (flabs c)(flabs d))
            (abnormal2 a b d c)
            (abnormal2 a b c d))
        (abnormal1 a b cd)))
  
  (define (abnormal1 [a : Flonum][b : Flonum][cd : Flonum]) : Flonum
    (fl* a (fl+ b (fl/ cd a))))
  
  (define (abnormal2 [a : Flonum][b : Flonum][c : Flonum][d : Flonum]) : Flonum
    (fl* a (fl+ b (fl* (fl/ c a) d))))
  
  (cond
    ;one is nan
    [(or (flnan? ab)(flnan? cd)) +nan.0]
    ;one is inf and the other points in the other direction (ie: +inf.0 -1)
    ;there is a chance that with reordering the result would not overflow
    [(flinfinite? ab)
     (if (fl< (* (flsgn ab) cd) 0.0)
         (abnormal a b c d cd)
         ab)]
    [(flinfinite? cd)
     (if (fl< (* (flsgn cd) ab) 0.0)
         (abnormal c d a b ab)
         cd)]
    ;underflow for both
    ;there is a chance that their sum might still be a measurable value
       ;[TODO]
    ;Normal path
    [else (normal ab cd)]))

(define (flprod [zs : (Listof Flonum)]) : Flonum
  (define-values (small big)(partition (λ ([z : Flonum])(fl< (flabs z) 1.0)) zs))
  (let loop ([small ((inst sort Flonum) small fl< #:key flabs)]
             [big ((inst sort Flonum) big fl> #:key flabs)]
             [z : Flonum 1.0])
;(println (list small big z))
    (cond
      [(and (empty? small)(empty? big)) z]
      [(empty? small) (foldr fl* z big)];fl* only works on 2 arguments
      [(empty? big) (foldr fl* z small)]
      [(fl< (flabs z) 1.0)
       (loop small (cdr big) (fl* (car big) z))]
      [else
       (loop (cdr small) big (fl* (car small) z))])))
(module+ showcase
  (flprod '(1e200 1e200 1e-200))
  (* 1e200 1e200 1e-200))

;;**************************************************************************************************
;; Constructor / accessor / conversion
;;**************************************************************************************************
(provide fc make-fcrectangular make-fcpolar fcreal-part fcimag-part)

(define (fc [z : Number]) : Float-Complex
  (make-flrectangular (fl (real-part z))(fl (imag-part z))))

(define-syntax make-fcrectangular (make-rename-transformer #'make-flrectangular))

(define (make-fcpolar [e : Flonum][α : Flonum]) : Float-Complex
  (make-rectangular (fl* e (flcos α))
                    (fl* e (flsin α))))

(define-syntax fcreal-part (make-rename-transformer #'flreal-part))
(define-syntax fcimag-part (make-rename-transformer #'flimag-part))

(define (fcmagnitude [z : Float-Complex]) (magnitude z))
(define (fcangle [z : Float-Complex]) (angle z))

#|
TODO
->integer ->exact 
|#

;;**************************************************************************************************
;; Basic arithmetic
;;**************************************************************************************************
(provide fcsum fc+)

;; addition
(define (fcsum [lst : (Listof Float-Complex)])
  (define-values (rs is)
    (for/fold ([rs : (Listof Flonum) '()]
               [is : (Listof Flonum) '()])
              ([z (in-list lst)])
      (values (cons (fcreal-part z) rs)
              (cons (fcimag-part z) is))))
  (make-fcrectangular (flsum rs)(flsum is)))

(define (fc+ . [z : Float-Complex *]) (fcsum z))

;; subtraction
(: fc- (case-> (Float-Complex -> Float-Complex)
               (Float-Complex Float-Complex * -> Float-Complex)))
(define fc-
  (case-lambda
    [(z) (make-fcrectangular (fl* -1.0 (fcreal-part z))
                             (fl* -1.0 (fcimag-part z)))]
    [(z1 . zs) (fc- (fcsum (cons (fc- z1) zs)))]))
(module+ showcase
  (fc- (make-fcrectangular +max.0 0.0)
       (make-fcrectangular epsilon.0 0.0)
       (make-fcrectangular +max.0 0.0))
  (- (make-fcrectangular +max.0 0.0)
     (make-fcrectangular epsilon.0 0.0)
     (make-fcrectangular +max.0 0.0)))


;; multiplication
(: fc* (case-> (-> 1.0+0.0i)
               (Float-Complex ->  Float-Complex)
               (Float-Complex Float-Complex -> Float-Complex)
               (Float-Complex * -> Float-Complex)))
(define fc*
  (case-lambda
    [() 1.0+0.0i]
    [([z : Float-Complex]) z]
    [([z1 : Float-Complex][z2 : Float-Complex])
     ;TODO handle cases where sub parts overflow
     (make-fcrectangular (fl*+* (fcreal-part z1)(fcreal-part z2)
                                (fl± (fcimag-part z1))(fcimag-part z2))
                         (fl*+* (fcreal-part z1)(fcimag-part z2)
                                (fcimag-part z1)(fcreal-part z2)))]
    ;; accumulating summing errors
    #;[ [zs : Float-Complex *]
      (define-values (small big)(partition (λ ([z : Float-Complex])(fl< (fcmagnitude z) 1.0)) zs))
      (let loop ([small ((inst sort Float-Complex) small fl< #:key fcmagnitude)]
                 [big ((inst sort Float-Complex) big fl> #:key fcmagnitude)]
                 [z : Float-Complex 1.0+0.0i])
        ;(println (list small big z))
        (cond
          [(and (empty? small)(empty? big)) z]
          [(empty? small) (foldr fc* z big)];fl* only works on 2 arguments
          [(empty? big) (foldr fc* z small)]
          [(fl< (fcmagnitude z) 1.0)
           (loop small (cdr big) (fc* (car big) z))]
          [else
           (loop (cdr small) big (fc* (car small) z))]))]
    ;; big chance of over/underflow
    [ [zs : Float-Complex *]
      (define len (length zs))
      (for/fold ([rs : (Listof Flonum) '()]
                 [is : (Listof Flonum) '()]
                 #:result (make-fcrectangular (flsum rs)
                                              (flsum is)))
                ([i (in-range (expt 2 len))])
(println (list i rs is))
        (define-values (r p)
          (for/fold ([r : Integer 0]
                     [p : (Listof Flonum) '()])
                    ([z (in-list zs)]
                     [j (in-naturals)])
            (if (bitwise-bit-set? i j)
                (values (+ r 1) (cons (fcimag-part z) p))
                (values r (cons (fcreal-part z) p)))))
        (case (modulo r 4)
          [(0)(values (cons (flprod p) rs) is)]
          [(2)(values (cons (flprod (cons -1.0 p)) rs) is)]
          [(1)(values rs (cons (flprod p) is))]
          [else (values rs (cons (flprod p) is))]))]))
(module+ showcase
  (fc* 1e155-1e154i .2e154-1e154i)
  (* 1e155-1e154i .2e154-1e154i))

;;**************************************************************************************************
;; Error checking
;;**************************************************************************************************
(provide fculp-error fcrulp-error fciulp-error)

(define (fculp-error [z1 : Float-Complex][z2 : Number]) : Flonum
  (flhypot (fcrulp-error z1 z2)
           (fciulp-error z1 z2)))

(define (fcrulp-error [z1 : Float-Complex][z2 : Number]) : Flonum
  (flulp-error (real-part z1)
               (real-part z2)))
(define (fciulp-error [z1 : Float-Complex][z2 : Number]) : Flonum
  (flulp-error (imag-part z1)
               (imag-part z2)))

#|
TODO
... bitfield, step etc...?
|#
