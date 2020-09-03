#lang typed/racket/base

#|
Things I want
fcquadrant fcmagnitude-min fcmagnitude-max
fcto-quadrant

fchypot ...?

fcround fcfloor fcceiling fctruncate

fcsqrt
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
(require "flonum-helpers.rkt")
(module+ test
  (require typed/rackunit))

;;**************************************************************************************************
;; Constructor / accessor / conversion
;;**************************************************************************************************
(provide fc make-fcrectangular make-fcpolar
         fcreal-part fcimag-part
         fcmagnitude fcangle
         fcconjugate)

(define (fc [z : Number]) : Float-Complex
  (make-flrectangular (fl (real-part z))(fl (imag-part z))))

(: make-fcrectangular (Flonum Flonum -> Float-Complex))
(define make-fcrectangular make-flrectangular)

(define (make-fcpolar [e : Flonum][α : Flonum]) : Float-Complex
  (make-rectangular (fl* e (flcos α))
                    (fl* e (flsin α))))

(: fcreal-part (Float-Complex -> Flonum))
(define fcreal-part flreal-part)
(: fcimag-part (Float-Complex -> Flonum))
(define fcimag-part flimag-part)

(define fcmagnitude (ann magnitude (Float-Complex -> Flonum)))
(define (fcangle [z : Float-Complex]) : Flonum
  (if (and (flinfinite? (fcimag-part z))
           (flinfinite? (fcimag-part z)))
      +nan.0
      (angle z)))

(define (fcconjugate [z : Float-Complex]) : Float-Complex
  (make-fcrectangular (fcreal-part z)
                      (fl± (fcimag-part z))))

#|
TODO
->exact-integer ->exact 
|#

;;**************************************************************************************************
;; Predicates
;;**************************************************************************************************
(provide fcreal? fcimag?
         fcreven? fcrodd? fcieven? fciodd?
         fcrational-parts? fcinteger-parts?
         fczero? fcinfinite? fcnan?
         fcrational? fcinteger?)

(define (fcreal?  [z : Float-Complex]) : Boolean (flzero? (fcimag-part z)))
(define (fcimag?  [z : Float-Complex]) : Boolean (flzero? (fcreal-part z)))

(define (fcreven? [z : Float-Complex]) : Boolean (fleven? (fcreal-part z)))
(define (fcrodd?  [z : Float-Complex]) : Boolean (flodd?  (fcreal-part z)))
(define (fcieven? [z : Float-Complex]) : Boolean (fleven? (fcimag-part z)))
(define (fciodd?  [z : Float-Complex]) : Boolean (flodd?  (fcimag-part z)))

(define-syntax-rule (define-test-both name and/or test)
  (define (name [z : Float-Complex]) : Boolean
    (and/or (test (fcreal-part z))(test (fcimag-part z)))))

(define-test-both fcrational-parts? and flrational?)
(define-test-both fcinteger-parts?  and flinteger?)
(define-test-both fczero?     and flzero?)
(define-test-both fcinfinite? or  flinfinite?)
(define-test-both fcnan?      or  flnan?)
(define (fcrational? [z : Float-Complex])
  (and (flrational? (flreal-part z))(flzero? (flimag-part z))))
(define (fcinteger? [z : Float-Complex])
  (and (flinteger? (flreal-part z))(flzero? (flimag-part z))))

;;**************************************************************************************************
;; Comparisons
;;**************************************************************************************************
(provide fc=)

(define fc= (ann = (Float-Complex Float-Complex Float-Complex * -> Boolean)))

(define-syntax-rule (define-~>fl-check name ~>fl test)
  (begin
    (define (name [z0 : Float-Complex] . [zs : Float-Complex *]) : Boolean
      (let loop ([f0 : Flonum (~>fl z0)]
                 [zs : (Listof Float-Complex) zs])
        (cond
          [(empty? zs) #t]
          [else
           (define f1 (~>fl (car zs)))
           (if (test f0 f1)
               (loop f1 (cdr zs))
               #f)])))
    (provide name)))
(define-~>fl-check fcmagnitude=  fcmagnitude fl=)
(define-~>fl-check fcmagnitude<  fcmagnitude fl<)
(define-~>fl-check fcmagnitude<= fcmagnitude fl<=)
(define-~>fl-check fcmagnitude>  fcmagnitude fl>)
(define-~>fl-check fcmagnitude>= fcmagnitude fl>=)

(define-~>fl-check fcreal-part=  fcreal-part fl=)
(define-~>fl-check fcreal-part<  fcreal-part fl<)
(define-~>fl-check fcreal-part<= fcreal-part fl<=)
(define-~>fl-check fcreal-part>  fcreal-part fl>)
(define-~>fl-check fcreal-part>= fcreal-part fl>=)

(define-~>fl-check fcimag-part=  fcimag-part fl=)
(define-~>fl-check fcimag-part<  fcimag-part fl<)
(define-~>fl-check fcimag-part<= fcimag-part fl<=)
(define-~>fl-check fcimag-part>  fcimag-part fl>)
(define-~>fl-check fcimag-part>= fcimag-part fl>=)

;;**************************************************************************************************
;; Basic arithmetic
;;**************************************************************************************************
(provide fcsum fc+ fc- fc* fcprod fcprod_alt fc/)

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
(: fc- (Float-Complex Float-Complex * -> Float-Complex))
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
(: fc* (Float-Complex * -> Float-Complex))
(define fc*
  (case-lambda
    [() 1.0+0.0i]
    [([z : Float-Complex]) z]
    [([z1 : Float-Complex][z2 : Float-Complex])
     (make-fcrectangular (fl*+* (fcreal-part z1)(fcreal-part z2)
                                (fl± (fcimag-part z1))(fcimag-part z2))
                         (fl*+* (fcreal-part z1)(fcimag-part z2)
                                (fcimag-part z1)(fcreal-part z2)))]
    [ [zs : Float-Complex *] (fcprod zs)]))

;; accumulating summing errors
(define (fcprod [zs : (Listof Float-Complex)]) : Float-Complex
  (cond
    [(empty? zs) +1.0+0.0i]
    [(empty? (cdr zs)) (car zs)]
    [(empty? (cddr zs)) (fc* (car zs) (cadr zs))]
    [else
     (define-values (small big*)(partition (λ ([z : Float-Complex])(fl< (fcmagnitude z) 1.0)) zs))
     (define-values (inf big)(partition (λ ([z : Float-Complex])(fcinfinite? z)) big*))
     (cond
       [(and (empty? small)(empty? big))
        (foldr fc* (car inf)(cdr inf))]
       [else
        (define a
          (let loop : Float-Complex
            ([small ((inst sort Float-Complex) small fl< #:key fcmagnitude)]
             [big ((inst sort Float-Complex) big
                                             (λ ([x : Flonum][y : Flonum]) (cond
                                                                             [(and (flrational? x)(flrational? y))
                                                                              (fl> x y)]
                                                                             [(flrational? x) #t]
                                                                             [else #f]))
                                             #:key fcmagnitude)]
             [z : Float-Complex 1.0+0.0i])
;(println (list small big z))
            (cond
              [(and (empty? small)(empty? big)) z]
              [(empty? small) (foldr fc* z big)];fc* only works on 2 arguments / (it's using this function for more)
              [(empty? big) (foldr fc* z small)]
              [(fl< (fcmagnitude z) 1.0)
               (loop small (cdr big) (fc* (car big) z))]
              [else
               (loop (cdr small) big (fc* (car small) z))])))
        (foldr fc* a inf)])]))

;; chance of over/underflow
(define (fcprod_alt [zs : (Listof Float-Complex)]) : Float-Complex
  (define len (length zs))
  (for/fold ([rs : (Listof Flonum) '()]
             [is : (Listof Flonum) '()]
             #:result (make-fcrectangular (flsum rs)
                                          (flsum is)))
            ([i (in-range (expt 2 len))])
;(println (list i rs is))
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
      [else (values rs (cons (flprod p) is))])))
(module+ showcase
  (fc* 1e155-1e154i .2e154-1e154i)
  (* 1e155-1e154i .2e154-1e154i)
  (fc* -2.395513621781982e+40+9.67196562623574e-309i -2.7385197440766536e+144+3.130002798848634e-32i)
  (* -2.395513621781982e+40+9.67196562623574e-309i -2.7385197440766536e+144+3.130002798848634e-32i))

;; *******************************************************************************
;; Floating - Complex - Division
;; *******************************************************************************
;; code from @gus-massa
(define (fc/_2 [z1 : Float-Complex][z2 : Float-Complex])
  (define a (real-part z1))
  (define b (imag-part z1))
  (define c (real-part z2))
  (define d (imag-part z2))

  (cond
    ;; *******************************************************************************
    ;; One of the denominater parts 0
    ;; *******************************************************************************
    [(flzero? d) ;includes the 0+0i case
     (make-fcrectangular (fl/ a c)
                         (fl/ b c))]
    [(flzero? c)
     (make-fcrectangular (fl/ b d)
                         (fl± (fl/ a d)))]
    
    ;; *******************************************************************************
    ;; nan?
    ;; *******************************************************************************
    [(or (flnan? a) (flnan? b) (flnan? c) (flnan? d))
     (make-fcrectangular +nan.0 +nan.0)]
    
    ;; *******************************************************************************
    ;; 0
    ;; *******************************************************************************
    [(and (flzero? a) (flzero? b)); c+di not 0 (see 2 & 3)
     0.0+0.0i]

    ;; *******************************************************************************
    ;; One of the parts inf
    ;; *******************************************************************************
    [(or (flinfinite? c) (flinfinite? d))
     (cond
       [(or (flinfinite? a) (flinfinite? b))
        +nan.0+nan.0i]
       [else
        ;not bothering with the correct sign...
        0.0+0.0i])]
    [(or (flinfinite? a) (flinfinite? b))
     ;; result is going to be ±inf.0±inf.0i anyway, so normalise all other numbers
     ;; if both a and b are inf, this can result in nan.0 for one of the parts
     ;; but this is (imo) correct, since we don't know the angle of z1
     ;; (99% sure it's not π/4 as reported by angle)
     (define a* (if (flinfinite? a) a (flsgn a)))
     (define b* (if (flinfinite? b) b (flsgn b)))
     (define d* (flsgn d))
     (define c* (flsgn c))
     (make-fcrectangular (+ (* a* c*) (* b* d*))
                         (- (* b* c*) (* a* d*)))]
    ;; *******************************************************************************
    ;; MAIN SOLVER
    ;; *******************************************************************************
    [else
     (define (full [a : Flonum][b : Flonum][c : Flonum][d : Flonum]): Float-Complex
       (define r (fl/ d c))
       (define den (fl+ c (fl* d r)))
       
       (cond
         [(or (flzero? a) (flzero? b))
          (define (one-is-zero [a : Flonum][b : Flonum][c : Flonum][d : Flonum]
                               [r : Flonum]) : Float-Complex
            (cond
              [(fl<=* 0.0 (flabs b) 1.0)
               (define r/den (fl/ r den))
               (cond
                 [(flinfinite? r/den)
                  ;not happening very often... c=d and den~e-2xx
                  ;should not happen anymore due to shifting before start
                  (begin
                    (define b* (fl* b 1024.))
                    (define c* (fl* c 1024.))
                    (define d* (fl* d 1024.))
                    (define den* (fl+ c* (fl* r d*)))
                    (make-fcrectangular (fl/ (fl* b* r) den*)
                                        (fl/ b* den*)))]
                 [else
                  (make-fcrectangular (fl* b r/den)
                                      (fl/ b den))])]
              [else
               (define db (fl* d b))
               (cond
                 [(flinfinite? db)
                  (make-fcrectangular (fl/ (fl* b r) den)
                                      (fl/ b den))]
                 [else
                  (make-fcrectangular (fl/ (fl/ db c) den)
                                      (fl/ b den))])]))
          (if (flzero? a)
              (one-is-zero 0.0 b c d r)
              (let ([z (one-is-zero 0.0 a c d r)])
                ;(* -i z)
                (make-fcrectangular (fcimag-part z)
                                    (fl± (fcreal-part z)))))]
         [else
          (define ar (if (fl< (flabs a) 1.0) (fl* a r) (fl* (fl/ a c) d)))
          (define br (if (fl< (flabs b) 1.0) (fl* b r) (fl* (fl/ b c) d)))
          (define den* (fl+ c (fl/ (fl* d d) r)))

          (cond
            [(or (flinfinite? ar)(flinfinite? br))
             (make-fcrectangular (fl/ (fl+ a (fl* b r)) den)
                                 (fl/ (fl- b (fl* a r)) den))]
            [else
             (make-fcrectangular (fl/ (fl+ a br) den)
                                 (fl/ (fl- b ar) den))])]))

     (define a2 (fllog2 (flabs a)))
     (define b2 (fllog2 (flabs b)))
     (define c2 (fllog2 (flabs c)))
     (define d2 (fllog2 (flabs d)))
     (define l (filter flrational? (list a2 b2 c2 d2)))
     (define m- (apply flmin* +inf.0 l))
     (define m+ (apply flmax* 0.0 l))
     (define mid (fl/ (fl+ m- m+) 2.0))
     (define-values (a* b* c* d*)
       (cond
         [(and (fl< m- -800.0)(fl< mid -50.0))
          (define shift (flexp2 (flmin 1000. (fl± (flround mid)))))
          (values (* a shift)(* b shift)(* c shift)(* d shift))]
         [(and (< 800 m+)(< 50 mid))
          (define shift (flexp2 (flmax -1000. (fl± (flround mid)))))
          (values (* a shift)(* b shift)(* c shift)(* d shift))]
         [else (values a b c d)]))

     (if (>= (abs c*) (abs d*))
         (full a* b* c* d*)
         (full (- b*) a* (- d*) c*))]))

(: fc/ (Float-Complex Float-Complex * -> Float-Complex))
(define fc/
  (case-lambda
    [([z : Float-Complex])
     (fc/_2 1.0+0.0i z)]
    [([z0 : Float-Complex][z1 : Float-Complex])
     (fc/_2 z0 z1)]
    [([z : Float-Complex] . [zs : Float-Complex *])
     (define-values (small big)(partition (λ ([z : Float-Complex])(fl< (fcmagnitude z) 1.0)) zs))
     (let loop : Float-Complex
       ([small ((inst sort Float-Complex) small fl< #:key fcmagnitude)]
        [big ((inst sort Float-Complex) big fl> #:key fcmagnitude)]
        [z : Float-Complex z])
       ;(println (list small big z))
       (cond
         [(and (empty? small)(empty? big)) z]
         [else
          (define zm (fcmagnitude z))
          (cond
            [(empty? small)
             (if (fl< zm 1.0)
                 (fc/_2 z (fcprod big))
                 (foldr fc/_2 z big))];not using fcprod on big for fear of overflow...
            [(empty? big)
             (if (fl< zm 1.0)
                 (foldr fc/_2 z small)
                 (fc/_2 z (fcprod big)))]
            [(fl< zm 1.0)
             (loop small (cdr big) (fc/_2 (car big) z))]
            [else
             (loop (cdr small) big (fc/_2 (car small) z))])]))]))

;;**************************************************************************************************
;; exp
;;**************************************************************************************************
(provide fcexp)

(define (fcexp [z : Float-Complex]) : Float-Complex
  (cond
    [(fcnan? z) +nan.0+nan.0i]
    [(and (eqv? (fcreal-part z) -inf.0) (flinfinite? (fcimag-part z)))
     ;; we don't really know the direction of zero, but do we realy care enough to make this nan?
     0.0+0.0i]
    [(flzero? (fcimag-part z))
     ;; error in typed-racket optimizer
     (make-fcrectangular (flexp (fcreal-part z)) (fcimag-part z))]
    [else (exp z)]))

;;**************************************************************************************************
;; log
;;**************************************************************************************************
(provide fclog)

(define (fclog [z : Float-Complex]) : Float-Complex
  (cond
    ;; racket assumes that (angle +inf.0+inf.0i) = pi/4, I disagree
    [(and (flinfinite? (fcreal-part z))(flinfinite? (fcimag-part z)))
     +nan.0+nan.0i]
    ;; racket makes a difference between (log -x-0.0i) and (log -x+0.0i) in y±pi×i
    ;; this is admittely nicer than always using +pi×i (both are right ...)
    [else (log z)]))

;;**************************************************************************************************
;; expt
;;**************************************************************************************************
(provide fcexpt)
(define (fcexpt [zb : Float-Complex][ze : Float-Complex]) : Float-Complex
  ;; this is the same as racket does, but since flcos/sin etc are more accurate than the standard
  ;; implementation, this gives accurate results
  ;; as with exp, the sign of zero is important!
  (define mag (fcmagnitude zb)) (define ang (fcangle zb))
  (define c (fcreal-part ze)) (define d (fcimag-part ze))
  (define nm (fl* (flexpt mag c)
                  (flexp (fl± (fl* ang d)))))
  (define na (if (flzero? d)
                 (fl* ang c)
                 (fl+ (fl* (fllog mag) d)
                      (fl* ang c))))
  (cond
    [(and (flzero? nm) (flinfinite? na))
     ;; prefer wrong sign zero over nan
     0.0+0.0i]
    [else
     (make-fcrectangular (fl* nm (flcos na))
                         (fl* nm (flsin na)))]))
(define (fcexpt2 [zb : Float-Complex][ze : Float-Complex]) : Float-Complex
  ;; this is the same as racket does, but since flcos/sin etc are more accurate than the standard
  ;; implementation, this gives accurate results
  ;; as with exp, the sign of zero is important!
  (define mag (fcmagnitude zb)) (define ang (fcangle zb))
  (define c (fcreal-part ze)) (define d (fcimag-part ze))
  (define nm (fl* (flexpt mag c)
                  (flexp (fl± (fl* ang d)))))
  (define-values (nac nas)
    (if (flzero? d)
        (let ([t (fl* ang c)]) (values (flcos t)(flsin t)))
        (let ([t1 (fl* (fllog mag) d)]
              [t2 (fl* ang c)])
          (values
           (fl+ (fl* (flcos t1) (flcos t2))
                (fl* (flsin t1) (flsin t2)))
           (fl+ (fl* (flsin t1) (flcos t2))
                (fl* (flcos t1) (flsin t2)))))))
(println (list mag ang nm nac nas))
  (cond
    [(and (flzero? nm) (flnan? nac))
     ;; prefer wrong sign zero over nan
     0.0+0.0i]
    [else
     (make-fcrectangular (fl* nm nac)
                         (fl* nm nas))]))
(define (fcexpt3 [zb : Float-Complex][ze : Float-Complex]) : Float-Complex
  ;; this is the same as racket does, but since flcos/sin etc are more accurate than the standard
  ;; implementation, this gives accurate results
  ;; as with exp, the sign of zero is important!
  (define mag (fcmagnitude zb)) (define ang (fcangle zb))
  (define a (fcreal-part zb)) (define b (fcimag-part zb))
  (define c (fcreal-part ze)) (define d (fcimag-part ze))
  (define nm (fl* (flexpt mag c)
                  (flexp (fl± (fl* ang d)))))
  (define na (if (flzero? d)
                 (fl* ang c)
                 (let ()
                   (define xa (flmin a b))
                   (define ya (flmax a b))
                   (define-values (u1 u2)   (fl2/ xa 0.0 ya 0.0))
                   (define-values (uu1 uu2) (fl2sqr u1 u2))
                   (define-values (up1 up2) (fl2+ uu1 uu2 1.0 0.0))
                   (define-values (us1 us2) (fl2sqrt up1 up2))
                   (define-values (mg1 mg2) (fl2* us1 us2 ya 0.0))
                   (define-values (lg1 lg2) (fl2log mg1 mg2))
                   
                   (define-values (*11 *12) (fl2* lg1 lg2 d 0.0))
                   (define-values (*21 *22) (fl2* ang 0.0 c 0.0))
                   (define-values (p1 p2) (fl2+ *11 *12 *21 *22))
                   p1)))
(println (list mag ang nm na))
  (cond
    [(and (flzero? nm) (flinfinite? na))
     ;; prefer wrong sign zero over nan
     0.0+0.0i]
    [else
     (make-fcrectangular (fl* nm (flcos na))
                         (fl* nm (flsin na)))]))

(define (fcexpt4 [zb : Float-Complex][ze : Float-Complex]) : Float-Complex
  (define (make-ans [ans : (Listof Float-Complex)]) : Float-Complex
    (fc/ (fcsum ans) (fc (length ans))))
  (for/fold ([ans : (Listof Float-Complex) '()]
             #:result (make-ans ans))
            ([i (in-range 20)]
             #:break (and (not (empty? ans))
                          (fc= (make-ans ans) (make-ans (cdr ans)))))
    (define z* (fc (expt 2 i)))
    (cons (fc* (fcexpt3 (fc* zb z*) ze) (fcexpt3 (fc/ z*) ze))
          ans)))

(define (bffcexpt [zb : Float-Complex][ze : Float-Complex]) : Float-Complex
  (local-require math/bigfloat)
  (define a (bf (fcreal-part zb))) (define b (bf (fcimag-part zb)))
  (define c (bf (fcreal-part ze))) (define d (bf (fcimag-part ze)))
  (define mag (bfhypot a b))
  (define ang (bfatan2 b a))
  (define nm (bf* (bfexpt mag c)
                  (bfexp (bf- (bf* ang d)))))
  (define-values (nac nas)
    (if (bf= 0.bf d)
        (let ([t (bf* ang c)]) (values (bfcos t) (bfsin t)))
        (let ([t1 (bf* (bflog mag) d)]
              [t2 (bf* ang c)])
          (println (list t1 t2))
          (values
           (bf+ (bf* (bfcos t1)(bfcos t2))
                (bf* (bfsin t1)(bfsin t2)))
           (bf+ (bf* (bfsin t1)(bfcos t2))
                (bf* (bfcos t1)(bfsin t2)))))))
  (cond
    [else
     (make-fcrectangular (bigfloat->flonum (bf* nm nac))
                         (bigfloat->flonum (bf* nm nas)))]))
(module+ test
  (check-equal? (fcexpt4 -2.1507959208093305e-308+2.3283599054444013e-106i -7.155603820248143e-68+0.633029814350625i)
                -0.3697574373443255+0.012235065503937041i)
  (check-equal? (fcexpt4  1.417835141498884e-308-2.296288150836904e-307i 5.5208913900880785e-155+1.8417515613935356i)
                15.680404792287854+3.6956975682622084i)
  (check-equal? (fcexpt4  2.424576983608611e-269+6.532320935117516e-135i -5.922315769101014e-287-0.23895875485177157i)
                0.006099829715045764-1.4554960242395032i))

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
