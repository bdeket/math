#lang typed/racket
(require "student-t-pdf.rkt"
         "student-t-cdf.rkt"
         "../gamma-dist.rkt"         
         "../../../flonum.rkt"
         "../dist-struct.rkt"
         "normal-inv-cdf.rkt"
         "normal-random.rkt")

(provide make-student-t-pdf
         make-student-t-cdf
         (rename-out [make-inverse-cdf make-student-t-inverse-cdf])
         flstudent-t-sample)
                     
;;; Student t distribution

;; Parameters
;   μ   - location parameter
;   σ   - scale parameter
;   ν   - degrees of freedom

;; Domains
;   μ   - any real number
;   σ   - any positive real number
;   ν   - any positive real number


;;;
;;; Implementation
;;;



(: find-bracket : ((Flonum -> Flonum) Flonum Flonum  -> (values Flonum Flonum)))
(define (find-bracket h [a -1.] [b 1.] )
  ; Since the function h is monotone, this strategy works.
  (define ha (h a))
  (define hb (h b))
  (if (or (and (positive? ha) (negative? hb))
          (and (negative? ha) (positive? hb)))
      (values a b)
      (find-bracket h (* 2. a) (* 2. b))))


;; (define-type (Inverse-CDF Out)
;;   (case-> (Real -> Out)
;;           (Real Any -> Out)
;;           (Real Any Any -> Out)))

(: make-inverse-cdf : (case-> (Real           -> (Inverse-CDF Flonum))
                              (Real Real Real -> (Inverse-CDF Flonum))))
(define make-inverse-cdf
  (case-lambda
    ; Y ~ σX+μ
    [(μ σ ν)
     (define inv-F (make-inverse-cdf ν))
     (λ (p [log? #f] [1-p? #f])
       (define x (inv-F p log? 1-p?))
       (define y (+ (* σ x) μ))
       (fl y))]
    
    ; X ~ t(ν)
    [(ν)
     (: 1-?-swap (-> (Real Any -> Flonum) (->* (Real) (Any Any) Flonum)))
     (define (1-?-swap f)
       (λ (p [log? #f] [1-? #f])
         (if (or log? (<= p 0.5))
             (if 1-?
                 ;; actually looking for close to 1
                 (- (f p log?))
                 ;; regular
                 (f p log?)
                 )
             (if 1-?
                 ;; actually looking for close to 0
                 (f (- 1 p) log?)
                 ;; regular
                 (- (f (- 1 p) log?))
                 ))))
     (case ν
       ; special cases
       [(1 2 4)
        (: plain-inv-F : (Flonum -> Flonum))
        (define plain-inv-F
          (case ν
            [(1) (λ (p)
                   (fltan (* pi (fl- p 0.5))))]
            [(2) (λ (p)
                   (define α (fl* 4. p (fl- 1. p)))
                   (* 2. (fl- p 0.5) (flsqrt (fl/ 2. α))))]
            [(4) (λ (p)
                   (define α (fl* 2. (flsqrt p) (flsqrt (fl- 1. p))))
                   (define q (fl/ (flcos (fl/ (flacos α) 3.)) α))
                   (fl* (flsgn (fl- p 0.5)) 2. (flsqrt (fl- q 1.))))]
            [else (λ (p) 0.0)])) ; happy type checking

        (1-?-swap
         (λ (p log?)
           (cond
             [(and (not log?) (< p 0))       +nan.0]
             [(if log? (< 0 p) (< 1 p))      +nan.0]
             [(if log? (= p -inf.0) (= p 0)) -inf.0]
             [(if log? (= p   0)  (= p 1))   +inf.0]
             [else
              (let ([p (fl p)])
                (plain-inv-F (if log? (flexp p) p)))])))]
       [(+nan.0)
        (λ (p [log? #f] [1-? #f]) +nan.0)]
       [(+inf.0)
        (1-?-swap
         (λ (p log?)
           (if log?
               (standard-flnormal-inv-log-cdf (fl p))
               (standard-flnormal-inv-cdf (fl p)))))]
       ; general
       [else
        (define F (make-student-t-cdf ν))
        (define lg1/2 (- (fllog 2.)))
        (let ([ν (fl ν)])
          (define-values (p- x-)
            (cond
              [(< ν 1e-20) (values 0.5 -max.0)]
              [(< ν 2e0)   (values (F -max.0 #f) -max.0)]
              [(let ([V (map fllog '(2e0   1e1  1e2 1e3 1e4   1e6))]
                     [X (map fllog '(1e162 1e33 1e4 6e1 5.1e1 39.))]
                     [T (fllog ν)])
               (for/or : (Option (List Flonum Flonum))
                 ([v0 (in-list V)]
                  [v1 (in-list (cdr V))]
                  [y0 (in-list X)]
                  [y1 (in-list (cdr X))]
                  #:when (<= v0 T v1))
                 (list 0. (- (flexp (+ y0 (* (/ (- T v0) (- v1 v0)) (- y1 y0))))))))
               => (λ (l) (apply values l))]
              [else   (values 0. -39.)]))
          (define lgp- (F -max.0 #t))
          (define lgp+ (F +max.0 #t))

          (: inv- (Flonum -> Flonum))
          (define inv-
            (let ([Q : (Listof (Pair Flonum Flonum)) '((0.5 . -1e-17))])
              (λ (p) ; input will be p in range [0 -> 0.5]
                (cond
                  [(<= p 0.)    +nan.0]
                  [(= p 0.5)       0.0]
                  [(= p -inf.0) -inf.0]
                  [(< p p-)     -inf.0]
                  [else
                   (: a0 (Option Flonum))(: b0 Flonum)(: f0 Flonum)
                   (define-values (a0 b0 f0)
                     (apply
                      values
                      (or (for/or : (Option (List Flonum Flonum Flonum))
                            ([q0 (in-list Q)]
                             [q1 (in-list (cdr Q))]
                             #:when (<= (car q0) p (car q1)))
                            (list (cdr q0) (cdr q1) (car q1)))
                          (list #f (cdar Q) (caar Q)))))
                   
                   (define-values (a b)
                     (if a0
                         (values a0 b0)
                         (let ([fx : Flonum f0][E : Flonum 2.])
                           (let find-bracket : (Values Flonum Flonum) ([a : Flonum (* b0 2.)] [b : Flonum b0] [fb : Flonum f0])
                             ; Since the function h is monotone, this strategy works.
                             (define fa (F a))
                             (when (< 0.075 (- fx fa))
                               (set! fx fa)
                               (set! Q (cons (cons fa a) Q)))
                             (when (< E (fllog (/ fx fa)))
                               (set! fx fa)(set! E (* 2. E))
                               (set! Q (cons (cons fa a) Q)))
                             (cond
                               [(<= fa p fb) (values a b)]
                               [else
                                (find-bracket (max x- (* 2. a)) a fa)])))))

                   (flbracketed-root (λ (x) (fl- (F x) p)) a b)]))))

          (: lginv (Flonum -> Flonum))
          (define lginv
            (let ([Q+ : (Listof (Pair Flonum Flonum)) (list (cons lg1/2  1e-16))]
                  [Q- : (Listof (Pair Flonum Flonum)) (list (cons lg1/2 -1e-16))])
              (λ (p) ; input will be p in range [-inf.0 -> 0.0]
                (cond
                  [(< 0. p)      +nan.0]
                  [(= p lg1/2)       0.0]
                  [(= p -inf.0) -inf.0]
                  [(< p lgp-)   -inf.0]
                  [(< lgp+ p)   +inf.0]
                  [(= p 0)      +inf.0]
                  [else
                   (define <? (< p lg1/2))
                   (define Q (if <? Q- Q+))
                   (define updateQ
                     (if <? (λ ([pair : (Pair Flonum Flonum)]) (set! Q- (cons pair Q-)))
                         (λ ([pair : (Pair Flonum Flonum)]) (set! Q+ (cons pair Q+)))))
                   (define in? (if <? <= >=))

                   (: a0 (Option Flonum))(: b0 Flonum)(: f0 Flonum)
                   (define-values (a0 b0 f0)
                     (apply
                      values
                      (or (for/or : (Option (List Flonum Flonum Flonum))
                            ([q0 (in-list Q)]
                             [q1 (in-list (cdr Q))]
                             #:when (in? (car q0) p (car q1)))
                            (list (cdr q0) (cdr q1) (car q1)))
                          (list #f (cdar Q) (caar Q)))))
                   
                   (define-values (a b)
                     (if a0
                         (values a0 b0)
                         (let ([fx : Flonum f0][E : Flonum 2.])
                           (let find-bracket : (Values Flonum Flonum)
                             ; if p<lg2 => x < 0, Q- is sorted from -inf.0 to +inf.0
                             ([a : Flonum (* b0 E)] [b : Flonum b0] [fb : Flonum f0])
                             ; Since the function h is monotone, this strategy works.
                             (define fa (F a #t))
                             (when (< E (if <? (/ fa fx) (/ fx fa)))
                               (set! fx fa)(set! E (* 2. E))
                               (updateQ (cons fa a)))
                             
                             (cond
                               [(in? fa p fb)
                                (when (if <? (< (* fx 2.) lgp-)
                                          (< lgp+ (/ fx 2.)))
                                  (updateQ (if <? (cons lgp- -max.0) (cons lgp+ +max.0))))
                                (values a b)]
                               [else
                                (find-bracket (* 2. a) a fa)])))))
                   (println Q+)
                   (println Q-)
                   
                   (flbracketed-root (λ (x) (fl- (F x #t) p)) a b)]))))
          
          (1-?-swap
           (λ (p log?)
             (if log?
                 (lginv (fl p))
                 (inv- (fl p))))))])]
    ))




(: flstudent-t-sample : (case-> (Real           Integer -> FlVector)
                                (Real Real Real Integer -> FlVector)))
(define flstudent-t-sample
  (case-lambda
    ; X ~ t(ν)    
    [(ν n) (cond
             [(n . < . 0)  (raise-argument-error 'sample-student-t "Natural" 1 n)]
             [else    

              (define ν/2 (fl (/ ν 2.)))
              ; Note: Our gamma distribution has a shape parameter.
              ;       A shape parameter of 2 corresponds to a a rate of 1/2.
              (define Xs  (flnormal-sample 0. 1. n))
              (define X²s (flgamma-sample ν/2 2. n))
              (build-flvector n
                              (λ (i)
                                (define X  (flvector-ref Xs  i))
                                (define X² (flvector-ref X²s i))
                                (fl (cast (/ X (sqrt (/ X² ν))) Real))))])]

    ; Y ~ σX+μ
    [(μ σ ν n) (cond
                 [(n . < . 0)  (raise-argument-error 'sample-student-t "Natural" 3 n)]
                 [else    
                  (define ν/2 (fl (/ ν 2.)))
                  (define Xs  (flnormal-sample 0. 1. n))
                  (define X²s (flgamma-sample ν/2 2. n))
                  (build-flvector n
                                  (λ (i)
                                    (define X  (flvector-ref Xs  i))
                                    (define X² (flvector-ref X²s i))
                                    (define x  (fl/ X (flsqrt (fl/ X² (fl ν)))))
                                    (fl+ (fl* (fl σ) x) (fl μ))))])]))

;;;
;;; Tests
;;;


(: nearly-equal? : (Real Real Real -> Boolean))
(define (nearly-equal? eps x y)
  (<= (abs (- x y)) eps))

;; Numerical test cases were computed by the free `wolframscript`.

;; All tests are expected to return $t.
;; If a set of tests results in #f, change `and` to `list` to see the
;; individual results.

#;(list "Density - PDF"
        (and
         ; N[PDF[StudentTDistribution[1], 0], 30]
         (nearly-equal? (expt 2 -54) ((make-pdf 1) 0)      0.3183098861837907)
         (nearly-equal? (expt 2 -55) ((make-pdf 2) 1)      (/ 1 (* 3 (sqrt 3))))
         ; generalized
         (nearly-equal? (expt 2 -55) ((make-pdf 2 2 1) 0)  0.07957747154594767)
         (nearly-equal? (expt 2 -55) ((make-pdf 2 2 1) 1)  0.12732395447351627)
         (nearly-equal? (expt 2 -55) ((make-pdf 3 4 5) 0)  0.06892452901798418)
         (nearly-equal? (expt 2 -55) ((make-pdf 3 4 5) 1)  0.08197963283068663)
         ; Log space
         ;   For pdf we compute log(p) directly without computing p first.
         ;   Note: The left value is actual the precise one. 
         (nearly-equal? (expt 2 -52) ((make-pdf 2) 1 #t)   (fllog ((make-pdf 2) 1)))       
         )
        "Cumulative - CDF"
        (and
         ; N[CDF[StudentTDistribution[1], 0], 30]
         (equal?                      ((make-cdf 1)  0)      0.5)
         (equal?                      ((make-cdf 1)  1)      0.75)
         (equal?                      ((make-cdf 1) -1)      0.25)
         (equal?                      ((make-cdf 2)  0)      0.5)
         (equal?                      ((make-cdf 2)  1)      0.7886751345948129)
         (nearly-equal? (expt 2 -55)  ((make-cdf 2) -1)      0.2113248654051871)
         ; generalized
         (nearly-equal? (expt 2 -55)  ((make-cdf 1 2 3) -1)  0.19550110947788532)
         (nearly-equal? (expt 2 -55)  ((make-cdf 1 2 3)  0)  0.3257239824240755)
         (nearly-equal? (expt 2 -55)  ((make-cdf 1 2 3)  1)  0.5)
         ; Log space
         ;   For cdf we compute p first, and the take the logarithm.
         ;   Is there a better way?
         (nearly-equal? (expt 2 -52) ((make-cdf 2) 1 #t)   (fllog ((make-cdf 2) 1)))
         )
        " Inverse Cumulative - Inverse CDF"
        ; Example to get expected result:
        ;   N[InverseCDF[StudentTDistribution[2], 1/10], 30]
        (and
         ; Special case ν=1
         (equal?                           ((make-inverse-cdf 1)  0)   -inf.0)
         (equal?                           ((make-inverse-cdf 1)  1)   +inf.0)
         (equal?                           ((make-inverse-cdf 1)  0.5) 0.) 
         (nearly-equal? (expt 2 -51)       ((make-inverse-cdf 1)  0.1) -3.0776835371752536)
         (nearly-equal? (expt 2 -51)       ((make-inverse-cdf 1)  0.9)  3.0776835371752536)
         ; Special case ν=2
         (equal?                           ((make-inverse-cdf 2)  0)   -inf.0)
         (equal?                           ((make-inverse-cdf 2)  1)   +inf.0)
         (equal?                           ((make-inverse-cdf 2)  0.5) 0.) 
         (nearly-equal? (expt 2 -51)       ((make-inverse-cdf 2)  0.1) -1.8856180831641267)
         (nearly-equal? (expt 2 -51)       ((make-inverse-cdf 2)  0.9)  1.8856180831641267)
         ; Special case ν=4
         (equal?                           ((make-inverse-cdf 4)  0)   -inf.0)
         (equal?                           ((make-inverse-cdf 4)  1)   +inf.0)
         (equal?                           ((make-inverse-cdf 4)  0.5) 0.) 
         (nearly-equal? (expt 2 -51)       ((make-inverse-cdf 4)  0.1) -1.5332062740589438)
         (nearly-equal? (expt 2 -51)       ((make-inverse-cdf 4)  0.9)  1.5332062740589438)
         ; General case
         (equal?                           ((make-inverse-cdf 3)  0.5) 0.) 
         (nearly-equal? (expt 2 -52)       ((make-inverse-cdf 3)  0.1) -1.6377443536962102)
         (nearly-equal? (expt 2 -52)       ((make-inverse-cdf 3)  0.9)  1.6377443536962102)
         (equal?                           ((make-inverse-cdf 5)  0)   -inf.0)
         (equal?                           ((make-inverse-cdf 5)  1)   +inf.0)
         (equal?                           ((make-inverse-cdf 5)  0.5) 0.) 
         (nearly-equal? (expt 2 -52)       ((make-inverse-cdf 5)  0.1) -1.475884048824481)
         (nearly-equal? (expt 2 -52)       ((make-inverse-cdf 5)  0.9)  1.475884048824481)
         ; Three parameters
         (nearly-equal? (expt 2 -55)  ((make-inverse-cdf 1 2 3)  0.5)  1)
         (nearly-equal? (expt 2 -51)  ((make-inverse-cdf 1 2 3)  0.1)  -2.2754887073924204)
         (nearly-equal? (expt 2 -50)  ((make-inverse-cdf 1 2 3)  0.9)   4.27548870739242)))
