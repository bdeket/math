#lang typed/racket/base

(require (for-syntax racket/base)
         racket/list
         (only-in math/base pi))
(require "../../flonum.rkt")

(provide (all-from-out "../../flonum.rkt")
         pi
         fl± flzero? fl*+* flprod fl<=* flmin* flmax*)

;;**************************************************************************************************
;; Flonum helpers
;;**************************************************************************************************
(define (fl± [a : Flonum]) : Flonum (fl* -1.0 a))

(define flzero? (ann zero? (-> Flonum Boolean)))

(define (fl*+* [a : Flonum][b : Flonum][c : Flonum][d : Flonum]) : Flonum
  (define ab (* a b))
  (define cd (* c d))
;(println (list ab cd))
  
  (define (normal [ab : Flonum][cd : Flonum])(+ ab cd))
  
  (define (abnormal [< : (Flonum Flonum -> Boolean)][test : (Flonum -> Boolean)]
                    [a : Flonum][b : Flonum][c : Flonum][d : Flonum][cd : Flonum])
    (if (< (flabs a)(flabs b))
        (abnormal0 < test b a c d cd)
        (abnormal0 < test a b c d cd)))

  (define (abnormal0 [< : (Flonum Flonum -> Boolean)][test : (Flonum -> Boolean)]
                     [a : Flonum][b : Flonum][c : Flonum][d : Flonum][cd : Flonum])
    (if (test cd)
        (if (< (flabs c)(flabs d))
            (abnormal2 a b d c)
            (abnormal2 a b c d))
        (abnormal1 a b cd)))
  
  (define (abnormal1 [a : Flonum][b : Flonum][cd : Flonum]) : Flonum
;(println (list a b cd))
    (fl* a (fl+ b (fl/ cd a))))
  
  (define (abnormal2 [a : Flonum][b : Flonum][c : Flonum][d : Flonum]) : Flonum
;(println (list a b c d))
    (fl* a (fl+ b (fl* (fl/ c a) d))))
  
  (cond
    ;one is nan
    [(or (flnan? ab)(flnan? cd)) +nan.0]
    ;one is inf and the other points in the other direction (ie: +inf.0 -1)
    ;there is a chance that with reordering the result would not overflow
    [(flinfinite? ab)
     (if (fl< (* (flsgn ab) cd) 0.0)
         (abnormal fl< flinfinite? a b c d cd)
         ab)]
    [(flinfinite? cd)
     (if (fl< (* (flsgn cd) ab) 0.0)
         (abnormal fl< flinfinite? c d a b ab)
         cd)]
    ;underflow for both
    ;there is a chance that their sum might still be a measurable value
    [(and (flzero? ab) (not (or (flzero? a) (flzero? b)))
          (fl< (flabs cd) 4.450147717014403e-308));smallest flonum that would register something
     (abnormal fl> (λ ([x : Flonum])(fl< x 1.0)) a b c d cd)]
    [(and (flzero? cd) (not (or (flzero? c) (flzero? d)))
          (fl< (flabs ab) 4.450147717014403e-308))
     (abnormal fl> (λ ([x : Flonum])(fl< x 1.0)) c d a b ab)]
    ;Normal path
    [else (normal ab cd)]))
(module+ showcase
  (fl*+* 1e155 .2e154 -1e154 1e154)
  (fl*+* 1e-163 1e-161 .2e-161 1e-162))

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

(define (fl<=* [a1 : Flonum][a2 : Flonum] . [as : Flonum *]) : Boolean
  (let loop ([a0 : Flonum a1]
             [as : (Listof Flonum)(list* a2 as)])
    (cond
      [(empty? as) #t]
      [(fl<= a0 (car as))
       (loop (car as)(cdr as))]
      [else #f])))
(define (flmin* [a1 : Flonum] . [as : Flonum *]) : Flonum
  (let loop ([a0 : Flonum a1]
             [as : (Listof Flonum) as])
    (cond
      [(empty? as) a0]
      [else
       (loop (flmin a0 (car as)) (cdr as))])))
(define (flmax* [a1 : Flonum] . [as : Flonum *]) : Flonum
  (let loop ([a0 : Flonum a1]
             [as : (Listof Flonum) as])
    (cond
      [(empty? as) a0]
      [else
       (loop (flmax a0 (car as)) (cdr as))])))
