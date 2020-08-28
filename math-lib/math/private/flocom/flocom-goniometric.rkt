#lang typed/racket/base

#|
fcsin fccos fctan fcasin fcacos fcatan
fccsc fcsec fccot ... ?
fcsinh fccosh fctanh fcasinh fcacosh fcatanh
fcsinpix fccospix fctanpix
fccscpix fcsecpix fccotpix
|#

(require "flocom-base.rkt"
         "flonum-helpers.rkt")

(provide fcsin fccos fctan)

(define (fcsin [z : Float-Complex]) : Float-Complex
  (define a (fcreal-part z)) (define b (fcimag-part z))
  (make-fcrectangular (fl* (flsin a) (flcosh b))
                      (fl* (flcos a) (flsinh b))))

(define (fccos [z : Float-Complex]) : Float-Complex
  (define a (fcreal-part z)) (define b (fcimag-part z))
  (make-fcrectangular (fl* (flcos a) (flcosh b))
                      (fl± (fl* (flsin a) (flsinh b)))))

(define (fctan [z : Float-Complex]) : Float-Complex
  (define 2a (* 2.0 (fcreal-part z))) (define 2b (* 2.0 (fcimag-part z)))
  (define den (fl+ (flcos 2a) (flcosh 2b)))
  (make-fcrectangular (fl/ (flsin 2a) den)
                      (fl/ (flsinh 2b) den)))
