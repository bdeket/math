#lang typed/racket/base

#|
fcasin fcacos fcatan
fccsc fcsec fccot ... ?

fcsinh fccosh fctanh
fcasinh fcacosh fcatanh

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
  (make-fcrectangular (if (fl< 720. (flabs 2b))
                          0.0 ;not the right sign, but at least not nan.0
                          (fl/ (flsin 2a) den))
                      (if (fl< 40.0 (flabs 2b))
                          (flsgn 2b)
                          (fl/ (flsinh 2b) den))))

;asin
;series at 0?
;continued fraction: (sqrt 1 - z²)*[z/(1-(2z²/(3-2z²/(5-12z²/(7-12z²/(9... ]
