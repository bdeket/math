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

(define (fcsin [z : Float-Complex])
  (define a (fcreal-part z))(define b (fcimag-part z))
  (make-fcrectangular (fl* (flsin a)(flcosh b))
                      (fl* (flcos a)(flsinh b))))
