#lang racket/base

(require racket/runtime-path
         ffi/unsafe
         (for-syntax racket/base))

(provide mpfr-lib mpc-lib)

(define-runtime-path libmpc-so
  (case (system-type)
    [(macosx) (error "TODO")'(so "libmpc.???.dylib")]
    [(windows) (error "TODO")'(so "libmpc-???.dll")]
    [else '(so "libmpc")]))

(define mpc-lib (ffi-lib libmpc-so '("3" "") #:fail (λ () #f)))
(define mpfr-lib (ffi-lib libmpc-so '("3" "") #:fail (λ () #f)))
