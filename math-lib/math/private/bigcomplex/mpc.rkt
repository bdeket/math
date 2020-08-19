#lang racket/base

(require (for-syntax racket/base)
         ffi/unsafe
         (only-in '#%foreign ctype-scheme->c)
         ffi/unsafe/cvector
         ffi/unsafe/custodian
         ffi/unsafe/define
         ffi/unsafe/atomic
         ffi/unsafe/global
         ffi/unsafe/custodian
         racket/math
         (only-in racket/vector vector-append)
         racket/runtime-path
         racket/promise
         racket/serialize
         (only-in rnrs/arithmetic/bitwise-6
                  bitwise-first-bit-set)
         "../bigfloat/gmp.rkt"
         "../bigfloat/utils.rkt"
         ;"../bigfloat/mpfr.rkt"
         (only-in "../bigfloat/mpfr.rkt"
                  thread-safe?
                  _mpfr
                  _prec_t
                  _mpfr-pointer
                  _rnd_t
                  mpfr-lib
                  make-mpfr
                  new-mpfr
                  real->bigfloat
                  mpfr-get-d
                  bigfloat?
                  bf-rounding-mode
                  bf-precision
                  bigfloat-precision
                  bigfloat->vector-for-hash
                  bf
                  bfrint
                  bfsgn
                  bfsum
                  bfsin
                  bfcos
                  bfceiling
                  bffloor
                  bftruncate
                  bigfloat->string
                  bigfloat->integer
                  bigfloat->rational
                  bigfloat-custom-write
                  string->bigfloat
                  bfnan?
                  bf=?
                  bfrational?
                  bfinfinite?
                  bfinteger?
                  mpfr-get-version))

(provide
 ;; Parameters
 bc-rounding-mode
 bc-min-precision
 bc-max-precision
 bc-precision
 ;; Type predicate
 (rename-out [mpc? bigcomplex?])
 ;; Accessors
 bcreal-part
 bcimag-part
 bigcomplex-precision
 ;; Conversion to and from Real
 flonum->bigcomplex
 integer->bigcomplex
 bigfloat->bigcomplex
 number->bigcomplex
 bigcomplex->float-complex
 bigcomplex->integer-complex
 bigcomplex->exact-number
 bigcomplex->number
 ;; String conversion
 bigcomplex->string
 string->bigcomplex
 ;; Main constructor
 bc
 bcmake-rectangular
 bcmake-polar
 bigcomplex-deserialize-info
 ;; Low-level stuff
 mpc-get-version
 mpc-lib
 get-mpc-fun
 _mpc_rnd_t
 _prec_t
 _mpc
 _mpc-pointer
 (struct-out mpc))

;; Arithmetic, comparison, and other functions are provided by the macros that create them

;; ===================================================================================================
;; Setup/takedown

(define-runtime-path libmpc-so
  (case (system-type)
    [(macosx) (error "TODO")'(so "libmpc.???.dylib")]
    [(windows) (error "TODO")'(so "libmpc-???.dll")]
    [else '(so "libmpc")]))

(define mpc-lib (ffi-lib libmpc-so '("3" "") #:fail (λ () #f)))

;; The mpfr_buildopt_tls_p() function indicates whether mpfr was compiled as thread-safe:
;; The mpc library provides everything from mpfr
(define-syntax get-mpc-fun
  (syntax-rules ()
    [(_ name type) (get-mpc-fun name type (make-not-available name))]
    [(_ name (_fun fun-arg ...) fail-thunk)
     (get-ffi-obj name mpc-lib (_fun #:in-original-place? (not thread-safe?) fun-arg ...) fail-thunk)]))


;; TODO not sure if this is necessary, mpc does not have an equivalent
(define mpc-free-cache (get-mpc-fun 'mpfr_free_cache (_fun -> _void)))
(when mpc-lib
  ;; Register `mpfr-free-cache` as shutdown action once within each place
  (let ([ht (get-place-table)])
    (unless (hash-ref ht 'mpfr-finalization-registered? #f)
      (let ([root-custodian (make-custodian-at-root)])
        (call-as-atomic
         (lambda ()
           (parameterize ([current-custodian root-custodian])
             (register-custodian-shutdown
              mpc-free-cache ; acts as a "random" object for a shutdown handle
              (λ (free)
                ;; The direct reference here is important, since custodian holds only
                ;; a weak reference to shutdown handle:
                (mpc-free-cache))))
           (hash-set! ht 'mpfr-finalization-registered? #t)))))))

;; ===================================================================================================
;; MPC types ... most are as in MPFR

(define _mpc_rnd_t _int)
(define (make-mpc-rnd rndr [rndi rndr])
  (+ rndr (arithmetic-shift rndi 4)))

;; ===================================================================================================
;; Parameters: rounding mode, precision

;; TODO for now only supporting one precision for both real/imag-parts

;; One of 'nearest 'zero 'up 'down
(define bc-rounding-mode (make-parameter 'nearest))
(define (current-rounding-mode)
  (define bcm (bc-rounding-mode))
  (define to-int (ctype-scheme->c _rnd_t))
  (if (and (pair? bcm))
      (make-mpc-rnd (to-int (car bcm) (cdr bcm)))
      (make-mpc-rnd (to-int bcm))))
(define (real-rounding)
  (define bcm (bc-rounding-mode))
  (if (pair? bcm) (car bcm) bcm))
(define (imag-rounding)
  (define bcm (bc-rounding-mode))
  (if (pair? bcm) (cdr bcm) bcm))

;; minimum precision (1 bit can't be rounded correctly)
(define bc-min-precision 2)
;; maximum precision (the number when longs are 64 bits is ridiculously large)
(define bc-max-precision _long-max)

(define bc-precision
  (make-parameter 128 (λ (p) (cond [(p . < . bc-min-precision)  bc-min-precision]
                                   [(p . > . bc-max-precision)  bc-max-precision]
                                   [else  p]))))

(define-syntax-rule (for-real body ...)
  (parameterize ([bf-rounding-mode (real-rounding)]
                 [bf-precision (bc-precision)])
    body ...))
(define-syntax-rule (for-imag body ...)
  (parameterize ([bf-rounding-mode (imag-rounding)]
                 [bf-precision (bc-precision)])
    body ...))

;; ===================================================================================================
;; _mpc type (bigcomplex)

(define (bigcomplex-equal? x1 x2 _)
  (define x1r (mpc-re x1))
  (define x1i (mpc-im x1))
  (define x2r (mpc-re x2))
  (define x2i (mpc-im x2))
  (and
   (equal? x1r x2r)
   (equal? x1i x2i)))


(define (bigcomplex-hash x recur-hash)
  (define vr (bigfloat->vector-for-hash (mpc-re x)))
  (define vi (bigfloat->vector-for-hash (mpc-im x)))
  (recur-hash (vector-append vr vi)))

(define (bigcomplex-deserialize re im)
  (parameterize ([bc-precision (bigfloat-precision re)])
    (bigfloat->bigcomplex re im)))

(define bigcomplex-deserialize-info
  (make-deserialize-info
   bigcomplex-deserialize
   #f))

(define bigcomplex-serialize-info
  (make-serialize-info
   (λ (x) (vector (mpc-re x)(mpc-im x)))
   #'bigcomplex-deserialize-info
   #f
   (or (current-load-relative-directory) 
       (current-directory))))


;; _mpc: two multi-precision floats (the main data type)
(define-cstruct _mpc ([re _mpfr] [im _mpfr])
  #:malloc-mode 'nonatomic
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write (λ (b port mode) (bigcomplex-custom-write b port mode))
  #:property prop:equal+hash (list bigcomplex-equal? bigcomplex-hash bigcomplex-hash)
  #:property prop:serializable bigcomplex-serialize-info
  )

(define sizeof-mpc (ctype-sizeof _mpc))

;#|

;; ===================================================================================================
;; Foreign functions

(define mpc-get-version (get-mpc-fun 'mpc_get_version (_fun -> _string)))
(define mpc/mpfr-get-version (get-mpc-fun 'mpfr_get_version (_fun -> _string)))
(unless (string=? (mpc/mpfr-get-version) (mpfr-get-version))
  (println "Warning: bigcomplex and bigfloat are using different versions of mpfr!"))

;; Allocation/initialization
(define mpc-set-nan (get-mpc-fun 'mpc_set_nan (_fun _mpc-pointer -> _void)))
(define mpc-init2 (get-mpc-fun 'mpc_init2 (_fun _mpc-pointer _prec_t -> _void)))
(define mpc-clear (get-mpc-fun 'mpc_clear (_fun _mpc-pointer -> _void)))
;; A "special free" for strings allocated and returned by mpfr_get_str:
(define mpc-free-str (get-mpc-fun 'mpc_free_str (_fun _pointer -> _void)))

;; Conversions from _mpfr to other types
(define mpc-get-str
  (get-mpc-fun 'mpc_get_str (_fun _int _ulong _mpc-pointer _mpc_rnd_t
                                    -> _pointer)))

;; Conversions from other types to _mpc
(define mpc-set (get-mpc-fun 'mpc_set (_fun _mpc-pointer _mpc-pointer _mpc_rnd_t -> _int)))

(define mpc-set-d (get-mpc-fun 'mpc_set_d  (_fun _mpc-pointer _double _mpc_rnd_t -> _int)))
(define mpc-set-si (get-mpc-fun 'mpc_set_si (_fun _mpc-pointer _long _mpc_rnd_t -> _int)))
(define mpc-set-z (get-mpc-fun 'mpc_set_z  (_fun _mpc-pointer _mpz-pointer _mpc_rnd_t -> _int)))
(define mpc-set-fr (get-mpc-fun 'mpc_set_fr (_fun _mpc-pointer _mpfr-pointer _mpc_rnd_t -> _int)))

(define mpc-set-d-d (get-mpc-fun 'mpc_set_d_d  (_fun _mpc-pointer _double _double _mpc_rnd_t -> _int)))
(define mpc-set-si-si (get-mpc-fun 'mpc_set_si_si (_fun _mpc-pointer _long _long _mpc_rnd_t -> _int)))
(define mpc-set-z-z (get-mpc-fun 'mpc_set_z_z  (_fun _mpc-pointer _mpz-pointer _mpz-pointer _mpc_rnd_t -> _int)))
(define mpc-set-fr-fr (get-mpc-fun 'mpc_set_fr_fr (_fun _mpc-pointer _mpfr-pointer _mpfr-pointer _mpc_rnd_t -> _int)))

(define mpc-set-str (get-mpc-fun 'mpc_set_str (_fun _mpc-pointer _string _int _mpc_rnd_t -> _int)))

;; ===================================================================================================
;; Construction

;; new-mpc: integer -> bigcomplex
(define (new-mpc prec)
  (make-mpc (new-mpfr prec) (new-mpfr prec)))

;; ===================================================================================================
;; Accessors

;; real and imag parts : bigcomplex -> bigfloat
(define bcreal-part mpc-re)
(define bcimag-part mpc-im)
(define (bigcomplex-precision bc)
  (cons (bigfloat-precision (mpc-re bc))
        (bigfloat-precision (mpc-im bc))))

;; ===================================================================================================
;; Conversion from Racket data types to bigcomplex

;; flonum->bigcomplex : float [Option float] -> bigcomplex
;; Converts a Racket inexact real to a bigcomplex; rounds if bc-precision < 53.
(define (flonum->bigcomplex real [imag #f])
  (define z (new-mpc (bc-precision)))
  (if imag
      (mpc-set-d-d z real imag (current-rounding-mode))
      (mpc-set-d z real (current-rounding-mode)))
  z)

;; integer->bigcomplex : integer [Option integer] -> bigcomplex
;; Converts a Racket integer to a bigcomplex; rounds if necessary.
(define (integer->bigcomplex real [imag #f])
  (define z (new-mpc (bc-precision)))
  (if imag
      (if (and (_long? real)(_long? imag))
          (mpc-set-si-si z real imag (current-rounding-mode))
          (mpc-set-z-z z (integer->mpz real) (integer->mpz imag) (current-rounding-mode)))
      (if (_long? real)
          (mpc-set-si z real (current-rounding-mode))
          (mpc-set-z z (integer->mpz real) (current-rounding-mode))))
  z)

;; bigfloat->bigcomplex : bigfloat [Option bigfloat] -> bigcomplex
;; copies bigfloats into a bigcomplex; rounding if necessary
(define (bigfloat->bigcomplex real [imag #f])
  (define z (new-mpc (bc-precision)))
  (if imag
      (mpc-set-fr-fr z real imag (current-rounding-mode))
      (mpc-set-fr z real (current-rounding-mode)))
  z)

;; number->bigcomplex : number -> bigcomplex
;; Converts any Racket number to a bigcomplex; rounds if necessary.
(define (number->bigcomplex value)
  (define r (real-part value))
  (define i (imag-part value))
  (define r&i
    (let ([r (real-part value)]
          [i (imag-part value)])
      (if (= i 0) (list r)(list r i))))
  (cond
    [(andmap inexact? r&i) (apply flonum->bigcomplex r&i)]
    [(andmap integer? r&i) (apply integer->bigcomplex r&i)]
    [else
     (bigfloat->bigcomplex (for-real (real->bigfloat r))
                           (for-imag (real->bigfloat i)))]))

;; ===================================================================================================
;; Conversion from mpfr_t to Racket data types

;; bigcomplex->float-complex : bigcomplex -> float-complex
;; Converts a bigfloat to a Racket float; rounds if necessary.
(define (bigcomplex->float-complex z)
  (make-rectangular (mpfr-get-d (mpc-re z) (real-rounding))
                    (mpfr-get-d (mpc-im z) (imag-rounding))))

;; bigcomplex->integer-complex : bigcomplex -> integer
;; Converts a bigfloat to a Racket integer; rounds if necessary.
(define (bigcomplex->integer-complex x)
  (unless (bcinteger? x) (raise-argument-error 'bigcomplex->integer-complex "bcinteger?" x))
  (make-rectangular (for-real (bigfloat->integer (mpc-re x)))
                    (for-imag (bigfloat->integer (mpc-im x)))))

;; bigfloat->rational : bigfloat -> rational
;; Converts a bigfloat to a Racket rational; does not round.
(define (bigcomplex->exact-number x)
  (unless (bcrational? x) (raise-argument-error 'bigcomplex->rational "bfrational?" x))
  (make-rectangular (for-real (bigfloat->rational (mpc-re x)))
                    (for-imag (bigfloat->rational (mpc-im x)))))

; bigfloat->real : bigfloat -> (or exact-rational flonum)
(define (bigcomplex->number x)
  (cond [(bcrational? x)  (bigcomplex->exact-number x)]
        [else  (bigcomplex->float-complex x)]))

;; ===================================================================================================
;; String conversions

(define (mpc-get-string z base rnd)
  (define bs (mpc-get-str base 0 z rnd))
  (define str (bytes->string/utf-8 (cast bs _pointer _bytes)))
  (mpc-free-str bs)
  str)

;; Converts a bigfloat to a Racket string of digits, with a decimal point.
;; Outputs enough digits to exactly recreate the bigfloat using string->bigfloat.
(define (bigcomplex->string x)
  (format "~a~a~ai"
          (regexp-replace "bf" (bigfloat->string (mpc-re x)) "0")
          (if (and (equal? (bcimag-sgn x) (bf 1)) (bfrational? (mpc-im x))) "+" "")
          (regexp-replace "bf" (bigfloat->string (mpc-im x)) "0")))

;; string->bigcomplex : string [integer] -> bigfloat
;; Converts a Racket string to a bigfloat.
;; TODO - check exhaustively!!!
(define (string->bigcomplex str)
  (define r&i (regexp-split #px"(?<=[^e])[+-]" str))
  (define len (length r&i))
  (cond
    [(< 2 len) #f]
    [(= 2 len)
     (define r (car r&i))
     (define slen (string-length r))
     (define i (string-append (substring str slen (+ slen 1)) (cadr r&i)))
     (cond
       [(regexp-match #px"i$" i)
        (define bfr (for-real (string->bigfloat r)))
        (define bfi (for-imag (string->bigfloat (regexp-replace #px"i$" i ""))))
        (if (and bfr bfi)
            (bigfloat->bigcomplex bfr bfi)
            #f)]
       [else #f])]
    [(= 1 len)
     (cond
       [(regexp-match #px"i$" (car r&i))
        (define bfi (for-imag (string->bigfloat (regexp-replace #px"i$" (car r&i) ""))))
        (if bfi (bigfloat->bigcomplex (for-real (bf 0)) bfi) #f)]
       [else
        (define bfr (for-real (string->bigfloat (car r&i))))
        (if bfr (bigfloat->bigcomplex  bfr (for-imag (bf 0))) #f)])]
    [else #f]))

(define (bigcomplex-custom-write x port mode)
  (cond
    [(and mpc-lib mpfr-lib gmp-lib)
     (write-string "(bc " port)
     (for-real (bigfloat-custom-write (mpc-re x) port mode))
     (write-string " " port)
     (for-imag (bigfloat-custom-write (mpc-im x) port mode))
     (write-string ")" port)]
    [else
     (write-string "#<_mpc>" port)]))
;; ===================================================================================================
;; Main bigcomplex constructor

;; bc : (or number string) -> bigcomplex
;;    : (U real bf) (U real bf) -> bigcomplex
(define bc
  (case-lambda
   [(v)
    (cond [(string? v)
             (define x (string->bigcomplex v))
             (if x x (error 'bf "expected well-formed decimal number; given ~e" v))]
          [else
           (number->bigcomplex v)])]
   [(r i) (bcmake-rectangular r i)]))

;; bc : (U real bf) (U real bf) -> bigcomplex
(define (bcmake-rectangular real imag)
  (define r&i (list real imag))
  (cond
    [(andmap (λ (x)(and (real? x)(inexact? x))) r&i) (apply flonum->bigcomplex r&i)]
    [(andmap integer? r&i) (apply integer->bigcomplex r&i)]
    [else
     (bigfloat->bigcomplex
      (if (bigfloat? real) real (for-real (bf real)))
      (if (bigfloat? imag) imag (for-imag (bf imag))))]))

;; bc : (U real bf) (U real bf) -> bigcomplex
(define (bcmake-polar magnitude angle)
  (define bfangle (if (bigfloat? angle) angle (for-real (bf angle))))
  (bcmul (bc magnitude)
         (bc (for-real (bfcos bfangle))
             (for-imag (bfsin bfangle)))))

;; ===================================================================================================
;; Unary functions bc->bc

(define-for-syntax 1ary-funs (list))
(provide (for-syntax 1ary-funs))

(define-syntax-rule (provide-1ary-fun name c-name)
  (begin
    (define cfun (get-mpc-fun c-name (_fun _mpc-pointer _mpc-pointer _mpc_rnd_t -> _int)))
    (define (name x)
      (define y (new-mpc (bc-precision)))
      (cfun y x (current-rounding-mode))
      y)
    (provide name)
    (begin-for-syntax (set! 1ary-funs (cons #'name 1ary-funs)))))

(define-syntax-rule (provide-1ary-funs [name c-name] ...)
  (begin (provide-1ary-fun name c-name) ...))

(provide-1ary-funs
 [bcproject 'mpc_proj]
 [bcneg 'mpc_neg]
 [bcsqr 'mpc_sqr]
 [bcconjugate 'mpc_conj]
 [bcsqrt 'mpc_sqrt]
 [bcexp 'mpc_exp]
 [bclog 'mpc_log]
 [bclog10 'mpc_log10]
 [bcsin 'mpc_sin]
 [bccos 'mpc_cos]
 [bctan 'mpc_tan]
 [bcsinh 'mpc_sinh]
 [bccosh 'mpc_cosh]
 [bctanh 'mpc_tanh]
 [bcasin 'mpc_asin]
 [bcacos 'mpc_acos]
 [bcatan 'mpc_atan]
 [bcasinh 'mpc_asinh]
 [bcacosh 'mpc_acosh]
 [bcatanh 'mpc_atanh])

(begin-for-syntax
  (set! 1ary-funs (remove* (list #'bcneg) 1ary-funs free-identifier=?)))

(define (bcreal-sgn z) (bfsgn (mpc-re z)))
(define (bcimag-sgn z) (bfsgn (mpc-im z)))

(define (bcround x)
  (parameterize ([bc-rounding-mode  'nearest])
    (bigfloat->bigcomplex (bfrint (mpc-re x))
                          (bfrint (mpc-im x)))))

(provide bcreal-sgn bcimag-sgn bcround)
(begin-for-syntax
  (set! 1ary-funs (list* #'bcround 1ary-funs)))


(define (bcsum zs)
  (bigfloat->bigcomplex (for-real (bfsum (map mpc-re zs)))
                        (for-imag (bfsum (map mpc-im zs)))))

(provide bcsum)

(define (bcceiling z)
  (bigfloat->bigcomplex (bfceiling (mpc-re z))
                        (bfceiling (mpc-im z))))
(define (bcfloor z)
  (bigfloat->bigcomplex (bffloor (mpc-re z))
                        (bffloor (mpc-im z))))
(define (bctruncate z)
  (bigfloat->bigcomplex (bftruncate (mpc-re z))
                        (bftruncate (mpc-im z))))
(provide bcceiling bcfloor bctruncate)
(begin-for-syntax
  (set! 1ary-funs (list* #'bcceiling #'bcfloor #'bctruncate 1ary-funs)))

;; Unary functions bc->bf

(define-for-syntax 1ary-bf-funs (list))
(provide (for-syntax 1ary-bf-funs))

(define-syntax-rule (provide-1ary-bf-fun name c-name)
  (begin
    (define cfun (get-mpc-fun c-name (_fun _mpfr-pointer _mpc-pointer _rnd_t -> _int)))
    (define (name x)
      (define y (new-mpfr (bc-precision)))
      (cfun y x (real-rounding))
      y)
    (provide name)
    (begin-for-syntax (set! 1ary-bf-funs (cons #'name 1ary-bf-funs)))))

(define-syntax-rule (provide-1ary-bf-funs [name c-name] ...)
  (begin (provide-1ary-bf-fun name c-name) ...))

(provide-1ary-bf-funs
 [bcangle 'mpc_arg]
 [bcmagnitude 'mpc_abs]
 [bcnorm 'mpc_norm])

(begin-for-syntax
  (set! 1ary-bf-funs (list* #'bcreal-part #'bcreal-sgn #'bcimag-part #'bcimag-sgn 1ary-bf-funs)))

;; Unary functions bc->bc bc

(define-for-syntax 1ary2-funs (list))
(provide (for-syntax 1ary2-funs))

(define-syntax-rule (provide-1ary2-fun name c-name)
  (begin
    (define cfun
      (get-mpc-fun c-name (_fun _mpc-pointer _mpc-pointer _mpc-pointer _mpc_rnd_t -> _int)))
    (define (name x)
      (define y (new-mpc (bc-precision)))
      (define z (new-mpc (bc-precision)))
      (cfun y z x (current-rounding-mode))
      (values y z))
    (provide name)
    (begin-for-syntax (set! 1ary2-funs (cons #'name 1ary2-funs)))))

(define-syntax-rule (provide-1ary2-funs [name c-name] ...)
  (begin (provide-1ary2-fun name c-name) ...))

(provide-1ary2-funs
 [bcsin+cos 'mpc_sin_cos])

;; ===================================================================================================
;; Unary predicates bc -> boolean

(define-for-syntax 1ary-preds (list))
(provide (for-syntax 1ary-preds))

(define-syntax-rule (provide-1ary-pred name c-name)
  (begin
    (define cfun (get-mpc-fun c-name (_fun _mpfr-pointer -> _int)))
    (define (name x) (not (zero? (cfun x))))
    (provide name)
    (begin-for-syntax (set! 1ary-preds (cons #'name 1ary-preds)))))

(define-syntax-rule (provide-1ary-preds [name c-name] ...)
  (begin (provide-1ary-pred name c-name) ...))

#| (provide-1ary-preds) NON DEFINED|#

(define (bcnan? z) (or (bfnan? (mpc-re z))(bfnan? (mpc-im z))))
(define (bcinfinite? z) (or (bfinfinite? (mpc-re z))(bfinfinite? (mpc-im z))))
(define (bcrational? z) (and (bfrational? (mpc-re z))(bfrational? (mpc-im z))))
(define (bcinteger? z) (and (bfinteger? (mpc-re z))(bfinteger? (mpc-im z))))
(define (bczero? z) (bc=? z (force 0.bc)))

(provide bcnan? bcinfinite? bcrational? bcinteger? bczero?)
(begin-for-syntax
  (set! 1ary-preds (list* #'bcnan? #'bcinfinite? #'bcrational? #'bcinteger? #'bczero? 1ary-preds)))

;; ===================================================================================================
;; Binary functions bc bc -> bc

(define-for-syntax 2ary-funs (list))
(provide (for-syntax 2ary-funs))

(define-syntax-rule (provide-2ary-fun name c-name)
  (begin
    (define cfun
      (get-mpc-fun c-name (_fun _mpc-pointer _mpc-pointer _mpc-pointer _mpc_rnd_t -> _int)))
    (define (name x1 x2)
      (define y (new-mpc (bc-precision)))
      (cfun y x1 x2 (current-rounding-mode))
      y)
    (provide name)
    (begin-for-syntax (set! 2ary-funs (cons #'name 2ary-funs)))))

(define-syntax-rule (provide-2ary-funs [name c-name] ...)
  (begin (provide-2ary-fun name c-name) ...))

(provide-2ary-funs
 [bcadd 'mpc_add]
 [bcsub 'mpc_sub]
 [bcmul 'mpc_mul]
 [bcdiv 'mpc_div]
 [bcexpt 'mpc_pow])

(begin-for-syntax
  (set! 2ary-funs (remove* (list #'bcadd #'bcsub #'bcmul #'bcdiv)
                           2ary-funs
                           free-identifier=?)))

;; ===================================================================================================
;; Binary predicates

(define mpc-cmp (get-mpc-fun 'mpc_cmp (_fun _mpc-pointer _mpc-pointer -> _int)))
(define mpc-cmp-abs (get-mpc-fun 'mpc_cmp_abs (_fun _mpc-pointer _mpc-pointer -> _int)))

(define (bc=? z1 z2)(zero? (mpc-cmp z1 z2)))
(define (bcmagnitude=? z1 z2)(zero? (mpc-cmp-abs z1 z2)))
(define (bcmagnitude<? z1 z2)(= -1 (mpc-cmp-abs z1 z2)))
(define (bcmagnitude<=? z1 z2)(<= (mpc-cmp-abs z1 z2) 0))
(define (bcmagnitude>? z1 z2)(= 1 (mpc-cmp-abs z1 z2)))
(define (bcmagnitude>=? z1 z2)(<= 0 (mpc-cmp-abs z1 z2)))


(define (bcquadrant z1 [z2 (force 0.bc)])
  (case (mpc-cmp z2 z1)
    [(0)  0]
    [(1)  quadrant-R-]
    [(2)  quadrant-R+]
    [(4)  quadrant-I-]
    [(5)  quadrant-C]
    [(6)  quadrant-D]
    [(8)  quadrant-I+]
    [(9)  quadrant-B]
    [(10) quadrant-A]))

(define (bcin-quadrant? q z1 [z2 (force 0.bc)])
  (and ((if (list? q) member equal?) (bcquadrant z1 z2) q) #t))
(define quadrant-A 'q++)
(define quadrant-B 'q-+)
(define quadrant-C 'q--)
(define quadrant-D 'q+-)
(define quadrant-R+ 'q+0)
(define quadrant-R- 'q+0)
(define quadrant-I+ 'q0+)
(define quadrant-I- 'q0-)
(define quadrant-R (list 0 quadrant-R+ quadrant-R-))
(define quadrant-I (list 0 quadrant-I+ quadrant-I-))
(define quadrant-A+ (list quadrant-A quadrant-R+ quadrant-I+ 0))
(define quadrant-B+ (list quadrant-B quadrant-R- quadrant-I+ 0))
(define quadrant-C+ (list quadrant-C quadrant-R- quadrant-I- 0))
(define quadrant-D+ (list quadrant-D quadrant-R+ quadrant-I- 0))
(define quadrant-0<R (list quadrant-A quadrant-D quadrant-R+))
(define quadrant-0<=R (list* quadrant-A quadrant-D quadrant-R+ quadrant-I))
(define quadrant-R<0 (list quadrant-B quadrant-C quadrant-R-))
(define quadrant-R<=0 (list* quadrant-B quadrant-C quadrant-R- quadrant-I))
(define quadrant-0<I (list quadrant-A quadrant-B quadrant-I+))
(define quadrant-0<=I (list* quadrant-A quadrant-B quadrant-I+ quadrant-R))
(define quadrant-I<0 (list quadrant-C quadrant-D quadrant-I-))
(define quadrant-I<=0 (list* quadrant-C quadrant-D quadrant-I- quadrant-R))

(provide bc=?
         bcmagnitude=? bcmagnitude<? bcmagnitude<=? bcmagnitude>? bcmagnitude>=?
         bcquadrant bcin-quadrant?
         quadrant-A quadrant-B quadrant-C quadrant-D quadrant-R+ quadrant-R- quadrant-I+ quadrant-I-
         quadrant-R quadrant-I quadrant-A+ quadrant-B+ quadrant-C+ quadrant-D+
         quadrant-R<0 quadrant-R<=0 quadrant-0<R quadrant-0<=R
         quadrant-I<0 quadrant-I<=0 quadrant-0<I quadrant-0<=I)

;; ===================================================================================================
;; Constants and variable-precision constants (i.e. 0-ary functions)

(define-for-syntax consts (list))
(provide (for-syntax consts))

(define-syntax-rule (define-bc-constant name prec expr)
  (begin
    (define name (lazy (parameterize ([bc-precision  prec]) expr)))
    (provide name)
    (begin-for-syntax
      (set! consts (cons #'name consts)))))

(define-bc-constant 0.bc 2 (integer->bigcomplex 0))
(define-bc-constant 1.bc 2 (integer->bigcomplex 1))
(define-bc-constant i.bc 2 (integer->bigcomplex 0 1))
(define-bc-constant -1.bc 2 (integer->bigcomplex 1))
(define-bc-constant -i.bc 2 (integer->bigcomplex 0 1))
(define-bc-constant +nan.bc 2 (number->bigcomplex +nan.0+nan.0i))


;; ===================================================================================================
;; Other

(define mpc-fma (get-mpc-fun 'mpc_fma (_fun _mpc-pointer _mpc-pointer _mpc-pointer _mpc-pointer _mpc_rnd_t -> _int)))
(define (bcmuladd z1 z2 z3)
  (define y (new-mpc (bc-precision)))
  (mpc-fma y z1 z2 z3 (current-rounding-mode))
  y)

(define mpc-rootofunity (get-mpc-fun 'mpc_rootofunity (_fun _mpc-pointer _ulong _ulong _mpc_rnd_t -> _int)))
(define (bcexp-2pii n)
  (define y (new-mpc (bc-precision)))
  (mpc-rootofunity y (denominator n) (numerator n) (current-rounding-mode))
  y)

(provide bcmuladd bcexp-2pii)

#|
TODO
Comparison functions
  mpc_comp_si_si mpc_cmp_si
* mpc_cmp_abs

Basic arithmetic functions
  mpc_add_ui mpc_add_fr
  mpc_sub_fr mpc_fr_sub mpc_sub_ui mpc_ui_sub mpc_ui_ui_sub
  mpc_mul_ui mpc_mul_si mpc_mul_fr mpc_mul_i
  mpc_div_ui mpc_div_fr mpc_ui_div mpc_fr_div
  mpc_mul_2ui mpc_mul_2si
  mpc_div_2ui mpc_div_2si
  mpc_pow_d mpc_pow_ld mpc_pow_si mpc_pow_z mpc_pow_fr

Other
  mpc_urandom (bc gmp_randstate_t -> int);; done in fct of bfrandom, see bigcomplex-mpc
|#

