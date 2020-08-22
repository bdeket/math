#lang typed/racket/base

(require (only-in "mpc.rkt" consts 0ary-funs)
         "bigcomplex-mpc.rkt"
         "../bigfloat/utils.rkt")

(req/prov-uniform-collection "mpc.rkt" consts (Promise Bigcomplex))
(req/prov-uniform-collection "mpc.rkt" 0ary-funs (-> Bigcomplex))
