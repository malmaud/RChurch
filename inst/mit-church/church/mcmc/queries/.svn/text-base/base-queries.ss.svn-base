#!r6rs

;; created on November 12, 2009 by NDG
;; authors: noah goodman

(library
 (church mcmc queries base-queries)

 (export mh-query
         visited-mh-query
         pt-query
         )

 (import (church utils rnrs)
         (_srfi :1)
         (church utils utils)
         (church constraint-propagation trace-constraint-propagation)
         (church mcmc mcmc-core)
         (church readable-scheme)
         (church church-eval logmath)
         (church church-eval syntax)
         (church church-eval church-eval)
         (church church-eval trace-eval)
         )

 ;;to be registered (in std-env) with (register-query! 'mh-query mh-query)
 (define (mh-query M N normal-form-query-expr env)
   (repeated-mcmc-query-core default-initialize
                             (repeat-kernel N (make-mh-kernel (make-trace-walk-proposal) trace-score-proc "trace-walk-kernel"))
                             M
                             (lambda (s) true) ;;every time a cycle is up, take a sample
                             trace-finish
                             normal-form-query-expr
                             env))

  (define (visited-mh-query M N visitor normal-form-query-expr env)
   (repeated-mcmc-query-core default-initialize
                             (repeat-kernel N (make-mh-kernel (make-trace-walk-proposal) trace-score-proc "trace-walk-kernel"))
                             M
                             (lambda (s) true) ;;every time a cycle is up, take a sample
                             (lambda (state)
                               (let ((sample (trace-finish state)))
                                 (apply-church-procedure visitor (list sample))
                                 sample))
                             normal-form-query-expr
                             env))

 (define (pt-query in-vars ladders M N normal-form-query-expr env)
   (repeated-mcmc-query-core default-initialize
                             (repeat-kernel N (make-mh-kernel (make-trace-walk-proposal) trace-score-proc "trace-walk-kernel"))
                             M
                             (lambda (s) true) ;;every time a cycle is up, take a sample
                             trace-finish
                             (PT-transform in-vars ladders normal-form-query-expr)
                             env))

 (define (PT-transform temp-vars temp-ladders normal-form-query-expr)
   (let* ((zipped-temps (apply zip temp-ladders))
          (chain-proc `(lambda ,temp-vars ,normal-form-query-expr)))
     `(let* ((temp-permutation (sample-index-permutation ,(length zipped-temps)))
             (returns (inverse-permute-list
                       (map (lambda (args) (apply ,chain-proc args))
                            (permute-list ',zipped-temps temp-permutation))
                       temp-permutation)))
        ;;returns a list of pairs (val . constraint-val)
        (pair (first (first returns)) (apply and (map rest returns))))))

 )
