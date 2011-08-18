#!r6rs

(library
 (church mcmc queries tempered-transitions)

 (export TT-proposal make-TT-kernel tt-query *tt-steps*)

 (import (church utils rnrs)
         (church readable-scheme)
                                        ;(church church-eval laziness)
                                        ;(church church-eval traces)
         (church mcmc mcmc-core)
         (church church-eval church-eval)
         (church church-eval environments-lexical)
         (church church-eval logmath)
         (_srfi :1))

 (define *tt-steps* (make-parameter false))
 
 ;;tempered transitions take a temperature schedule (a list of pairs: (number of mh steps at this temp . list of temperatures) ).
 ;;they do a sequence of temperature-changes (with trace-update), followed by trace-walk kernel moves.
 ;;they first go up the temp-schedule, then back down (symmetrically!).
 ;;due to detailed balance, only the scores of intermediate states and the f/b for temp-change trace-updates need be tracked.

 ;;TODO: allow tempering on posterior-temperature.
 ;;TODO: add erp-choice probs as (optional) part of schedule?

(define (tt-query temp-vars base-temps temp-schedule M N normal-form-query-expr env)
   (repeated-mcmc-query-core default-initialize
                             (repeat-kernel N (make-TT-kernel temp-vars base-temps temp-schedule))
                             M
                             (lambda (s) true) ;;every time a cycle is up, take a sample
                             trace-finish
                             normal-form-query-expr
                             (extend-environment temp-vars base-temps env) ))


 (define (make-TT-kernel temp-vars base-temps temp-schedule . kernel)
   (let ((kernel (if (null? kernel)
                     (make-mh-kernel (make-trace-walk-proposal)  trace-score-proc "in-TT-trace-walk-kernel")
                     (first kernel))))
     (make-mh-kernel (make-TT-proposal temp-vars
                                       base-temps
                                       temp-schedule
                                       kernel)
                     trace-score-proc
                     "TT-kernel")))
 
 (define (make-TT-proposal temp-vars base-temps temp-schedule kernel)
   (let* ((temps (pair (pair 0 base-temps) temp-schedule))
          (temps (append temps (reverse temps))))
   (lambda (tc env)
     (let-values (( (tc fw bw)  (TT-proposal tc env temp-vars temps kernel)))
       (make-move fw bw tc)))))

 ;;this is the main TT-proposal fn. assumes schedule is already a symmetric (up-down) path.
 ;;assumes starting temp is the base temp (where the tc was sampled).
 ;;returns tc and f/b-probs for whole TT proposal (assuming things cancel due to detailed balance and symmetric temp schedule).
 (define (TT-proposal tc env temp-vars temp-schedule kernel . accum-f/b)
   (let* ((accum-forw-prob (if (null? accum-f/b) LOG-PROB-1 (first accum-f/b)))
          (accum-backw-prob (if (null? accum-f/b) LOG-PROB-1 (second accum-f/b)))
          (steps-at-temp (first (first temp-schedule)))
          (temps (rest (first temp-schedule)))
          (extended-env (extend-environment temp-vars temps (enclosing-environment env)))
          (mcmc-kernel (repeat-kernel steps-at-temp kernel))
          (rejuvinated-tc (mcmc-kernel tc extended-env))) ;;apply mcmc kernels at this temp.
     (if (null? (rest temp-schedule))
         ;;that's it -- return the tc and f/b-prob 1:
         (values tc accum-forw-prob accum-backw-prob)
         ;go to next temp level -- step the temps forward, doing a trace-update at the new temps and accumulating the f/b-probs:
         (let*-values (( (new-temps) (rest (second temp-schedule)))
                       ( (extended-env) (extend-environment temp-vars new-temps (enclosing-environment env)))
                       ( (next-tc next-forw-prob next-backw-prob) (church-eval rejuvinated-tc extended-env))
                       ( (next-score) (trace-score-proc next-tc))
                       ( (curr-score) (trace-score-proc rejuvinated-tc)))
           (when (*tt-steps*) (for-each display (list "TT-proposal: old-temps: " temps ", new-temps: " new-temps "\n"
                                                      "   curr-score: " curr-score ", next-score: " next-score
                                                      ", next-forw-prob: " next-forw-prob ", next-backw-prob: " next-backw-prob "\n")))
           (TT-proposal next-tc env temp-vars (rest temp-schedule) kernel
                        (+ next-forw-prob curr-score accum-forw-prob)
                        (+ next-backw-prob next-score accum-backw-prob))))))
                        
                        ;(+ next-forw-prob next-score accum-forw-prob)
                        ;(+ next-backw-prob curr-score accum-backw-prob)))))))

 )


;;;old version
;;  ;;this is the main TT-proposal fn. assumes schedule is already a symmetric (up-down) path.
;;  ;;returns tc and f/b-probs for whole TT proposal (assuming things cancel due to detailed balance and symmetric temp schedule).
;;  (define (TT-proposal tc env temp-vars temp-schedule kernel . accum-f/b)
;;    (let ((accum-forw-prob (if (null? accum-f/b) LOG-PROB-1 (first accum-f/b)))
;;          (accum-backw-prob (if (null? accum-f/b) LOG-PROB-1 (second accum-f/b)))
;;          (steps-at-temp (first (first temp-schedule))))
;;      (if (= steps-at-temp 0)
;;          (if (null? (rest temp-schedule))
;;              ;that's it -- return the tc and f/b-prob 1:
;;              (values tc accum-forw-prob accum-backw-prob)
;;              ;go to next temp level -- step the temps forward, doing a trace-update at the new temps and accumulating the f/b-probs:
;;              (let*-values (( (temps) (rest (second temp-schedule)))
;;                            ( (extended-env) (extend-environment temp-vars temps (enclosing-environment env)))
;;                            ( (next-tc next-forw-prob next-backw-prob) (church-eval tc extended-env)))
;;                (TT-proposal next-tc env temp-vars (rest temp-schedule) kernel
;;                             (+ next-forw-prob accum-forw-prob)
;;                             (+ next-backw-prob accum-backw-prob))))
;;          ;do an MH step at this temp, accumulate the score ratio into f/b-probs:
;;          (let*-values (( (temps) (rest (first temp-schedule)))
;;                        ( (extended-env) (extend-environment temp-vars temps (enclosing-environment env)))
;;                        ( (next-tc) (kernel tc extended-env))
;;                        ( (next-score) (trace-container->score next-tc))
;;                        ( (curr-score) (trace-container->score tc)))
;;            (TT-proposal next-tc env temp-vars (pair (pair (- steps-at-temp 1) temps) (rest temp-schedule)) kernel
;;                         (+ accum-forw-prob next-score)  
;;                         (+ accum-backw-prob curr-score))))))