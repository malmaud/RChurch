#!r6rs

;;written by NDG on Jun 17, 2008
;;based (long ago) on SICP eval code.

;;this file is the wrapper for church's eval. most of the actual work happens in trace-eval.ss and trace-update.ss

(library (church church-eval church-eval)
         
         (export church-eval 
                 trace-container? 
                 trace-container->value 
                 trace-container->score 
                 trace-container->erps
                 ;;trace-container->gradient
                 trace-container->xrp-registry ;;used by constraint-prop, should remove export someday.
                 trace-container->mem-registry ;;ditto.
                 trace-container->trace        ;;ditto.
                 make-trace-container          ;;ditto.
                 )
         
         (import (rename (church utils rnrs)
                         (real? rnrs-real?))
                 (church readable-scheme)
                 (church church-eval traces)
                 (church church-eval laziness)
                 (church church-eval trace-eval)
                 (church church-eval trace-update)
                 (church church-eval trace-update-parameters)
                 (church utils utils)
                 (church utils AD)
                 (_srfi :1) ; lists
                 )
         
         
         ;;;church-eval can take a trace or a syntax object and returns a trace and forward/backward probs (ie. proposal).
         ;;the optional arguments should alternate a flag with it's vaue. flags and are:
         ;;  'propose: make a proposal, value is path/name of erp to make a proposal to.
         ;;  'force-complete-eval: force re-evaluation to happen for all nodes, value is true/false, default false.
         
         (define (church-eval in env . control-arguments)
           (parameterize ([xrp-registry (if (trace-container? in)
                                            (copy-xrp-registry (trace-container->xrp-registry in))
                                            (new-xrp-registry))]
                          [mem-registry (if (trace-container? in)
                                            (copy-mem-registry (trace-container->mem-registry in))
                                            (new-mem-registry))]
                          [release-queue '()]
                          )

                         ;; (hash-table-walk (mem-registry) (lambda (key memproc-table)
                         ;; (display (length (hash-table-keys memproc-table)))(display " ")))(newline)
                         (let*
                             ((params (setup-parameters control-arguments))
                              (trace-f/b (if (trace-container? in)
                                             (call/cc (lambda (return)
                                                        (parameterize ([*church-eval-exit-on-exception* return]
                                                                       [*update-params* params])
                                        ;(write (erp-to-propose (*update-params*)))
                                                                      (trace-update (trace-container->trace in) env '()))))
                                             (trace-eval in env '() (params->constraints params)))))
                           
                           ;; force the score and erps fresh here, within the dynamic
                           ;; context of the registries, to prep for whatever else we need to do.
                           (force-lrb-fresh (trace->score (proposal->trace trace-f/b)))
                           (force-lrb-fresh (get-dominated-erps (proposal->trace trace-f/b)))

                           
                           ;;release keys from registries (when trace-delete has added the associated objects to the release queue):
                           (map (lambda (obj) (cond
                                          ((xrp? obj) (release-xrp obj))
                                          ((memoized-procedure? obj) (release-memproc obj))
                                          (else (error "church-eval" "don't know how to release that object..."))))
                                (release-queue))
                           
                           ;;return a trace-container and forw/rev proposal scores.
                           (values (make-trace-container (proposal->trace trace-f/b)  (xrp-registry) (mem-registry))
                                   (force-lrb-fresh (forward/backward-score->forward-score (proposal->forward/backward-score trace-f/b)))
                                   (force-lrb-fresh (forward/backward-score->backward-score (proposal->forward/backward-score trace-f/b)))))))
         
         
         
         ;;trace-container adt
         
         (define (trace-container? tc)  (and (list? tc) (eq? (first tc) 'trace-container)))
         (define (make-trace-container trace xrp-registry mem-registry) (list 'trace-container
                                                                              trace
                                                                              xrp-registry
                                                                              mem-registry))
                                                                              ;(mlist false)))
         
         (define (trace-container->value tc)  (trace->value (second tc)))         
         (define (trace-container->score tc)  (parameterize ([xrp-registry (trace-container->xrp-registry tc)]
                                                             [mem-registry (trace-container->mem-registry tc)])
                                                (force-lrb (trace->score (second tc)))))
         (define (trace-container->erps tc)  (parameterize ([xrp-registry (trace-container->xrp-registry tc)]
                                                            [mem-registry (trace-container->mem-registry tc)])
                                                (force-lrb (get-dominated-erps (second tc)))))
         
         
         ;;things only used in church-eval functions:
         (define (trace-container->trace tc) (second tc))
         (define (trace-container->xrp-registry tc) (third tc))
         (define (trace-container->mem-registry tc) (fourth tc))

         
;;          ;;returns the gradient of the score wrt any real-valued erps in the trace.
;;          ;;once this is computed, it is stored (so we only do that work once).
;;          ;;FIXME: this causes an extra complete eval, since the scoring function is separated from trace-update. eventually want to integrate AD with trace-update.
;;          (define (trace-container->gradient tc)
;;            (if (mcar (fifth tc))
;;                (mcar (fifth tc))
;;                (parameterize ([xrp-registry (trace-container->xrp-registry tc)]
;;                               [mem-registry (trace-container->mem-registry tc)])
;;                  (let-values (((real-world discrete-world) (partition (lambda (erp-entry) (continuous? (rest erp-entry))) (trace-container->erps tc))))
;;                    (let* ((real-world-names (map first real-world))
;;                           (real-world-vals (map rest real-world))
;;                           (scoring-fn (lambda (real-world-vals)
;;                                         (parameterize ([*fixed-erps* (append (map pair real-world-names real-world-vals) discrete-world)])
;;                                           (let-values (([new-tc fw-score bw-score] (church-eval (get-syntax (trace-container->trace tc))
;;                                                                                                 (get-env (trace-container->trace tc)))))
;;                                             (trace-container->score new-tc))))))
;;                      (set-mcar! (fifth tc) (map pair real-world-names ((gradient-list-R scoring-fn) real-world-vals)))
;;                      (mcar (fifth tc)))))))

;;           ;;for detecting continuous erps...
;;          (define (continuous? x) (and (rnrs-real? x) (not (fixnum? x))))
         
         )