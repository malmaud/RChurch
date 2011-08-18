#!r6rs

;; created on Jun 17, 2008 by NDG
;; authors: noah goodman, daniel roy


(library 
 (church mcmc mcmc-core)
 
 (export convert-lex 
         mcmc-query-core
         repeated-mcmc-query-core
         trace-finish 
         default-initialize 
         repeat-kernel 
         cycle-kernel 
         make-mh-kernel
         *MH-acceptance-fn*
         
         make-trace-walk-proposal
         trace-score-proc
         
         make-move
         move-fp
         move-rp
         move-state
         
         mcmc-lex-query
         mcmc-query
         mcmc-def-query
         *mh-steps*
         *mh-statistics*
         *show-syntax*
         update-proposal-statistics!
         get-proposal-statistics
         num-accepted
         num-proposals
         display-proposal-statistics
         reset-proposal-statistics!
         
         mcmc-tc->query-val
         mcmc-tc->cond-val)
 
 (import (church utils rnrs)
         (_srfi :1)
         (_srfi :43)
         (_srfi :69)
         ;(church church-eval traces)
         (church church-eval logmath)
         (church church-eval syntax)
         (church constraint-propagation constraints)
         ;(church church-eval laziness)
         (church church-eval church-eval)
         (church readable-scheme)
         (church constraint-propagation trace-constraint-propagation)
         (church utils utils)
         (church utils AD)
         )
 
 
 (define *mh-steps* (make-parameter #f))
 (define *mh-statistics* (make-parameter #f))
 (define *show-syntax* (make-parameter #f))

 ;;take-sample? returns false if we should keep going, or a non-false value if we should take a sample at this point.
 (define (repeated-mcmc-query-core initialize kernel num-samples take-sample? sample-value normal-form-query-exp env)
   (define count 0)
   (define samples '())
   (let* ((normal-form-query-exp (begin (when (> (*verbosity*) 9) (display "  Beginning syntactic analysis...\n"))
                                        (sexpr->syntax normal-form-query-exp env))) )
     (when (*show-syntax*) (display "\nsyntax:  ") (write-with-syntax-ids normal-form-query-exp) (display "\n\n"))
     (when (> (*verbosity*) 9) (display "  Beginning MCMC...\n"))
     (let mcmc-loop ((state (kernel (initialize normal-form-query-exp env) env)))
       (if (>= count num-samples)
           (reverse samples)
           (begin
             (when (take-sample? state)
               (set! count (+ count 1))
               (set! samples (pair (sample-value state) samples))
               (when (> (*verbosity*) 11) (display "   Sample ")(display count)(newline)))
             (mcmc-loop (kernel state env)) )))))


;;;deprecated....
 (define (mcmc-query initialize kernel stop? finish query-exp filter-exp env)
   (let* ((normal-form-query-exp `((lambda (q-e) (pair c-e (,filter-exp c-e))) ,query-exp) ))
     (mcmc-query-core initialize kernel stop? finish normal-form-query-exp env) ))
  (define (mcmc-lex-query initialize kernel stop? finish lex-exp query-exp cond-exp env)
   (let* ((normal-form-query-exp (convert-lex lex-exp query-exp cond-exp)))
     (mcmc-query-core initialize kernel stop? finish normal-form-query-exp env) ))
  (define (mcmc-query-core initialize kernel stop? finish normal-form-query-exp env)
   (let* ((normal-form-query-exp (begin (when (> (*verbosity*) 9) (display "  Beginning syntactic analysis...\n"))
                                        (sexpr->syntax normal-form-query-exp env))) )
     (when (> (*verbosity*) 9) (display "  Beginning MCMC...\n"))
     (finish (let mcmc-loop ((state (initialize normal-form-query-exp env)))
               (if (stop? state)
                   state
                   (mcmc-loop (kernel state env)) )))))
  (define (mcmc-def-query initialize kernel num-samples take-sample? sample-value defs query-exp cond-exp env)
   (let* ((normal-form-query-exp `(begin ,@defs (pair ,query-exp ,cond-exp)) ))
     (repeated-mcmc-query-core initialize kernel num-samples take-sample? sample-value normal-form-query-exp env) ))
 (define (convert-lex  lex-exp query-exp cond-exp)
     (lex->lambdas lex-exp `(pair ,query-exp ,cond-exp))) 
 (define (lex->lambdas lex-exp inner-exp)
   (if (null? lex-exp) inner-exp
       (let ((vars (list (first (first lex-exp))))
             (val-exp (second (first lex-exp))))
         (list (list 'lambda vars (lex->lambdas (rest lex-exp) inner-exp)) val-exp))))
;;;
 
;;fns for looking at c-e and q-e values.
;; assumes mcmc expressions will return a pair (q-e-val . c-e-val).
 (define (mcmc-tc->query-val tc) (first (trace-container->value tc)))
 (define (mcmc-tc->cond-val tc) (rest (trace-container->value tc)))

 (define (trace-finish tc) (mcmc-tc->query-val tc))
 (define (trace-score-proc trace-container)
   (if (and (not (eq? (trace-container->value trace-container) 'zero-probability-proposal-exception))
            (eq? true (mcmc-tc->cond-val trace-container)))
       (trace-container->score trace-container)
       LOG-PROB-0 ))

;;;init
 ;;initialize to a trace with non-zero probability and return value true, 
 ;;this version samples, tries to set the root value to true by constraint propagation, and re-samples on failure.
 ;;should be much faster than rejection for query predicates with noise "near the bottom".
 (define (smart-initialize exp env)
   (when (*global-debug*) (display "Initializing MH chain (by smart propagation/rejection)..\n"))
   (let*-values ([(trace-container forw rev) (church-eval exp env)]
                 [(init-trace-container) (force-to-value trace-container (pair *wildcard-value* true) env)])
     (if (and (not (eq? init-trace-container 'fail)) 
              ;(eq? true (trace-container->value init-trace-container)) 
              (not (= LOG-PROB-0 (trace-container->score init-trace-container))))
         init-trace-container
         (smart-initialize exp env) )))
 
 (define initialize smart-initialize)
 (define default-initialize smart-initialize)
 

;;; various kernels and kernel combinators
 
 ;; mixture kernel
 (define (mixture-kernel cdf . kernels ) 
   (lambda (state env)
     (define u (random-real))
     (let loop ((kernels kernels)
                (cdf cdf) )
       (if (<= u (car cdf))
           ((car kernels) state env)
           (loop (cdr kernels)
                 (cdr cdf) )))))
 
 ;; cycle kernel
 (define (cycle-kernel . kernels)
   (lambda (state env)
     (let loop ((kernels kernels)
                (state state))
       (if (null? kernels)
           state
           (loop (rest kernels)
                 ((first kernels) state env) )))))
 
 ;; repeat a kernel
 (define (repeat-kernel steps kernel)
   (lambda (state env)
     (let loop ((steps steps)
                (state state) )
       (if (<= steps 0)
           state
           (loop (- steps 1) (kernel state env)) ))))

  (define (interleaved-kernel . kernels)
   (define which -1)
   (define num-kernels (length kernels))
   (define kernel-vector (list->vector kernels))
   (lambda (state env)
     (set! which (modulo (+ which 1) num-kernels))
     ((vector-ref kernel-vector which) state env) ))

  ;;basic MH kernel
 (define (ADmin a b) (if (< a b) a b)) ;;min that plays nice with AD (should provide lifted min in AD.ss?)
 ;;make acceptance fn a parameter, so that it can be overidden to get greedy search mode.
 (define *MH-acceptance-fn* (make-parameter (lambda (new-prob old-prob fw-prob bw-prob)
                                              (ADmin LOG-PROB-1 (- (+ new-prob bw-prob) (+ old-prob fw-prob))))))
 (define (make-mh-kernel proposal score-proc kernel-name) 
   (lambda (state env)
     (let* ((old-p (score-proc state))
            (move (proposal state env))
            (fp (move-fp move))
            (rp (move-rp move))
            (new-state (move-state move))
            (new-p (score-proc new-state))
            (acceptance-prob ((*MH-acceptance-fn*) new-p old-p fp rp))
            (accept (log-flip acceptance-prob)) )
       
       ; update proposal acceptance statistics
       (when (or (*global-debug*) (*mh-steps*) (*mh-statistics*))
         (update-proposal-statistics! kernel-name accept (move-details move)))
       
       (when (or (*global-debug*) (*mh-steps*))
         (let ((kernel-statistics (get-overall-proposal-statistics kernel-name)))
           (for-each display
                     (list 
                      kernel-name "\n"
                      (if accept "  accept." "  reject.")
                      "  acceptance probability: " (untapify (exp acceptance-prob)) " = (exp " (untapify acceptance-prob) ") \n"
                      "       = new-p rp / old-p fp = " (untapify new-p) " " (untapify rp) " / " (untapify old-p) " " (untapify fp) "\n" 
                      "  accepted: " (first kernel-statistics) "/" (second kernel-statistics)
                      "\n\n\n"))))
       
       (if accept new-state state)
       )))

 ;;MH with delayed rejection
  (define (make-delayed-rejection-mh-kernel proposal score-proc delay-prob kernel-name) 
   (lambda (state env)
     (let* ((old-p (score-proc state))
            (move (proposal state env))
            (fp (move-fp move))
            (rp (move-rp move))
            (new-state (move-state move))
            (new-p (score-proc new-state))
            (acceptance-prob ((*MH-acceptance-fn*) new-p old-p fp rp))
            (accept (log-flip acceptance-prob)) )
       
       ; update proposal acceptance statistics
       (when (or (*global-debug*) (*mh-steps*) (*mh-statistics*))
         (update-proposal-statistics! kernel-name accept (move-details move)))
       
       (when (or (*global-debug*) (*mh-steps*))
         (let ((kernel-statistics (get-overall-proposal-statistics kernel-name)))
           (for-each display
                     (list 
                      kernel-name "\n"
                      (if accept "  accept." "  reject.")
                      "  acceptance probability: " (untapify (exp acceptance-prob)) " = (exp " (untapify acceptance-prob) ") \n"
                      "       = new-p rp / old-p fp = " (untapify new-p) " " (untapify rp) " / " (untapify old-p) " " (untapify fp) "\n" 
                      "  accepted: " (first kernel-statistics) "/" (second kernel-statistics)
                      "\n\n\n"))))
       
       (if accept
           new-state
           (if (flip delay-prob)
               ;;delay the rejection and try again... apply the proposal to the proposed state, accumulate the fw/bw with additional terms for acceotance prob and delay prob.
               state;;reject back to initial state.
           ))
       )))

  
;;; things for MH kernels
  ;;FIXME: change to record.
 (define (make-move forward-probability backward-probability new-state . move-details)
   (list forward-probability backward-probability new-state move-details) )
 (define (move-fp move) (list-ref move 0))
 (define (move-rp move) (list-ref move 1))
 (define (move-state move) (list-ref move 2))
 (define (move-details move) (list-ref move 3))


;;;trace-walk-proposal
 (define (make-trace-walk-proposal . erp-filter) ;;erp-filter allows you to specify proposals to only a subset of erps...
   (let ((erp-filter (if (null? erp-filter) (lambda (x) x) (first erp-filter))))
   (lambda (trace-container env)
     (when (or (*global-debug*) (*mh-steps*)) (display "trace-walk-proposal..\n"))
     (let ((erps (filter erp-filter (trace-container->erps trace-container))))
       (if (null? erps)
           (make-move LOG-PROB-1 LOG-PROB-1 trace-container "null-erps")
           (let*-values (;[ (erps) (filter erp-filter (trace-container->erps trace-container))]
                         [ (chosen-erps) (choose-erps-to-change erps)]
                                        ;( (dummy) (for-each display (list "chosen erp: "  chosen-erps "\n")))
                         [ (forw-erp-choice-prob) (prob-of-erp-choice chosen-erps erps)]
                         [ (new-trace-container forw-prob rev-prob) (church-eval trace-container env 'propose chosen-erps)]; 'force-complete-eval true)]
                         [ (rev-erp-choice-prob) (prob-of-erp-choice chosen-erps (filter erp-filter (trace-container->erps new-trace-container)))]
                         )
       
       (when (*global-debug*)
         (for-each display (list "current target-val: " (mcmc-tc->query-val trace-container) ", "
                                 "proposal target-val: " (if (eq? (trace-container->value new-trace-container) 'zero-probability-proposal-exception)
                                                             "zero-probability-proposal-exception"
                                                             (mcmc-tc->query-val new-trace-container))
                                 ", "
                                 "proposal conditioner value: " (mcmc-tc->cond-val new-trace-container) "\n" )))
       (when (or (*global-debug*) (*mh-steps*))
         (for-each display (list "     rev-prob: "  (untapify rev-prob)
                                 " rev-erp-choice-prob: " (untapify rev-erp-choice-prob)
                                 " proposal score: " (untapify (trace-container->score new-trace-container)) "\n"
                                 "     forw-prob: "  (untapify forw-prob)
                                 " forw-erp-choice-prob: " (untapify forw-erp-choice-prob)
                                 " old score: " (untapify (trace-container->score trace-container)) "\n \n")))

       (make-move (+ forw-prob forw-erp-choice-prob)
                  (+ rev-prob rev-erp-choice-prob)
                  new-trace-container
                  (map first chosen-erps))))))))

 (define (choose-erps-to-change dominated-erps)
   (list (uniform-draw dominated-erps))) ;;for now just chose one erp (or none iff there are no erps in trace to chose from).
 
 ;; FIXME: choose-erps should return the associated probability
 (define (prob-of-erp-choice erps dominated-erps)
   (if (null? dominated-erps) 
       LOG-PROB-1 ;;on zero-probability-proposal-exception the dominated-erps can be null without the erps being null....
       (- (* (length erps) (log (length dominated-erps)))) )) ;; for now assume erps are chosen uniformly at random *with* replacement.
 
;;  (define (anneal score-proc temp)
;;    (lambda (state)
;;      (/ (score-proc state) temp) ))

 
;;;stuff for tracking proposal statistics
 ;; hash table for proposal acceptance rates
 (define proposal-table (make-hash-table))
 (define (num-accepted statistics move-details) (first (hash-table-ref statistics move-details (lambda ()
                                                                                                 (hash-table-set! statistics move-details (list 0 0))
                                                                                                 (hash-table-ref statistics move-details)))))
 (define (num-proposals statistics move-details) (second (hash-table-ref statistics move-details (lambda ()
                                                                                                 (hash-table-set! statistics move-details (list 0 0))
                                                                                                 (hash-table-ref statistics move-details)))))
 ;(define (update-statistics num-proposals num-accepted move-details old-statistics)
 ;  (hash-table-set! old-statistics move-details (list num-proposals num-accepted)))
 
 (define (get-proposal-statistics kernel-name)
   (hash-table-ref proposal-table
                   kernel-name
                   (lambda ()
                     (hash-table-set! proposal-table kernel-name (make-hash-table))
                     (hash-table-ref proposal-table kernel-name))))
                   ;(lambda () (list 0 0))))

  (define (get-overall-proposal-statistics kernel-name)
    (let ((stats (hash-table-values (get-proposal-statistics kernel-name))))
      (list (apply + (map first stats)) (apply + (map second stats)))))
 
 (define (reset-proposal-statistics!)
   (set! proposal-table (make-hash-table)))
 
 (define (update-proposal-statistics! kernel-name accept move-details)
   (let* ((statistics (get-proposal-statistics kernel-name))
          (new-num-proposals (+ (num-proposals statistics move-details) 1))
          (new-num-accepted (if accept (+ (num-accepted statistics move-details) 1) (num-accepted statistics move-details))))
     (hash-table-set! statistics move-details (list new-num-accepted new-num-proposals))))
     ;(hash-table-set! proposal-table kernel-name (make-statistics new-num-accepted new-num-proposals move-details statistics))))
 
 (define (display-proposal-statistics-row kernel-name statistics name-col-length)
   (display-justified kernel-name name-col-length)
   (let ((overall-stats (get-overall-proposal-statistics kernel-name)))
     (for-each display
               (list
                (first overall-stats) "/" (second overall-stats)
                " (" (exact->inexact (/ (first overall-stats) (second overall-stats))) ") \n")))
   (for-each (lambda (detail)
               (display-justified " " name-col-length)
               (for-each display
                         (list
                          "  " detail ":  "
                          (num-accepted statistics detail) "/" (num-proposals statistics detail)
                          " (" (exact->inexact (/ (num-accepted statistics detail) (num-proposals statistics detail))) ") \n")))
             (hash-table-keys statistics)))
 
 (define (display-proposal-statistics)
   (when (> (length (hash-table-keys proposal-table)) 0)
     (begin
       (display "\nProposal acceptance statistics:\n")
       (let ((name-col-length (+ (apply max (map string-length (hash-table-keys proposal-table))) 5)))
         (hash-table-walk proposal-table (lambda (n s) (display-proposal-statistics-row n s name-col-length)))))))


 
 
 (when (> (*verbosity*) 12)
   (display "loaded mcmc-core.ss\n"))
 
 )