#!r6rs

;;written by NDG on Jun 17, 2008
;;based (long ago) on SICP eval code.

;;this file contains the core functions needed for church's trace updater, which enables MCMC.

(library (church church-eval trace-update)
         
         (export trace-update *trace-update-debug* *church-eval-exit-on-exception* *update-params* release-queue)
         
         (import (church utils rnrs)
                 (_srfi :1) ; lists
                 (_srfi :69) ; hash tables
                 (church readable-scheme)
                 (church constraint-propagation constraints)
                 (church church-eval environments-lexical)
                 (church church-eval traces)
                 (church church-eval logmath)
                 (church church-eval laziness)
                 (church church-eval syntax)
                 (church church-eval env-equiv)
                 (church church-eval trace-eval)
                 (church church-eval trace-update-parameters)
                 (church utils utils))
         
         (define *trace-update-debug* (make-parameter #f))
         
         (define *church-eval-exit-on-exception* (make-parameter #f))
         
         (define *update-params* (make-parameter #f))
         
         (define env-equiv-registry (make-parameter (make-hash-table)))
         
         ;;;;;;;;;;;;;;;;   trace-update
         ;;;trace-update takes a trace and walks over it re-evaluating as necessary. returns updated trace.
         (define (trace-update trace env stack-address)
           (let ((stack-address (pair (syntax->id (get-syntax trace)) stack-address)))
           
           (assert-with-message (lambda () (is-eval-node? trace)) (lambda () (error "Tried to trace-update non eval-node: " trace)))
           
           (when (and (*global-debug*) (*trace-update-debug*))
             (display "Trace-update for node with expression: ")
             (write (get-original-expr trace))
             (newline)
             (display "   at stack-address: ")(display stack-address)(newline)
                 ;(display (force-lrb (get-dominated-erps trace))) (newline)
                 ;(display (erps-to-propose (*update-params*))) (newline)
                 ;(display (map hash-table-keys (hash-table-values (mem-registry))) )  (newline)
                 ;(when (and (not (null? (force-lrb (get-dominated-erps trace)))) (not (null? (erp-to-propose (*update-params*))))
                 ;           (equal? (first (erp-to-propose (*update-params*))) (first (first (force-lrb (get-dominated-erps trace)))))
                 ;           (not (equal? (rest (erp-to-propose (*update-params*))) (rest (first (force-lrb (get-dominated-erps trace))))))
                 ;           )
                 ;  (error "out of sync"))
             (when (not (need-to-re-eval? trace env stack-address))
               (display "   Not re-evaling this node! \n")))
           
           (with-exception-handler 
            (lambda (exn) (begin (display "\nException within trace-update on expression: ")
                                 (write (get-original-expr trace))
                                 (newline)
                                 (raise-continuable exn) ))
            
            (lambda ()
              (if (need-to-re-eval? trace env stack-address)
                    ;;FIXME: cache dispatch fn?
                    (let* ((syntax (get-syntax trace))
                           (dispatch-fn
                            (cond ((syntax:begin? syntax) eval-begin)
                                  ((syntax:self-evaluating? syntax) eval-self-evaluating) 
                                  ((syntax:variable? syntax) eval-variable-lookup)
                                  ((syntax:quoted? syntax) eval-quoted)
                                  ((syntax:lambda? syntax) eval-lambda)
                                  ((syntax:get-env? syntax) eval-get-env)
                                  ((syntax:definition? syntax) eval-update-define)
                                  ((syntax:if? syntax) eval-if)
                                  ((syntax:application? syntax) eval-application)
                                  (else
                                   (error "Unknown expression type - EVAL" syntax)))))
                      
                      (let-values (( (value subtraces forw-prob back-prob . dominated-erps) (dispatch-fn trace env stack-address) ))
                        (make-proposal (if (null? dominated-erps)
                                           (update-eval-node trace syntax env value subtraces)
                                           (update-eval-node trace syntax env value subtraces (first dominated-erps)))
                                       (make-forward/backward-score forw-prob back-prob))))
                    
                    (make-proposal trace (make-forward/backward-score LOG-PROB-1 LOG-PROB-1))
                    )
                ))
           )
           )
         
         
         ;;re-eval can be avoided when (relevant part of) environment hasn't changed, nothing in trace is marked for proposal.
         ;;TODO: need to cache env-equiv functions in trace for speed?
         (define (need-to-re-eval? trace env stack-address)
           (or ;;this forces re-evaluation when a complete refresh is requested:
               (force-complete-eval? (*update-params*))
               ;;this re-evaluats if anything relevant has changed in the environment:
               (not ((syntax->env-equiv (get-syntax trace) (env-equiv-registry)) env (get-env trace)))
               ;;this forces re-evaluating if a proposal is requested below this node:
               (dominates-proposal-node? stack-address)
               ))
         
         ;;we force the dominated-erps allowing cached values to be used. 
         ;;this should speed up programs that use mem, but must make sure to force-lrb-fresh in church-eval....
         ;;FIXME: speed....
         (define (dominates-proposal-node? stack-address)
           (define (prefix? proposal-address)
             (let* ((proposal-depth (length proposal-address))
                    (stack-depth (length stack-address))
                    (check-depth (min proposal-depth stack-depth)))
               (equal? (take-right stack-address check-depth) (take-right proposal-address check-depth))))
           (fold (lambda (erp accum) (or (prefix? (first erp)) accum)) false (erps-to-propose (*update-params*))))
         
         ;;;;;here are the definitions of the dispatch functions that do the work:
         
         (define (eval-self-evaluating trace env stack-address)
           (values (get-expr trace) '() LOG-PROB-1 LOG-PROB-1))
         
         (define (eval-quoted trace env stack-address)
           (values (quote-syntax->text-of-quotation (get-syntax trace)) '() LOG-PROB-1 LOG-PROB-1))
         
         (define (eval-lambda trace env stack-address)
           (let ((syntax (get-syntax trace)))        
             (values (make-procedure (lambda-syntax->lambda-parameters syntax)
                                     (lambda-syntax->lambda-body syntax)
                                     env)
                     '() 
                     LOG-PROB-1 LOG-PROB-1)))
         
         (define (eval-get-env trace env stack-address)
           (values env '() LOG-PROB-1 LOG-PROB-1))
         
         ;if node: subtraces are ordered: (0)predicate, (1)branch actually followed (consequent or alternative).
         (define (eval-if trace env stack-address) 
           (let* ((syntax (get-syntax trace))
                  (subtraces (get-subtraces trace)) 
                  (pred-trace (first subtraces))
                  (new-pred-proposal (trace-update pred-trace env stack-address)))
             (if (equal? (trace->value pred-trace)
                         (trace->value (proposal->trace new-pred-proposal)) )
                 ;;same branch, just update:
                 (let* ((new-branch-proposal (trace-update (second subtraces) env stack-address))
                        (sub-evals (list new-pred-proposal new-branch-proposal))
                        (new-subtraces (extract-traces sub-evals)))
                   (values (trace->value (proposal->trace new-branch-proposal))
                           new-subtraces 
                           (eager-sum (extract-forward-probs sub-evals))
                           (eager-sum (extract-reverse-probs sub-evals)) ))
                 ;;switch branches (generate new branch from scratch):
                 (let* ((additional-rev-prob (trace-delete (second subtraces)))
                        (branch-syntax (if (true? (trace->value (proposal->trace new-pred-proposal)))
                                           (if-syntax->if-consequent syntax)
                                           (if-syntax->if-alternative syntax)))
                        (new-branch-proposal (trace-eval branch-syntax env stack-address))
                        (sub-evals (list new-pred-proposal new-branch-proposal))
                        (new-subtraces (extract-traces sub-evals)))
                   (values (trace->value (proposal->trace new-branch-proposal))
                           new-subtraces
                           (eager-sum (extract-forward-probs sub-evals))
                           (eager-sum (cons additional-rev-prob (extract-reverse-probs sub-evals))) )
                   ) )))
         
         
         
         ;For begin update everything keeping traces. value is value of last expr.
         ;;FIXME: mutual recursions will trigger unecessary updates, since the second defined var will lok changed when the first is updated....
         ;;  note: before we can update the subtraces, we have to re-extend the env with the appropriate values for the defines.
         (define (eval-begin trace env stack-address)
           (let* ((subtraces (get-subtraces trace))
                  (defined-vars (first (syntax->details (get-syntax trace)))) ;;note: syntax details for begin is a list of defined vars.
                  (env (extend-environment defined-vars defined-vars env))
                  (dummy (for-each (lambda (subtr)
                                     (when (syntax:definition? (get-syntax subtr))
                                       (define-variable!
                                         (definition-syntax->definition-variable (get-syntax subtr))
                                         (trace->value subtr)
                                         env)))
                                   subtraces))
                  (subevals (map-in-order (lambda (t) (trace-update t env stack-address)) subtraces))
                  )
             (values (trace->value (proposal->trace (last subevals))) ;(trace->value new-relevant-trace)
                     (map proposal->trace subevals)
                     (eager-sum (extract-forward-probs subevals))
                     (eager-sum (extract-reverse-probs subevals))) ))
         
         ;application node: subtraces are: (0) application, (1) operator, (2..) operands
         (define (eval-application trace env stack-address)
           (let* ((subtraces (get-subtraces trace))
                  (new-operator-proposal (trace-update (second subtraces) env stack-address))
                  (new-operand-proposals (map (lambda (tr) (trace-update tr env stack-address)) (rest (rest subtraces)) ))
                  (new-appl-trace (trace-apply-update (trace->value (proposal->trace new-operator-proposal))
                                                      (map (compose trace->value proposal->trace) new-operand-proposals)
                                                      (first subtraces) 
                                                      stack-address))
                  (sub-evals (pair new-appl-trace (pair new-operator-proposal new-operand-proposals))))
             (cond
                 ((no-proposals? (get-syntax trace))
                  (values (trace->value (proposal->trace new-appl-trace))
                          (extract-traces sub-evals)
                          (eager-sum (extract-forward-probs sub-evals))
                          (eager-sum (extract-reverse-probs sub-evals))
                          (list)))
                ; ((double-down? (get-syntax trace)) ....)
                 (else (values (trace->value (proposal->trace new-appl-trace))
                         (extract-traces sub-evals)
                         (eager-sum (extract-forward-probs sub-evals))
                         (eager-sum (extract-reverse-probs sub-evals)))))))
         
         (define (no-proposals? syntax)
           (and (syntax:application? syntax) (eq? (first (syntax->original-expr syntax)) 'no-proposals)))
  
         
         (define (eval-variable-lookup trace env stack-address) 
           (let* ((lexical-address (first (syntax->details (get-syntax trace))))
                  (value (head (lookup-value-by-id lexical-address env))) )
             (values value '() LOG-PROB-1 LOG-PROB-1) ))
         
         
         ;;extend the environment -- allows recursive definitions. 
         (define (eval-update-define trace env stack-address)
           (let* ((syntax (get-syntax trace))
                  (defn-val-subtrace (first (get-subtraces trace)))
                  (defn-val-proposal (trace-update defn-val-subtrace env stack-address)) )
             
             (define-variable!
               (definition-syntax->definition-variable syntax)
               (trace->value (proposal->trace defn-val-proposal))
               env)
             
             (values (trace->value (proposal->trace defn-val-proposal));(void) ;env-to-extend
                     (list (proposal->trace defn-val-proposal))
                     (eager-sum (extract-forward-probs (list defn-val-proposal)))
                     (eager-sum (extract-reverse-probs (list defn-val-proposal)))
                                        ;LOG-PROB-1 LOG-PROB-1
                     )))    
         
         ;;;;;;;;;;;;;   trace-apply
         ;;application returns a subtrace whos root is an application node (and the forward/reverse probs). 
         ;if the operator and operands are already loaded into an initial application node, then re-application is conservative.
         (define (trace-apply-update operator operands trace stack-address)
           
           (assert-with-message (lambda () (is-apply-node? trace)) (lambda () (error "Tried to trace-apply non apply-node: " trace)))
           (assert-with-message (lambda () (valid-operator? operator)) 
                                (lambda () (error "Tried to trace-apply to invalid operator: OPERATOR: " operator "\n"
                                                  "  OPERANDS: " operands "\n"
                                                  "  OLD TRACE: " trace "\n")))
           
           (if (op-equal? operator (get-operator trace))
               ;operator the same -- re-apply conservatively:
               (let ((dispatch-fn 
                      (cond 
                        ((mem? operator) update-apply-mem)
                        ((memoized-procedure? operator) update-apply-memoized-procedure)
                        ((eval-or-apply? operator) apply-trace-producing-primitive)
                        ((primitive-procedure? operator) apply-primitive-procedure)
                        ((elementary-random-procedure? operator) apply-erp)
                        ((make-xrp? operator) update-apply-make-xrp)
                        ((xrp? operator) update-apply-xrp)
                        ((compound-procedure? operator) apply-compound-procedure)
                        (else (error "Unknown procedure type -- APPLY" "OPERATOR" operator "OPERAND: " operands "TRACE: " trace)))))
   ;;                (when (and (*global-debug*) (*trace-update-debug*))   ;this is useful for debugging:
;;                      (for-each display (list "\noperator un-changed re-applying conservatively\n")))
                 (let-values (( (value score subtraces forw-prob back-prob . dominated-erps) (dispatch-fn operator operands trace stack-address)))
                   (make-proposal (if (null? dominated-erps)
                                      (update-apply-node trace operator operands value score subtraces)
                                      (update-apply-node trace operator operands value score subtraces (first dominated-erps)))
                                  (make-forward/backward-score forw-prob back-prob))))
               
               ;operator has changed -- re-apply from scratch:
               (let ((additional-rev-prob (trace-delete trace))
                     (sub-evals (list (trace-apply operator operands stack-address *wildcard-constraint-set*))))
                 
                 (when (and (*global-debug*) (*trace-update-debug*))   ;this is useful for debugging:
                   (let ((old-operator (get-operator trace)))
                     (for-each display
                               (list "\noperator change triggered complete re-apply, new-op " (if (pair? operator) (first operator) operator) ": "
                                     (cond ((compound-procedure? operator) (procedure-body operator))
                                           ((elementary-random-procedure? operator) operands)
                                           ((xrp? operator) (xrp->hyperparams operator))
                                           (else "."))
                                     "\n   old-op " (if (pair? old-operator) (first old-operator) old-operator) ": "
                                     (cond ((compound-procedure? old-operator) (procedure-body old-operator))
                                           ((elementary-random-procedure? old-operator) (get-operands trace))
                                           ((xrp? operator) (xrp->hyperparams operator))
                                           (else "."))                                  
                                     "\n")
                               )))
                 
                 (make-proposal (first (extract-traces sub-evals))
                                (make-forward/backward-score (eager-sum (extract-forward-probs sub-evals))
                                                             (eager-sum (cons additional-rev-prob (extract-reverse-probs sub-evals))))))
               ))
         
         ;;this function defines operator equality for purposes of re-application, it is more fine grained than equal?, in order to avoid unecessary re-application.
         ;;we reflect into operator in case of compound or memoized procedure to avoid complete re-evaluation when only environments etc change.
         ;;if treat-erps-badly is turned on we call any two erps equal -- thus when an erp operator is changed the new operator is forced to have the return value of the old operator, rather than sampling afresh.
         (define treat-erps-badly true)
         (define (op-equal? op1 op2)
           (cond
             ((and (compound-procedure? op1) (compound-procedure? op2)) 
              (equal? (procedure-body op1) (procedure-body op2)))
             ((and (memoized-procedure? op1) (memoized-procedure? op2)) 
              (mem-op-equal? op1 op2))
             ((and treat-erps-badly (elementary-random-procedure? op1) (elementary-random-procedure? op2))
              true)
             ((and (xrp? op1) (xrp? op2) (eq? (xrp->name op1) (xrp->name op2))) ;;two xrps with same type are treated as same for apply-update.
              true)
             (else (equal? op1 op2))))
         
         ;equality for memoized operators (used in trace-apply-update) depends on the underlying procedures, not the memoized contents.
         (define (mem-op-equal? op1 op2) 
           (op-equal? (memproc->underlying-proc op1) (memproc->underlying-proc op2)))
         
         
         (define (update-apply-mem operator operands trace stack-address)
           (let* ((memproc (trace->value trace))
                  (proposals (map (lambda (subt) (trace-apply-update (first operands)
                                                                     (get-operands subt)
                                                                     subt 
                                                                     (pair (get-operands subt) (memproc->stack-address memproc))  )) ;FIXME: should try to detect if re-apply isn't needed.
                                  (memproc->subtraces memproc)))
                  (new-subtraces (map proposal->trace proposals))
                  ;;FIXME: we make a new memproc only so that env-equiv will re-evaluate (in case subtrace values have changed)...
                  ;(new-memproc memproc)
                  (new-memproc (list 'memoized-procedure (memproc->key memproc) (memproc->underlying-proc memproc) stack-address (gensym) ))
                  )
             (update-memproc-subtraces new-memproc new-subtraces)
             (values new-memproc
                     (delay-sum (map trace->score (memproc->subtraces new-memproc)))
                     '()
                     (eager-sum (map (compose forward/backward-score->forward-score proposal->forward/backward-score) proposals))
                     (eager-sum (map (compose forward/backward-score->backward-score proposal->forward/backward-score) proposals))
                     (delay-append (map get-dominated-erps (memproc->subtraces new-memproc))))))
         
           
         (define (update-apply-memoized-procedure memproc operands trace stack-address)
           (if (and (equal? operands (get-operands trace)) (eq? (memproc->key memproc) (memproc->key (get-operator trace))))
               
               ;;operands and memproc unchanged, just get the value:
               (values (trace->value (memproc->subtrace memproc operands))
                       LOG-PROB-1 '() LOG-PROB-1 LOG-PROB-1)
               
               ;;otherwise, decrement counts from old memproc, then apply new memproc to new operands:
               (let-values ([ (extra-rev) (decrement-memproc-counts (get-operator trace) (get-operands trace))]
                            [ (val score subtr fw rev . erps)  (apply-memoized-procedure memproc operands stack-address *wildcard-constraint-set*)] )
                 (if (null? erps)
                     (values val score subtr fw (eager-sum (list extra-rev rev)))
                     (values val score subtr fw (eager-sum (list extra-rev rev)) (first erps))
                     ))))
         
         
         
         ;;there are some primitives (eval, apply) that take a trace, do random stuff, and return a trace as their value -- we want to treat this as the subtrace.
         (define (apply-trace-producing-primitive operator operands trace stack-address)
           (let* ((subtraces (get-subtraces trace))
                  (additional-rev-prob (if (and (eq? operator 'eval) (not (equal? (first operands) (get-original-expr (first subtraces)))))
                                           (trace-delete (first subtraces)) ;(trace->score (first subtraces))
                                           LOG-PROB-1 ))
                  (sub-evals (list (cond 
                                     ((eq? operator 'eval) 
                                      (if (equal? (first operands) (get-original-expr (first subtraces))) ;;FIXME: is this the right equality check?
                                          (trace-update (first subtraces) (second operands) stack-address)
                                          (trace-eval (sexpr->syntax (first operands) (second operands)) (second operands) stack-address))) ;;FIXME: syntax ids change so stack-address gets out of synch...
                                     ((eq? operator 'apply) (trace-apply-update (first operands) (second operands) (first subtraces) stack-address))
                                     (else (error "Unknown trace-producing primitive" operator)))))
                  (new-subtrace (extract-traces sub-evals)))
                          
             (values (trace->value (first new-subtrace)) 
                     (trace->score (first new-subtrace)) 
                     new-subtrace
                     (eager-sum (extract-forward-probs sub-evals))
                     (eager-sum (cons additional-rev-prob (extract-reverse-probs sub-evals))) )   ))
         
         
         ;;primitive application is called out to underlying scheme -- random choices in primitives will not be added to trace!
         (define (apply-primitive-procedure operator operands trace stack-address)
           (values (apply-in-underlying-scheme (primitive-implementation operator) operands)
                   LOG-PROB-1 ;note stochastic primitive procedures must be wrapped up as erps to compute score.
                   '()
                   LOG-PROB-1 LOG-PROB-1))
         
         ;;apply an erp:
         (define (apply-erp operator operands trace stack-address)
           (if (assoc stack-address (erps-to-propose (*update-params*)))
               ;;make a proposal -- the proposer returns a new value and forward and reverse probs. 
               (let*-values (( (fixed-erp-entry) (assoc stack-address (*fixed-erps*)) )
                             ( (proposed-val forw-prob back-prob) 
                               (if (false? fixed-erp-entry)
                                   (if (null? (erp-proposer operator))
                                       ;;use default proposer if erp doesn't have one:
                                       (unconstrained-default-proposer operator operands (trace->value trace))
                                       ;;otherwise apply proposer from erp:
                                       ((primitive-implementation (erp-proposer operator)) operator operands (trace->value trace)))
                                           ;;FIXME: allow non-primitive proposers?...
                                   (values (rest fixed-erp-entry) LOG-PROB-1 LOG-PROB-1)) )
                             ( (proposed-val) ((*erp-interpretation*) proposed-val))
                             ( (proposal-score) (apply-church-procedure (erp-scorer operator) 
                                                                        (list operands proposed-val) 
                                                                        )))
                 (when (and (*global-debug*) (*trace-update-debug*))
                   (for-each display (list "   making proposal to ERP: previous value: " (trace->value trace) 
                                           ", new value: " proposed-val ".\n")))
                  (if (= LOG-PROB-0 proposal-score)
                      (begin (when (and (*global-debug*) (*trace-update-debug*)) (display "proposal proposed zero probability event!"))
                             ((*church-eval-exit-on-exception*)
                              (make-proposal Zero-probability-proposal-exception-node 
                                             (make-forward/backward-score LOG-PROB-1 LOG-PROB-1))))
                      (values proposed-val
                              proposal-score
                              '()
                              forw-prob 
                              back-prob
                              (list (pair stack-address proposed-val)) ;(list (gensym 'erp)) ;;generate a name for the erp. any reason to want same name as before update?
                             )))
                      
               ;;otherwise, just re-compute the score, but need to catch score = -Inf error and call the error handler.
               (let ((new-score (apply-church-procedure (erp-scorer operator) 
                                                        (list operands (trace->value trace))
                                                        )))
                 (when (and (*global-debug*) (*trace-update-debug*))
                   (for-each display (list "   re-scoring but not making proposal to ERP.\n")))
                 (if (= new-score LOG-PROB-0)
                     (begin (when (and (*global-debug*) (*trace-update-debug*))
                              (display "Found zero probability event on re-scoring! Support of ERP probably changed and this might be bad!\n"))
                            ((*church-eval-exit-on-exception*)
                             (make-proposal Zero-probability-proposal-exception-node (make-forward/backward-score LOG-PROB-1 LOG-PROB-1))))
                     (values (trace->value trace)
                             new-score
                             '()
                             LOG-PROB-1 LOG-PROB-1
                             (list (pair stack-address (trace->value trace))) ;(list (gensym 'erp)) ;;generate a name for the erp. any reason to want same name as before update?
                             ) ))))
         
                  
         ;;apply a compound procedure by calling trace-eval on its body in an extended enviroment,
         ;;check to see if the values being bound in the environment extension have changed -- mark them as changed or not.
         (define (apply-compound-procedure operator operands trace stack-address)
           (let* ((old-subtraces (get-subtraces trace))
                  (old-operands (get-operands trace))
                  (binds (bindings (procedure-parameters operator) operands))
                  (binding-vars (first binds))
                  (binding-vals (rest binds))
                  
                  ;;this returns true if bindings are eq? (cause that's fast), later might have to check equal?
                  (unchanged-bindings (map eq? binding-vals 
                                           (rest (bindings (procedure-parameters operator) old-operands)))) 
                  (extended-env (extend-environment binding-vars
                                                    binding-vals
                                                    (procedure-environment operator)
                                                    unchanged-bindings 
                                                    ))
                  (sub-evals (list (trace-update (first old-subtraces) extended-env stack-address)))
                  (new-subtrace (extract-traces sub-evals)))
             
;             (when (and (*global-debug*) (*trace-update-debug*))
;               (for-each display (list "   applied compound procedure, binding vars: " binding-vars ", binding-vals: " binding-vals
;                                       ", unchanged-bindings: " unchanged-bindings "\n"
;                                       "     old binding vals were: " 
;                                       (rest (bindings (procedure-parameters operator) old-operands)) 
;                                       "\n")))
             
             (values (trace->value (first new-subtrace)) 
                     (trace->score (first new-subtrace)) 
                     new-subtrace
                     (eager-sum (extract-forward-probs sub-evals))
                     (eager-sum (extract-reverse-probs sub-evals)) )   ))
         
         
         
         
         ;;;;;update a make-xrp node.
         ;FIXME: check if sampler/unsampler/scorer/init-stats have changed.
         (define (update-apply-make-xrp operator operands trace stack-address)
           (let-values (( (xrp-name sampler unsampler scorer init hyperparams . proposer) (apply values operands)))
             (let* ((xrp-key (xrp->key (trace->value trace))) ;;key of the xrp.
                    (xrp (list 'xrp sampler unsampler scorer xrp-key
                               (if (null? proposer) '() (first proposer))
                               xrp-name
                               stack-address)))
               
               ;;if hyperparams have changed, check score to catch score= -Inf error and call the error handler.
               ;;efficient xrps will cache the work done here for re-scoring.
               (when (not (equal? (xrp->hyperparams xrp) hyperparams))
                 (update-xrp-hyperparams xrp hyperparams)
                 (when (= LOG-PROB-0 
                          (apply-church-procedure scorer (list (xrp->stats xrp) hyperparams) ))
                   (begin (when (and (*global-debug*) (*trace-update-debug*)) (display "Found zero probability event on re-scoring!"))
                          ((*church-eval-exit-on-exception*)
                           (make-proposal Zero-probability-proposal-exception-node (make-forward/backward-score LOG-PROB-1 LOG-PROB-1))))))
               
               (values xrp
                       (delay-sum (list (apply-church-procedure scorer (list (xrp->stats xrp) hyperparams))))
                       '()
                       LOG-PROB-1
                       LOG-PROB-1
                       )))) ;;make-xrp nodes don't have to dominated their xrp, because the xrp-registry is a parameter pointing to the right thing.
         
         
         ;;update an xrp apply:
         (define (update-apply-xrp operator operands trace stack-address)
           ;;first, if xrp has changed (but has same name), decrement and increment stats:
           (when (not (eq? operator (get-operator trace))) ;FIXME: do we need eq? on xrp-key?
             (let* ((old-operator (get-operator trace))
                    (value (trace->value trace)))
               ;;decrement stats from old xrp:
               (update-xrp-stats old-operator 
                                 (apply-church-procedure (xrp->unsampler old-operator)
                                                         (list value (xrp->stats old-operator) (xrp->hyperparams old-operator))
                                                         ))
               ;;increment stats onto new xrp:
               (update-xrp-stats operator 
                                 (apply-church-procedure (xrp->unsampler operator)
                                                         (list value (xrp->stats operator) (xrp->hyperparams operator) 'increment-stats)
                                                         ))

             
               ;;check that this didn't cause score = LOG-PROB-0:
                 ;;FIXME: add check.
             
               ))
           (if (assoc stack-address (erps-to-propose (*update-params*)))
               ;;make a proposal -- the proposer returns a new value and forward and reverse probs.
               (let*-values (( (fixed-erp-entry) (assoc stack-address (*fixed-erps*)) )
                             ( (proposed-val forw-prob back-prob)
                               (if (false? fixed-erp-entry)
                                   (if (null? (xrp->proposer operator))
                                       ;;use default proposer if erp doesn't have one:
                                       (default-xrp-proposer operator operands (trace->value trace))
                                       ;;otherwise apply proposer from erp:
                                       ((primitive-implementation (xrp->proposer operator)) operator operands (trace->value trace)))
                                       ;;FIXME: allow non-primitive proposers?...
                                   (values (rest fixed-erp-entry) LOG-PROB-1 LOG-PROB-1)))) ;;FIXME: does this work right for sufficient stats???
                 
                 (when (*trace-update-debug*)
                   (for-each display (list "   making proposal to XRP: previous value: " (trace->value trace) 
                                           ", new value: " proposed-val ".\n"))
                   (when (eq? proposed-val (trace->value trace)) (display "    What a stupid proposal!!\n")))
                 (values proposed-val
                         LOG-PROB-1 ;;make-xrp pays score.
                         '()
                         forw-prob 
                         back-prob
                         (list (pair stack-address proposed-val))  ;(list (pair (gensym 'xrp) (xrp->name operator))) ;;generate a name for the xrp. any reason to want same name as before update?
                         ))
                      
               ;;otherwise, we don't have to do anything (xrps don't have any parameters themselves).
               ;(make-xrp nodes will adjust score as needed if hyper-param arguments change....)
               (values (trace->value trace)
                       LOG-PROB-1 ;;make-xrp gets score.
                       '()
                       LOG-PROB-1 LOG-PROB-1
                       (list (pair stack-address (trace->value trace)))  ;(list (pair (gensym 'xrp) (xrp->name operator))) ;;generate a name for the xrp. any reason to want same name as before update?
                       )))

         
         
         ;;xrp proposers take in operator operands and old-value returns proposed-value, foreward and backward probability.
         ;NOTE: proposers are responsible for updating sufficient stats as needed, and must check support (ie. don't propose impossible value).
         ;this default proposer ignores the current value and proposes from the prior (ie. by trace-apply for the sampler).
         ;NOTE: we re-score to get conditional probs... will be very inneficient unless xrps cache work appropriately.
         (define (default-xrp-proposer operator operands old-value)
           (assert-with-message (lambda () (xrp? operator)) 
                                (lambda () (error "Attempt to apply default xrp proposal to non-xrp application.")))
           ;;init by applying the underlying procedure:
           (let* ((hyperparams (xrp->hyperparams operator))
                  (old-stats (xrp->stats operator))
                  (old-score (apply-church-procedure (xrp->scorer operator) (list old-stats hyperparams) ))
                  
                  ;;unsample, update stats, and get base score:
                  (base-stats (apply-church-procedure (xrp->unsampler operator) (list old-value old-stats hyperparams) ))
                  ;(dummy (update-xrp-stats operator base-stats))
                  ;(base-score (apply-church-procedure (xrp->scorer operator) (list base-stats hyperparams) ))
                  
                  ;;sample, update stats, and get new score:
                  (value-and-stats (apply-church-procedure (xrp->sampler operator) (list base-stats hyperparams) ))
                  (dummy (update-xrp-stats operator (second value-and-stats)))
                  (new-score (apply-church-procedure (xrp->scorer operator) (list (second value-and-stats) hyperparams) )))

             ;;forward prob is conditional prior, since we've proposed from prior. similarly, for reverse prob.
             ;;NOTE: the forward conditional is actually (new-score - base-score), and the backwards is (new-score - base-score),
             ;;  the base-scores will cancel in the acceptance probability, so we drop them here.
             (values (first value-and-stats) 
                     new-score   
                     old-score)))
         
                
         
         
;;          ;;;;;;;;;;;;;  un-trace
;;          ;;this function is used to remove a subtrace: 
;;          ;it releases trace-nodes and collects up the backward probs (prob of regenerating of what it's deleting).
;;          ;returns backward probability.
;;          ;it undraws all xrps, and removes xrp-keys from the xrp-registry when the make-xrp is removed -- even though score is paid by make-erp
;;          ;node, the backward probs are accumulated from the xrp nodes (to properly account when an xrp is deleted, but not it's make-xrp).
;;          (define release-queue (make-parameter '()))
;;          (define (trace-delete trace)
;;            (let ((score 
;;                   (cond 
;;                     ((and (is-apply-node? trace) (xrp? (get-operator trace)))
;;                      (let* ((operator (get-operator trace))
;;                             (hyperparams (xrp->hyperparams operator))
;;                             (old-stats (xrp->stats operator))
;;                             (old-score (apply-church-procedure (xrp->scorer operator) (list old-stats hyperparams) ))
                            
;;                             (new-stats (apply-church-procedure (xrp->unsampler (get-operator trace)) 
;;                                                                (list (trace->value trace) old-stats hyperparams)
;;                                                                ))
;;                             (new-score (apply-church-procedure (xrp->scorer operator) (list new-stats hyperparams) )))
;;                        (update-xrp-stats (get-operator trace) new-stats)
;;                        ;;the backward probability is the conditional prob of regenerating this sample = old-score - new-score:
;;                        (- old-score new-score)) ) ;;FIXME: can xrp scores be lazy? '-' won't work on lrbs....
;;                     ((and (is-apply-node? trace) (make-xrp? (get-operator trace)))
;;                      ;;add the xrp to the queue, in order to remove the xrp-key from the registry at end of church-eval:
;;                      (release-queue (pair (trace->value trace) (release-queue)))
;;                      LOG-PROB-1 ;;backward prob paid by xrp nodes.
;;                      )
;;                     ((and (is-apply-node? trace) (erp? (get-operator trace)))
;;                      ;;return the probability of this erp draw:
;;                      (get-score trace)
;;                      )
;;                     ((and (is-apply-node? trace) (mem? (get-operator trace)))
;;                      ;;add the mem-proc to the queue, in order to remove the key from the registry at end of church-eval:
;;                      (release-queue (pair (trace->value trace) (release-queue)))
;;                      LOG-PROB-1 ;;backward prob paid by decrementing memproc nodes.
;;                      )
;;                     ((and (is-apply-node? trace) (memoized-procedure? (get-operator trace)))
;;                      (decrement-memproc-counts (get-operator trace) (get-operands trace))
;;                      )
;;                     (else
;;                      (eager-sum (map trace-delete (get-subtraces trace))))) ))
;;              (delete-trace-node trace)
;;              score))
         
         
         
         
         (when (> (*verbosity*) 12)
           (display "loaded trace-update\n"))
         
         )