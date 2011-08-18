#!r6rs

;;written by NDG on Jun 17, 2008
;;based (long ago) on SICP eval code.

;;this file contains trace-eval, which is like scheme eval, but returns a trace of the evaluation.

(library 
 (church church-eval trace-eval)
 
 (export *trace-eval-debug*
         *fixed-erps*
         *erp-interpretation*
         *lazy*
         *adaptation*
         
         trace-eval
         trace-apply
         trace-delete
         release-queue
         
         unconstrained-default-proposer
         apply-memoized-procedure
         
         bindings
         
         apply-church-procedure
         valid-operator?
         mem?
         eval-or-apply?
         elementary-random-procedure?
         erp?
         marg
         erp-scorer
         erp-sampler
         erp-unsampler
         erp-proposer
         make-procedure
         compound-procedure?
         procedure-parameters
         procedure-body
         procedure-environment
         primitive-procedure?
         primitive-implementation
         apply-in-underlying-scheme
         
         extract-traces
         extract-forward-probs
         extract-reverse-probs
         
         xrp-registry 
         new-xrp-registry
         copy-xrp-registry
         release-xrp
         update-xrp-stats
         update-xrp-hyperparams
         xrp->stats
         xrp->hyperparams
         
         xrp?
         make-xrp?
         xrp->scorer
         xrp->sampler
         xrp->unsampler
         xrp->key
         xrp->proposer
         xrp->name
         
         mem-registry
         new-mem-registry
         copy-mem-registry
         release-memproc
         
         memoized-procedure?
         memproc->subtraces
         memproc->subtrace
         memproc->key
         memproc->underlying-proc
         memproc->stack-address
         increment-memproc-counts
         decrement-memproc-counts
         update-memproc-subtraces
         )
 
 (import (church utils rnrs)
         (_srfi :1) ; lists
         (_srfi :69) ; hash tables
         (church readable-scheme)
         (church constraint-propagation primitive-inverses)
         (church constraint-propagation constraints)
         (church church-eval environments-lexical)
         (church church-eval traces)
         (church church-eval logmath)
         (church church-eval laziness)
         (church church-eval syntax)
         (church utils utils)
         (church external math-env))
 

 (define *lazy* (make-parameter #f))
 (define *adaptation* (make-parameter #f))
 (define *trace-eval-debug* (make-parameter #f))
 (define *fixed-erps* (make-parameter '()))
 (define *erp-interpretation* (make-parameter (lambda (x) x)))

 
  ;; trace-eval is a version of eval that keeps around its results.
 (define (trace-eval syntax env stack-address . args)
   (let* ((constraint-set (if (or (null? args)
                                  (equal? (first args) 'wildcard))
                              *wildcard-constraint-set*
                              (make-constraint-set (first args))))
          (result (force-proposal (trace-eval-core syntax env stack-address constraint-set)
                                  stack-address constraint-set)))
     (if (constraint-violated? (trace->value (proposal->trace result))
                               constraint-set)
         (make-proposal Zero-probability-proposal-exception-node
                        (proposal->forward/backward-score result))
         result)))

 
 (define (trace-eval-core syntax env stack-address constraint-set)
   
   (assert-with-message (lambda () (syntax? syntax)) (lambda () (error "Tried to trace-eval non-syntax object: " syntax)))
   
   (when (and (*global-debug*) (*trace-eval-debug*))
     (display "Trace-eval for node with expression: ")
     (write (syntax->original-expr syntax))
     (newline))
   
   (with-exception-handler 
    (lambda (exn) (begin (display "\nException within trace-eval on expression: ")
                         (write (syntax->original-expr syntax))
                         (display "\n  constraint-set: ")
                         (write constraint-set)
                         (newline)
                         (raise-continuable exn) ))
    (lambda () 
      
      (let ((dispatch-fn (cond ((syntax:begin? syntax) trace-eval-begin)
                               ((syntax:self-evaluating? syntax) trace-eval-self-evaluating) 
                               ((syntax:variable? syntax) trace-eval-variable-lookup)
                               ((syntax:quoted? syntax) trace-eval-quoted)
                               ((syntax:lambda? syntax) trace-eval-lambda)
                               ((syntax:get-env? syntax) trace-eval-get-env)
                               ((syntax:definition? syntax) trace-eval-define)
                               ((syntax:if? syntax) trace-eval-if)
                               ((syntax:application? syntax) trace-eval-application)
                               (else
                                (error "Unknown expression type - EVAL" syntax))))
            (stack-address (pair (syntax->id syntax) stack-address))) ;;extend stack-address with the id of this expression.
        
        (let-values (( (value subtraces forw-prob back-prob . dominated-erps)
                       (dispatch-fn syntax env stack-address constraint-set) ))
          (make-proposal (if (null? dominated-erps)
                             (build-eval-node syntax env value subtraces)
                             (build-eval-node syntax env value subtraces (first dominated-erps)))
                         (make-forward/backward-score forw-prob back-prob))))
      ))
   )
 
 
 ;;;;;here are the definitions of the dispatch functions that do the work:
 
 (define (trace-eval-self-evaluating syntax env stack-address constraint-set)
   (values (syntax->expr syntax) '() LOG-PROB-1 LOG-PROB-1))
 
 (define (trace-eval-quoted syntax env stack-address constraint-set)
   (values (quote-syntax->text-of-quotation syntax) '() LOG-PROB-1 LOG-PROB-1))
 
 (define (trace-eval-lambda syntax env stack-address constraint-set)
   (values (make-procedure (lambda-syntax->lambda-parameters syntax)
                           (lambda-syntax->lambda-body syntax)
                           env)
           '() LOG-PROB-1 LOG-PROB-1))
 
 (define (trace-eval-get-env syntax env stack-address constraint-set)
   (values env '() LOG-PROB-1 LOG-PROB-1))
 
 ;; if node: subtraces are ordered: (0)predicate, (1)branch actually followed (consequent or alternative).
 (define (trace-eval-if syntax env stack-address constraint-set)
   (let* ((new-pred-proposal (trace-eval-core (if-syntax->if-predicate syntax) env stack-address *wildcard-constraint-set*))
          (new-branch-proposal (trace-eval-core (if (true? (trace->value (proposal->trace new-pred-proposal)))
                                                    (if-syntax->if-consequent syntax)
                                                    (if-syntax->if-alternative syntax))
                                                env stack-address constraint-set))
          (sub-evals (list new-pred-proposal new-branch-proposal))
          (new-subtraces (extract-traces sub-evals)))
     
     (values (trace->value (proposal->trace new-branch-proposal))
             new-subtraces
             (eager-sum (extract-forward-probs sub-evals))
             (eager-sum (extract-reverse-probs sub-evals)) ) ))
 
 
 ;; For begin: drop a frame in env with all defined variables in the begin, evaluate everything in order, keep all traces.
 (define (trace-eval-begin syntax env stack-address constraint-set)
   (let* ((defined-vars (first (syntax->details syntax))) ;;note: syntax details for begin is a list of defined vars.
          (env (extend-environment defined-vars defined-vars env))
          (subexps (drop (syntax->expr syntax) 1))
          (first-subevals (map-in-order
                           (lambda (s) (trace-eval-core s env stack-address *wildcard-constraint-set*))
                           (drop-right subexps 1)))
          (last-subeval (trace-eval-core (last subexps) env stack-address constraint-set))
          (subevals (append first-subevals (list last-subeval))))
     (values (trace->value (proposal->trace (last subevals))) ;value of begin is value of last expression.
             (map proposal->trace subevals)
             (eager-sum (extract-forward-probs subevals))
             (eager-sum (extract-reverse-probs subevals))) ))

 ;; application node: subtraces are: (0) application, (1) operator, (2..) operands
 (define (trace-eval-application syntax env stack-address constraint-set)
   (let* ((new-operator-proposal (force-proposal (trace-eval-core (first (syntax->expr syntax)) env stack-address *wildcard-constraint-set*) stack-address constraint-set))
          (new-operator (proposal->value new-operator-proposal))
          (operand-handler (cond ((compound-procedure? new-operator) compound-operand-handler)
                                 ((primitive-procedure? new-operator) primitive-operand-handler)
                                 (else base-operand-handler)))
          (new-operand-proposals (operand-handler new-operator (rest (syntax->expr syntax)) env stack-address constraint-set))
          (new-appl-trace (trace-apply new-operator
                                       (map proposal->value new-operand-proposals)
                                       stack-address
                                       constraint-set)) 
          (sub-evals (pair new-appl-trace (pair new-operator-proposal new-operand-proposals))))
     (apply values
            (append
             (list (proposal->value new-appl-trace)
                   (extract-traces sub-evals)
                   (eager-sum (extract-forward-probs sub-evals))
                   (eager-sum (extract-reverse-probs sub-evals)))
             (if (no-proposals? syntax)
                 (list '())
                 (list))))))

  ;; base-operand-handler evaluates all operands, then forces
  (define (base-operand-handler operator operand-exprs env stack-address constraint-set)
   (let ((parameterized-eval
          (lambda (expr) (trace-eval-core expr env stack-address *wildcard-constraint-set*))))
     (map (lambda (expr) (force-proposal (parameterized-eval expr) stack-address *wildcard-constraint-set*))
          operand-exprs)))

 (define (skip? x)
   (eq? x 'skip))

 (define (wildcard-list? val)
   (and (not (null? val))
        (wildcard? (first val))))

 ;; primitive-operand-handler handles operand-exprs as determined by the operator's inverse;
 ;; in particular, the inverse determines how to derive operand constraints from overall constraints
 ;; sequentially and it may decide to skip evaluations.
 ;;
 ;; Policy:
 ;; * when applying a primitive inverse to a constraint set, we map it across
 ;;   the set then apply union. except if any inverse returns *wilcard-value*j,
 ;;   then the union is *wildcard-value*.
 ;; * sequential inverses: primitive inverses can be sequentialized like this:
 ;;   each takes a constraint-value and a partial list of argument values, and
 ;;   returns a set of possible values for the next argument (ie. values that
 ;;   extend the argument list, such that the list still has extensions satisfying
 ;;   the constraint)
 (define (primitive-operand-handler operator operand-exprs env stack-address constraint-set)
   (let* ((parameterized-eval (lambda (expr cset) (force-proposal (trace-eval-core expr env stack-address cset)
                                                             stack-address
                                                             cset)))
          (inverse (get-seq-inverse operator))
          (constraint-values (constraint-set->values constraint-set)))
     (define (constrained-eval-next-operand operand-expr args-so-far)
        (let* ((parameterized-inverse (lambda (constraint) (inverse constraint args-so-far (length operand-exprs))))
               (current-constraint-lists (map-until parameterized-inverse constraint-values wildcard-list?)))
          (if (any skip? current-constraint-lists)
              'skip
              (let* ((current-constraint-sets (map make-constraint-set current-constraint-lists))
                     (current-constraint-set (constraint-set-union current-constraint-sets)))
                (parameterized-eval operand-expr current-constraint-set)))))
     (accumulative-fold constrained-eval-next-operand operand-exprs '() proposal->value)))

 ;; compound-operand-handler delays all operands, then evaluates
 (define (compound-operand-handler operator operand-exprs env stack-address constraint-set)
   (let ((parameterized-eval
          (lambda (expr) (trace-eval-core expr env stack-address *wildcard-constraint-set*)))
         (delayed-operand-exprs (map (lambda (x) (delay x env stack-address)) operand-exprs)))
     (map parameterized-eval delayed-operand-exprs)))
 
 (define (no-proposals? syntax)
   (and (syntax:application? syntax)
        (eq? (first (syntax->original-expr syntax)) 'no-proposals)))

 ;; FIXME: Make sure the force here is necessary
 (define (trace-eval-variable-lookup syntax env stack-address constraint-set)
   (let ((lexical-address (first (syntax->details syntax))))
     (let-values ([(value imp-score) (force (first (lookup-value-by-id lexical-address env))
                                            stack-address
                                            constraint-set)])
       (values value '() imp-score LOG-PROB-1))))
  
 ;;extend the environment -- allows recursive definitions. 
 (define (trace-eval-define syntax env stack-address constraint-set)
   (let* ((defn-val-proposal (trace-eval-core (definition-syntax->definition-value syntax)
                                                         env
                                                         stack-address
                                                         constraint-set)) )
     (begin (define-variable! (definition-syntax->definition-variable syntax)
              (trace->value (proposal->trace defn-val-proposal))
              env)
            (values (trace->value (proposal->trace defn-val-proposal)) ;(void) ;env-to-extend
                    (list (proposal->trace defn-val-proposal))
                    (eager-sum (extract-forward-probs (list defn-val-proposal)))
                    (eager-sum (extract-reverse-probs (list defn-val-proposal)))
                    ))))
 
 ;;;;;;;;;;;;;   trace-apply
 ;; application returns a subtrace whose root is an application node (and the forward/reverse probs). 
 ;; if the operator and operands are already loaded into an initial application node, then re-application is conservative.
 (define (trace-apply operator operands stack-address constraint-set)
   
   (assert-with-message (lambda () (valid-operator? operator)) 
                        (lambda () (error "Tried to trace-apply to invalid operator: OPERATOR: " operator "\n"
                                     "  OPERANDS: " operands "\n")))
   (let ((dispatch-fn 
          (cond 
           ((mem? operator) apply-mem)
           ((memoized-procedure? operator) apply-memoized-procedure)
           ((eval-or-apply? operator) apply-trace-producing-primitive)
           ((primitive-procedure? operator) apply-primitive-procedure)
           ((elementary-random-procedure? operator) apply-erp)
           ((make-xrp? operator) apply-make-xrp)
           ((xrp? operator) apply-xrp)
           ((compound-procedure? operator) apply-compound-procedure)
           (else (error "trace-eval" "Unknown procedure type -- APPLY" "OPERATOR" operator "OPERAND: " operands)))))
     
     (let-values (( (value score subtraces forw-prob back-prob . dominated-erps)
                    (dispatch-fn operator operands stack-address constraint-set)))
       (make-proposal (if (null? dominated-erps)
                          (build-apply-node operator operands value score subtraces)
                          (build-apply-node operator operands value score subtraces (first dominated-erps)))
                      (make-forward/backward-score forw-prob back-prob))))) 
 
 (define (apply-mem operator operands stack-address constraint-set)
   (let* ((proc (first operands))
          (from-delay (if (not (null? (rest operands))) (second operands) false))
          (new-mem-proc (build-new-memoized-procedure proc stack-address from-delay)))
     (values new-mem-proc
             (delay-sum (map trace->score (memproc->subtraces new-mem-proc)))
             '()
             LOG-PROB-1
             LOG-PROB-1
             (delay-append (map get-dominated-erps (memproc->subtraces new-mem-proc))) )))
 
 (define (apply-memoized-procedure memproc operands stack-address constraint-set)
   (let ((subtrace (memproc->subtrace memproc operands)))
     (if (false? subtrace)
         
         (let ((proposal (trace-apply (memproc->underlying-proc memproc)
                                      operands
                                      (pair operands (memproc->stack-address memproc))
                                      constraint-set)))
           (update-memproc-subtraces memproc (list (proposal->trace proposal)))
           (increment-memproc-counts memproc operands)
           (values (trace->value (proposal->trace proposal))
                   LOG-PROB-1 ;; score is paid (lazily) by mem node
                   '()
                   (forward/backward-score->forward-score (proposal->forward/backward-score proposal))
                   (forward/backward-score->backward-score (proposal->forward/backward-score proposal))
                   ;; (get-dominated-erps (proposal->trace proposal))  
                   ))
         
         (begin (increment-memproc-counts memproc operands)
                (values (trace->value subtrace)
                        LOG-PROB-1 '() LOG-PROB-1 LOG-PROB-1
                        ;; (get-dominated-erps subtrace) ;;we set a memproc application to dominate the erps of the corresponding trace....
                        ))
         )))
 
 ;; there are some primitives (eval, apply) that take a trace, do random stuff,
 ;; and return a trace as their value -- we want to treat this as the subtrace.
 ;; FIXME: eval does a fresh syntax pass, which makes stack-address out of
 ;; synch!! need syntax ids to be deterministic?
 (define (apply-trace-producing-primitive operator operands stack-address constraint-set)
   (let* ([is-constrained-eval (and (eq? operator 'eval)
                                    (> (length operands) 2))]
          [sub-evals (list (cond [(eq? operator 'eval)
                                  (trace-eval-core (sexpr->syntax (first operands) (second operands))
                                                   (second operands)
                                                   stack-address
                                                   (if is-constrained-eval
                                                       (make-constraint-set (third operands))
                                                       constraint-set))]
                                 [(eq? operator 'apply)
                                  (trace-apply (first operands)
                                               (second operands)
                                               stack-address
                                               constraint-set)]))]
          [new-subtrace (extract-traces sub-evals)]
          [fw (eager-sum (extract-forward-probs sub-evals))]
          [bw (eager-sum (extract-reverse-probs sub-evals))]
          [score (trace->score (first new-subtrace))]
          [value (if is-constrained-eval
                     (pair (trace->value (first new-subtrace)) (- score fw))
                     (trace->value (first new-subtrace)))])
     (values value score new-subtrace fw bw)))
 
 ;;primitive application is called out to underlying scheme -- random choices in primitives are not added to trace!
 (define (apply-primitive-procedure operator operands stack-address constraint-set)
   (values (apply-in-underlying-scheme (primitive-implementation operator) operands)
           LOG-PROB-1 ;note stochastic primitive procedures must be wrapped up as erps to compute score.
           '()
           LOG-PROB-1 LOG-PROB-1))

 ;;apply an erp, use default proposer to get a fresh trace.
 (define (apply-erp operator operands stack-address constraint-set)
   (let*-values ([(fixed-erp-entry) (assoc stack-address (*fixed-erps*))]
                 [(proposed-val forw-prob back-prob)
                  (if (false? fixed-erp-entry)
                      (sample-erp-value operator operands stack-address constraint-set)
                      (values (rest fixed-erp-entry) LOG-PROB-1 LOG-PROB-1))]
                 [(proposed-val) ((*erp-interpretation*) proposed-val)]
                 [(proposal-score) (apply-church-procedure (erp-scorer operator)
                                                           (list operands proposed-val)
                                                           *wildcard-constraint-set*)])
     (values proposed-val
             proposal-score
             '()
             forw-prob
             back-prob
             (list (pair stack-address proposed-val)))))

 (define (get-erp-score operator operands val)
   (apply-church-procedure (erp-scorer operator)
                           (list operands val)
                           *wildcard-constraint-set*))

 ;; return value, importance score
 (define (sample-erp-value operator operands stack-address constraint-set)
   (cond ;; [(*adaptation*)
         ;;  (adaptive-proposer operator operands stack-address constraint-set)]
         [(not (wildcard-constraint-set? constraint-set))
          (constrained-default-proposer operator operands stack-address constraint-set)]
         [else
          (unconstrained-default-proposer operator operands)]))

 ;; FIXME:
 ;; - add real context
 ;; - (get-erp-support operator operands)
 ;; - (get-bounds context val)
 ;; - fuzzysample and weighted-ints->ints need to be bound in
 ;; (define (adaptive-proposer operator operands stack-address constraint-set) ;; context)
 ;;   (let* ([context 'UNDEFINED]
 ;;          [possible-values (if (wildcard-constraint-set? constraint-set)
 ;;                               (get-erp-support operator operands)
 ;;                               (constraint-set->values constraint-set))]
 ;;          [priors (map (lambda (val) (get-erp-score operator operands val)) possible-values)]
 ;;          [intervals (map (lambda (val) (get-bounds context val)) possible-values)])
 ;;     (fuzzysample (weighted-ints->ints possible-values priors intervals))))
 
 ;; proposers take in operator operands and old-value returns proposed-value,
 ;; foreward and backward probability.
 ;; FIXME: make this happen in log space
 ;; FIXME: reject if all weights are 0 or if there are no possible values?
 ;; FIXME: what should the backward score be?
 (define (constrained-default-proposer operator operands stack-address constraint-set)
   (let* ((possible-values (constraint-set->values constraint-set))
          (weights (map exp (log-normalize (map (lambda (val) (get-erp-score operator operands val)) possible-values))))
          (sample-index (sample-discrete weights))
          (sample (list-ref possible-values sample-index))
          (sample-weight (list-ref weights sample-index)))
     (values sample (log sample-weight) LOG-PROB-1)))
 
 ;; this default proposer ignores the current value and proposes from the prior
 ;; (ie. by trace-apply for the underlying procedure).
 (define (unconstrained-default-proposer operator operands . old-value)
   (assert-with-message (lambda () (elementary-random-procedure? operator))
                        (lambda () (error "Attempt to apply default proposal to non-erp application.")))
   ;;init by applying the underlying procedure:
   (let* ((proposed-val (apply-church-procedure (erp-sampler operator) operands *wildcard-constraint-set*))
          (new-score (get-erp-score operator operands proposed-val))
          (old-score (if (null? old-value)
                         LOG-PROB-1
                         (get-erp-score operator operands (first old-value)))))
     ;;FIXME: inefficient 'cause we re-score old value each time, and double score new value.....

     (when (and (*global-debug*) (*trace-eval-debug*))
           (for-each display (list "  Default proposal: (re-)initialized and (re-)scored erp. "
                                   "Previous value: " old-value ", new value: " proposed-val ".\n")))

     ;;forward prob is marginal probability, since we've proposed from prior. similarly, reverse prob is old-score:
     (values proposed-val new-score old-score)))
 
 ;;apply a compound procedure by calling trace-eval on its body in an extended enviroment,
 (define (apply-compound-procedure operator operands stack-address constraint-set)
   (let* ((binds (bindings (procedure-parameters operator) operands))
          (binding-vars (first binds))
          (binding-vals (rest binds))
          (unchanged-bindings (make-list (length binding-vals) false)) ;;on first eval all bindings are new.
          (extended-env (extend-environment binding-vars
                                            binding-vals
                                            (procedure-environment operator)
                                            unchanged-bindings ))
          (sub-evals (list (trace-eval-core (procedure-body operator) extended-env stack-address constraint-set)))
          (new-subtrace (extract-traces sub-evals)))
     
     (when (and (*global-debug*) (*trace-eval-debug*))
           (for-each display (list "   applied compound procedure, binding vars: "
                                   binding-vars ", binding-vals: " binding-vals)))
     
     (values (trace->value (first new-subtrace)) 
             (trace->score (first new-subtrace)) 
             new-subtrace
             (eager-sum (extract-forward-probs sub-evals))
             (eager-sum (extract-reverse-probs sub-evals)) )))
 
 
 
 ;;;;;create a make-xrp node.
 ;; an xrp object contains sample, unsample, and score functions, and a pointer to the sufficients stats and hyperparams.
 ;; the tricky thing is that sufficient stats must be updated in a way that maintains purity across church-eval calls....
 ;; we do this for now by maintaining a global xrp registry (a parameter) that is (shallow) copied on church-eval.
 ;; 
 ;; scores (and dominated erps?) are delayed.
 ;; the church procedure (make-xrp sample unsample score init hyper-params) returns a new xrp object. 
 ;; first three arguments are church procedures: 
 ;;  init is the initial sufficient stats.
 ;;  (sample stats hyperparams) samples from the xrp conditioned on stats, returns sampled value and new stats (for pure update) as a list.
 ;;  (unsample value stats hyperparams) decrements stats, removing value from draws (can pretend it's last by exchangeability), returns new stats.
 ;;  (score stats hyperparams) returns the score (log-prob) of the xrp with given sufficient stats and parameters.
 ;;  init-stats is the initial sufficient stats.
 ;;  hyperparams is the hyper parameters.
 ;; 
 (define (apply-make-xrp operator operands stack-address constraint-set)
   (let-values (( (xrp-name sampler unsampler scorer init-stats hyperparams . proposer) (apply values operands)))
     (let* ((xrp-key (gensym 'xrp-key)) ;;key of the xrp.
            (xrp (list 'xrp sampler unsampler scorer xrp-key   ;FIXME: use stack-address as key?
                       (if (null? proposer) '() (first proposer))
                       xrp-name 
                       stack-address))) 
       (update-xrp-stats xrp init-stats)
       (update-xrp-hyperparams xrp hyperparams)
       (values xrp
               (delay-sum (list (apply-church-procedure scorer 
                                                        (list (xrp->stats xrp)
                                                              hyperparams)
                                                        *wildcard-constraint-set*)))
               '()
               LOG-PROB-1
               LOG-PROB-1
               ))))
 
 ;;;apply an xrp -- sample a fresh value.
 ;; the make-xrp pays the cost, so no scoring here.
 ;; FIXME: need to apply *erp-interpretation* to value (before scoring??) in order for AD to work on xrp values.
 (define (apply-xrp operator operands stack-address constraint-set)
   (let* ((hyperparams (xrp->hyperparams operator))
          (old-stats (xrp->stats operator))
          (old-score (apply-church-procedure (xrp->scorer operator) (list old-stats hyperparams) *wildcard-constraint-set*))
          
          ;;sample, update stats, and get new score:
          (value-and-stats (apply-church-procedure (xrp->sampler operator) (list old-stats hyperparams) *wildcard-constraint-set*))
          (dummy (update-xrp-stats operator (second value-and-stats)))
          (new-score (apply-church-procedure (xrp->scorer operator) (list (second value-and-stats) hyperparams) *wildcard-constraint-set*)))
     
     ;;forward prob is conditional prior, since we've proposed from prior. this is: (new-score - old-score).
     (values (first value-and-stats)
             LOG-PROB-1
             '()
             (- new-score old-score)  ;;FIXME: can xrp scores be lazy? '-' won't work on lrbs.... 
             LOG-PROB-1
             (list (pair stack-address (first value-and-stats)))    ;(list (pair (gensym 'xrp) (xrp->name operator))) ;;an xrp dominates itself, make it's name here....
             )))
 
 
 
 
 ;;;;;;;memoization stuff
 ;; written by NDG on Jun 20, 2008
 ;; this is a stateful implementation of the memoizer.                   
 
 (define (new-mem-registry) (make-hash-table)) ;;by default this is equal?-based.
 
 (define mem-registry (make-parameter (new-mem-registry)))
 
 ;;FIXME: we may not need to copy every memproc table on every church-eval...
 (define (copy-mem-registry reg) 
   (let ((new (new-mem-registry))) ;;FIXME: resize first?.. (* 2 (hash-table-size hash-table))
    (hash-table-walk reg (lambda (key memproc-table) (hash-table-set! new key (hash-table-copy memproc-table))))
     new))

 (define (release-memproc memproc) (hash-table-delete! (mem-registry) (memproc->key memproc)))
 
 (define (memoized-procedure? p) (tagged-list? p 'memoized-procedure))
 (define (memproc->key memproc) (second memproc))
 (define (memproc->underlying-proc memproc) (third memproc))
 (define (memproc->stack-address memproc) (fourth memproc))
 (define (memproc->from-delay memproc)
   (sixth memproc))
  
 ;;the memproc-table stores pairs of (count . subtrace). 
 (define (memproc->subtraces memproc) 
   (map rest (hash-table-values (hash-table-ref (mem-registry) (memproc->key memproc)))))
 (define (memproc->subtrace memproc operands)
   (rest
    (hash-table-ref/default (hash-table-ref (mem-registry) (memproc->key memproc))
                            operands
                            (pair #f #f))))

 (define (update-memproc-subtraces memproc subtraces) 
   (let ((memproc-table (hash-table-ref (mem-registry) (memproc->key memproc))))
     (map (lambda (trace) 
            (hash-table-update! memproc-table (get-operands trace) 
                               (lambda (old-entry) (pair (first old-entry) trace))
                               (lambda () (pair 0 trace))))
          subtraces)))
 
 (define (increment-memproc-counts memproc operands) 
   (hash-table-update! (hash-table-ref (mem-registry) (memproc->key memproc))
                       operands
                       (lambda (old-entry) (pair (+ (first old-entry) 1) (rest old-entry)))))

 ;;decrement a count, if count is now 0, delete the subtrace. returns the probability of anything it deletes.
 (define (decrement-memproc-counts memproc operands) 
   (let* ((memproc-table (hash-table-ref (mem-registry) (memproc->key memproc)))
          (entry (hash-table-ref memproc-table operands)))
     (if (= 1 (first entry))
         (begin (hash-table-delete! memproc-table operands)
                (trace-delete (rest entry)))
         (begin (hash-table-set! memproc-table operands (pair (- (first entry) 1) (rest entry)))
                LOG-PROB-1))))
 
 ;;this function creates a new memoized procedure.
 (define (build-new-memoized-procedure proc stack-address . args)
   (let ((key (gensym 'mem-table))
         (memproc-table (make-hash-table)) ;;the operands->subtrace hash, is equal?-based (default in srfi 69).
         (from-delay (if (null? args) false (first args))))
     ;; store the stateful part in the mem registry:
     (hash-table-set! (mem-registry) key memproc-table)
     ;; return a memoized procedure:
     (list 'memoized-procedure
           key
           proc
           stack-address
           (gensym) ;;this gensym is only here so that env-equiv knows when a mem-proc in updated....
           from-delay)))
 
 
 ; (define (memoized-procedure? p)
 ;   (tagged-list? p 'memoized-procedure))
 ; 
 ; (define (memoized-procedure p)
 ;   (second p))
 ; 
 ; (define (memoizer-table p)
 ;   (third p))
 ; 
 ; (define (underlying-proc p)
 ;   (fourth p))
 ; 
 ; (define (params-for-apply p)
 ;   (fifth p))
 ; 
 ; ;;this function creates a new memoized procedure.
 ; (define (build-new-memoized-procedure proc params)
 ;   (let ((table (make-hash-table))) ;this is equal?-based (default in srfi 69).
 ;     (list 'memoized-procedure
 ;           (lambda args ;;FIXME: this should be (args) since we always call on list of operands anyway....
 ;             (hash-table-ref table args
 ;                             (lambda () 
 ;                               (let ((new-val (trace-apply proc (first args) params)))
 ;                                 (hash-table-set! table args new-val)
 ;                                 new-val))))
 ;           table
 ;           proc
 ;           params)))
 ; 
 ; 
 ; ;;fn should be a function which takes (args trace-and-probs) as input.
 ; ;;this function copies the memoizer into a fresh hash-table, so can be used as a 'pure' function. (this is less efficient than operating in place, when that's possible.)
 ; (define (map-over-mem fn memproc)
 ;   (fold-over-mem 
 ;    (lambda (args trace-and-probs accum) (add-to-mem-table! args (fn args trace-and-probs) accum))
 ;    (build-new-memoized-procedure (underlying-proc memproc) (params-for-apply memproc)) ;;we initialize at a fresh table, so this will be pure.
 ;    memproc))
 ; 
 ; (define (add-to-mem-table! args val memproc)
 ;   (hash-table-set! (memoizer-table memproc) args val)
 ;   memproc)
 ; 
 ; ;;;fn takes (args trace-and-probs accum)
 ; ;;this doesn't strip out the proposal probs.
 ; (define (fold-over-mem fn initializer memproc) 
 ;   (hash-table-fold (memoizer-table memproc) fn initializer))
 
 


         ;;;;;;;;;;;;;  un-trace
         ;;this function is used to remove a subtrace: 
         ;it releases trace-nodes and collects up the backward probs (prob of regenerating of what it's deleting).
         ;returns backward probability.
         ;it undraws all xrps, and removes xrp-keys from the xrp-registry when the make-xrp is removed -- even though score is paid by make-erp
         ;node, the backward probs are accumulated from the xrp nodes (to properly account when an xrp is deleted, but not it's make-xrp).
         (define release-queue (make-parameter '()))
         (define (trace-delete trace)
           (let ((score 
                  (cond 
                    ((and (is-apply-node? trace) (xrp? (get-operator trace)))
                     (let* ((operator (get-operator trace))
                            (hyperparams (xrp->hyperparams operator))
                            (old-stats (xrp->stats operator))
                            (old-score (apply-church-procedure (xrp->scorer operator) (list old-stats hyperparams) ))
                            
                            (new-stats (apply-church-procedure (xrp->unsampler (get-operator trace)) 
                                                               (list (trace->value trace) old-stats hyperparams)
                                                               ))
                            (new-score (apply-church-procedure (xrp->scorer operator) (list new-stats hyperparams) )))
                       (update-xrp-stats (get-operator trace) new-stats)
                       ;;the backward probability is the conditional prob of regenerating this sample = old-score - new-score:
                       (- old-score new-score)) ) ;;FIXME: can xrp scores be lazy? '-' won't work on lrbs....
                    ((and (is-apply-node? trace) (make-xrp? (get-operator trace)))
                     ;;add the xrp to the queue, in order to remove the xrp-key from the registry at end of church-eval:
                     (release-queue (pair (trace->value trace) (release-queue)))
                     LOG-PROB-1 ;;backward prob paid by xrp nodes.
                     )
                    ((and (is-apply-node? trace) (erp? (get-operator trace)))
                     ;;return the probability of this erp draw:
                     (get-score trace)
                     )
                    ((and (is-apply-node? trace) (mem? (get-operator trace)))
                     ;;add the mem-proc to the queue, in order to remove the key from the registry at end of church-eval:
                     (release-queue (pair (trace->value trace) (release-queue)))
                     LOG-PROB-1 ;;backward prob paid by decrementing memproc nodes.
                     )
                    ((and (is-apply-node? trace) (memoized-procedure? (get-operator trace)))
                     (decrement-memproc-counts (get-operator trace) (get-operands trace))
                     )
                    (else
                     (eager-sum (map trace-delete (get-subtraces trace))))) ))
             (delete-trace-node trace)
             score))

 
 
 ;;;;some utils for church:
 
 (define (apply-church-procedure proc args . optional-args)
   ;; give null stack-address -- so only use for side computations.
   (let* ((constraint-set (if (null? optional-args)
                             *wildcard-constraint-set*
                             (first optional-args)))
          (proposal (trace-apply proc args '() constraint-set))
          (val (proposal->value proposal)))
     (trace-delete (proposal->trace proposal))
     ;;FIXME: trace-delete, otherwise xrps and mem get confused because things that aren't part of trace get added.
     val))
 
 (define (bindings params vals)
   (cond
     ((symbol? params) (pair (list params) (list vals)))
     ((null? params) (pair '() '()))
     (else (if (null? vals) (error "bindings" "Not enough arguments!")
               (let ((remainder (bindings (tail params) (tail vals))))
                 (pair (pair (head params) (first remainder)) 
                       (pair (first vals) (rest remainder))))))))
 
 
 (define (extract-traces proposal-list)
   (map proposal->trace proposal-list))
 
 (define (extract-forward-probs proposal-list)
   (map (compose forward/backward-score->forward-score proposal->forward/backward-score) proposal-list))
 
 (define (extract-reverse-probs proposal-list)
   (map (compose forward/backward-score->backward-score proposal->forward/backward-score) proposal-list))
 
 
 
 
 (define (valid-operator? operator) 
   (or (mem? operator) 
       (memoized-procedure? operator) 
       (eval-or-apply? operator) 
       (primitive-procedure? operator) 
       (elementary-random-procedure? operator) 
       (compound-procedure? operator) 
       (eq? 'noop (first operator))))
 
 (define (mem? p)
   (eq? p 'mem))
 
 (define (eval-or-apply? p)
   (or (eq? p 'eval) (eq? p 'apply)))
 
 
 (define (erp? p)
   (tagged-list? p 'erp))
 (define elementary-random-procedure? erp?)
 
 (define (marg procedure prob . proposer)
   (if (null? proposer)
       (list 'erp procedure prob '() )
       (list 'erp procedure prob '() (first proposer))))
 
 ;;FIXME: add assert that erp is actually an erp.
 (define (erp-scorer erp) (third erp))
 
 (define (erp-sampler erp) (second erp))
 
 (define (erp-unsampler erp) (fourth erp))
 
 (define (erp-proposer erp) 
   (if (< 4 (length erp)) (fifth erp) '()))

 (define (erp->name erp)
   (if  (< 5 (length erp)) (sixth erp) '()))
 
 
 (define (make-procedure parameters body env)
   (list 'procedure parameters body env))
 
 (define (compound-procedure? p)
   (tagged-list? p 'procedure))
 
 
 (define (procedure-parameters p) (cadr p))
 (define (procedure-body p) (caddr p))
 (define (procedure-environment p) (cadddr p))
 
 
 (define (primitive-procedure? proc)
   (tagged-list? proc 'primitive))
 
 ;;FIXME: assert that proc is actually primitive
 (define (primitive-implementation proc) (second proc))
 
 (define apply-in-underlying-scheme apply)
 
 
 ;;xrp stuff:
 (define (new-xrp-registry) (make-hash-table)) ;;by default this is equal?-based.
 
 (define xrp-registry (make-parameter (new-xrp-registry)))
 
 (define (copy-xrp-registry reg) (hash-table-copy reg))
 
 (define (release-xrp xrp)
;;    (let* ((hyperparams (xrp->hyperparams xrp))
;;           (old-stats (xrp->stats xrp))
;;           (score (apply-church-procedure (xrp->scorer xrp) (list old-stats hyperparams) )))
;;      (display score)(newline))
   (hash-table-delete! (xrp-registry) (xrp->key xrp)))
 
 (define (update-xrp-stats xrp new-stats) (hash-table-update! (xrp-registry) (xrp->key xrp) 
                                                              (lambda (stats-hyperp) (list new-stats (second stats-hyperp)))
                                                              (lambda () (list '() '()))))
 (define (xrp->stats xrp) (first (hash-table-ref (xrp-registry) (xrp->key xrp))))
 (define (update-xrp-hyperparams xrp new-hyperparams) (hash-table-update! (xrp-registry) (xrp->key xrp) 
                                                                          (lambda (stats-hyperp) (list (first stats-hyperp) new-hyperparams))
                                                                          (lambda () (list '() '()))))
 (define (xrp->hyperparams xrp) (second (hash-table-ref (xrp-registry) (xrp->key xrp))))
 
 (define (xrp? p)
   (tagged-list? p 'xrp))
 
 (define (make-xrp? p)
   (eq? p 'make-xrp))
 
 (define (xrp->scorer xrp) (fourth xrp))
 (define (xrp->sampler xrp) (second xrp))
 (define (xrp->unsampler xrp) (third xrp))
 (define (xrp->key xrp) (fifth xrp))
 (define (xrp->proposer xrp) (sixth xrp))
 (define (xrp->name xrp) (seventh xrp))



   ;; Laziness

 (define (delayed? val)
   (and
    (memoized-procedure? val)
    (memproc->from-delay val)))

 (define (delay syntax env stack-address)
   ;; FIXME: By using syntax->original expression and redoing the syntax
   ;; pass with (mem (lambda () ...)) wrapped around the expression, we
   ;; are redoing work that has been done before (and possibly keeping
   ;; adaptation from working).
   (if (not (*lazy*))
       syntax
       (sexpr->syntax `(mem (lambda () ,(syntax->original-expr syntax)) true) env)))
   
 (define (force val stack-address constraint-set)
   ;;(when (list? val)
   ;;  (for-each display (list "forcing val of length " (length val) ", car " (car val) ", delayed " (delayed? val) "\n")))
   (if (or (not (*lazy*)) (not (delayed? val)))
       (values val LOG-PROB-1)
       (let* ((forced-proposal (trace-apply val '() stack-address constraint-set))
              (forced-val (trace->value (proposal->trace forced-proposal)))
              (imp-score (forward/backward-score->forward-score
                          (proposal->forward/backward-score forced-proposal))))
         (let-values ([(reforced-val remaining-imp-score) (force forced-val stack-address constraint-set)])
           (values reforced-val (+ imp-score remaining-imp-score))))))

 (define (force-proposal proposal stack-address constraint-set)
   (if (not (*lazy*))
       proposal
       (let* ((trace (proposal->trace proposal))
              (fw/bw (proposal->forward/backward-score proposal)))
         (let-values ([(val imp-score) (force (trace->value trace) stack-address constraint-set)])
           (make-proposal
            (build-eval-node (get-syntax trace) (get-env trace) val (get-subtraces trace))
            (make-forward/backward-score 
             (+ (forward/backward-score->forward-score fw/bw) imp-score)
             (forward/backward-score->backward-score fw/bw)))))))
 
 
 
 (when (> (*verbosity*) 12)
   (display "loaded trace-eval\n"))
 
 
 )