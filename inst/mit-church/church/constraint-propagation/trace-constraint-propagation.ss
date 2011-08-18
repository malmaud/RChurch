#!r6rs

;; created on Jun 29, 2008 by noah goodman
;; authors: noah goodman

(library 
 (church constraint-propagation trace-constraint-propagation)
 
 (export force-to-value
         *constraint-debug*)
 
 (import (church utils rnrs)
         (_srfi :69)
         (_srfi :1)
         (church readable-scheme)
         (church constraint-propagation constraints)
         (church constraint-propagation primitive-inverses)
         (church church-eval environments-lexical)
         (church church-eval logmath)
         (church church-eval church-eval)
         (church church-eval trace-eval)
         (church church-eval traces)
         (church church-eval syntax)
         (church utils utils))
 
 ;;this is code to do simple constraint propogation in order to more sanely initialize MH.
 
 ;;TODO:
 ;;   fix push-multi-vals-down-branches  --  at the moment if a try conditionally succeeds (conditioned on forcing stack) it doesn't try other possibilities, but if the forcing then fails later it doesn't come back.
 
 (define *constraint-debug* (make-parameter #f))
 
 (define (force-to-value trace-container val env)
   (assert-with-message (lambda () (trace-container? trace-container)) (lambda () "not a trace container!"))
   (when (*global-debug*) 
     (if (not (fail? (merge val  (trace-container->value trace-container))));(equal? val (trace-container->value trace-container))
         (for-each display (list "constraint propogator called but trace already satisfies condition.\n"))
         (for-each display (list "attempting to set root value to " val "..\n"))))
   (parameterize ([xrp-registry (copy-xrp-registry (trace-container->xrp-registry trace-container))]
                  [mem-registry (copy-mem-registry (trace-container->mem-registry trace-container))])
     (let* ((trace (trace-container->trace trace-container))
            (try (make-val trace val '())))
       (if (fail? try)
           (begin (when (*global-debug*) (for-each display (list "  failed to set value!\n\n"))) 
                  'fail)
           (begin (when (*global-debug*) (for-each display (list "  succeded in constraint propogation! updating trace.. ")))
                  (let*-values ([ (updated-container) (make-trace-container try (xrp-registry) (mem-registry))]
                                ;;do a pass of church-eval to propogate new value:
                                [ (newtrace-container forw rev) (church-eval updated-container env 'force-complete-eval true)]) 
                    (when (*global-debug*) 
                      (if (not (fail? (merge val  (trace-container->value newtrace-container)))) ;(equal? val (trace-container->value newtrace-container))
                          (for-each display (list "update succeeded!\n\n"))
                          (for-each display (list "updated trace still has wrong value!! trace value: " (trace-container->value newtrace-container) "\n\n"))))
                    (if (not (fail? (merge val  (trace-container->value newtrace-container)))) ;(equal? val (trace-container->value newtrace-container))
                        newtrace-container
                        'fail) ))))))
 
 ;;this is the main function here, it tries to make the trace have value val...
 ;;returns adjusted trace, or 'fail if it gives up.
 (define (make-val trace val forcing-stack)
   (when (and (*global-debug*) (*constraint-debug*)) 
     (for-each display (list "  attempting to set trace value to " 
                             (if (eq? val *wildcard-value*) "wildcard (which is free)." val)
                             "\n")))
   ;"current value is " (trace->value trace) "\n")))
   (if (is-eval-node? trace) 
       (push-eval-node trace val forcing-stack) 
       (push-apply-node trace val forcing-stack)))
 
 ;;this tries to push a single value down a branch
 (define (push-val-down-branch n val trace forcing-stack)
   (if (fail? trace) 'fail
       (let ((new-sub (make-val (list-ref (get-subtraces trace) (- n 1)) val forcing-stack)))
         (if (fail? new-sub)  ;(or (eq? new-sub 'fail) (and (pair? new-sub) (eq? (first new-sub) 'fail)))  
             'fail
             (begin
               (assert-with-message (lambda () (or (is-eval-node? new-sub) (is-apply-node? new-sub))) (lambda () "non-trace in push-val-down-branch"))
               (replace-subtrace! n new-sub trace))
             ;(if (eval-node? new-sub)
             ;    (replace-subtrace n new-sub trace)
             ;    (pair (replace-subtrace n (first new-sub) trace) (rest new-sub)))
             ))))
 
 ;;this pushes a bunch of values down a bunch of branches. fails if any of them fails. 
 (define (push-vals-down-branches branches vals trace forcing-stack)
   (if (not (= (length branches) (length vals)))
       'fail
       (if (null? vals)
           trace
           (push-val-down-branch (first branches) (first vals) (push-vals-down-branches (rest branches) (rest vals) trace forcing-stack) forcing-stack))))
 
 
 
 (define (push-eval-node trace val forcing-stack)
   ;;if merge succeeds in finding a common refinement it means val is already satisfied by the trace value.
   (if (not (fail? (merge val (trace->value trace))))
       trace
       (let ((exp (get-expr trace)))
         (when (and (*global-debug*) (*constraint-debug*)) (for-each display (list "    in expression: " (get-original-expr trace) "\n")))
         (assert-with-message (lambda () (is-eval-node? trace)) (lambda () "not eval node..."))
         (cond 
           ((self-evaluating? exp) 'fail) 
           
           ((variable? exp) 
            (if (fail? (push-variable-value! exp val forcing-stack)) 'fail trace))
           
           ((quoted? exp) 'fail)
           ((lambda? exp) 'fail)
           ((get-env? exp) 'fail)

           ;;the value of a definition is the value of its binding, so try to set that:
           ((definition? exp)
            (let ((try (push-val-down-branch 1 val trace forcing-stack)))
              (if (fail? try)
                  'fail
                  try)))
           
           ((begin? exp)
            (let* ((defined-vars (first (syntax->details (get-syntax trace)))) ;;note: syntax details for begin is a list of defined vars.
                   (extended-forcing-stack (extend-environment defined-vars (make-list (length defined-vars) *wildcard-value*) forcing-stack))
                   (try (push-val-down-branch (length (get-subtraces trace)) val trace extended-forcing-stack))
                   (required-def-vals (first-frame-values extended-forcing-stack)))
              (if (fail? try)
                  'fail
                  (let ((definition-locations (filter-map (lambda (subtr index)
                                                            (if (syntax:definition? (get-syntax subtr)) index false))
                                                          (get-subtraces trace)
                                                          (iota (length (get-subtraces trace)) 1 1))))
                    (when (and (*global-debug*) (*constraint-debug*)) 
                      (for-each display (list "    pushing possible define settings: " required-def-vals
                                              "\n      for expression: " (get-original-expr trace)
                                              "\n      in branches: " definition-locations"\n" )))
                    (push-multi-vals-down-branches definition-locations
                                                   (list required-def-vals)
                                                   try 
                                                   forcing-stack)))))
           
           ((if? exp) 
            (let ((try (push-val-down-branch 2 val trace forcing-stack)))
              (if (fail? try)
                  ;try to set the predicate branch to other value:
                  (begin (when (and (*global-debug*) (*constraint-debug*)) (for-each display (list "    failed to force existing branch, trying to switch predicate.\n")))
                  (let* ((new-pred-val (not (trace->value (list-elt (get-subtraces trace) 1))))
                         (pred-try (push-val-down-branch 1 new-pred-val trace forcing-stack)))
                    (if (fail? pred-try)
                        'fail
                        ;if that worked, we have to build a new branch and try to push the value down it:
                        (let* ((branch-syntax (if (true? new-pred-val)
                                                  (if-syntax->if-consequent (get-syntax trace))
                                                  (if-syntax->if-alternative (get-syntax trace))))
                               ;(new-branch-trace (proposal->trace (church-eval branch-syntax (get-env trace))))
                               (new-branch-trace (proposal->trace (trace-eval branch-syntax (get-env trace) '())))
                               (branch-trace-try (make-val new-branch-trace val forcing-stack)))
                          (if (fail? branch-trace-try)
                              'fail
                              (replace-subtrace! 2 branch-trace-try pred-try))
                          ))) )
                  try)))
           
           ;;value comes from first subtrace (the apply node), try setting the value, expect back a trace and a list of value-lists for the operand subtraces..
           ((application? exp) 
            (let* ((application-subtrace (first (get-subtraces trace)))
                   (application-push (make-val application-subtrace val forcing-stack)))
              ;             (assert-with-message (or (fail? application-push) (pair? application-push)) "bare app node" (lambda () (display (get-operator application-push))))
              (if (fail? application-push)
                  'fail
                  (let ((new-trace (replace-subtrace! 1 (first application-push) trace)) ;(first apply-push))
                        (operand-settings (rest application-push)))
                    ;;ok, now go through pushing the possible operand settings, stopping when you get one...
                    (when (and (*global-debug*) (*constraint-debug*)) 
                      (for-each display (list "    pushing possible operand settings: " operand-settings " for expression: " (get-original-expr trace) "\n")))
                    (push-multi-vals-down-branches (range 3 (length (get-subtraces new-trace))) 
                                                   operand-settings 
                                                   new-trace 
                                                   forcing-stack)))))
           
           (else 'fail)))))
 
 
 ;;this takes a list of branches and a list of lists of value settings for those branches, and tries to set them. takes the first success...
 (define (push-multi-vals-down-branches branches valses trace forcing-stack)
   (if (null? valses) 'fail
       (let ((try (push-vals-down-branches branches (first valses) trace forcing-stack)))
         (if (fail? try) (push-multi-vals-down-branches branches (rest valses) trace forcing-stack) try))))
 
 
 ;;push a value through an apply node.
 ;;returns a pair (trace/'fail . list-of-operand-setting-lists)
 (define (push-apply-node trace val forcing-stack)
   (let* ((operator (get-operator trace))
          (operands (get-operands trace))
          (operand-wildcards (make-list (length operands) *wildcard-value*)))
     (assert-with-message (lambda () (is-apply-node? trace)) (lambda () "not apply node..."))
     ;;if merge succeeds in finding a common refinement it means val is already satisfied by the trace value.
     (if (not (fail? (merge val (trace->value trace))))
         (pair trace (list operand-wildcards))
         (cond 
           
           ((primitive-procedure? operator) 
            (when (and (*global-debug*) (*constraint-debug*)) (for-each display (list "    pushing through primitive procedure... \n")))
            (let ((inverse-vals (invert-primitive operator val operands)))
              (when (and (*global-debug*) (*constraint-debug*)) (for-each display (list "      inverse values: " inverse-vals " \n")))
              (if (or (null? inverse-vals) (fail? inverse-vals)) 'fail
                  (pair trace inverse-vals) )))
           
           ;;find the key for this argument, push the value down that trace, set! the return into the memoizer hash table
           ((memoized-procedure? operator) 
            (when (and (*global-debug*) (*constraint-debug*)) (for-each display (list "    pushing through memoized procedure... \n")))
            (let* ((mem-trace (memproc->subtrace operator operands))
                   (new-mem-trace (make-val mem-trace val the-empty-environment))) ;;traces in memoizer start w/ apply node, so this is (trace . operand-value-lists)
              (if (fail? new-mem-trace) 'fail
                  (begin
                    (update-memproc-subtraces operator (list (first new-mem-trace)))
                    (pair trace (rest new-mem-trace)) ))))
           
           ((eval-or-apply? operator) 
            (cond 
              ;((eq? operator 'eval)  )
              ((eq? operator 'apply) ;;push value down single subtrace, which is the apply node of the operator onto the unlisted operands
               (let ((try (make-val (get-subtrace 0 trace) val forcing-stack)))
                 (if (fail? try)
                     'fail
                     (pair (replace-subtrace! 1 (first try) trace) (map (lambda (operands) (list *wildcard-value* operands)) (rest try))))))
              (else 'fail)))
           
           ((elementary-random-procedure? operator) 
            (when (and (*global-debug*) (*constraint-debug*)) (for-each display (list "    pushing through erp... \n")))
            (let ((val-prob (apply-church-procedure (erp-scorer operator) 
                                                    (list operands val)
                                                    )))
              (if (= val-prob LOG-PROB-0) 
                  (begin (when (*global-debug*) (for-each display (list "    couldn't set erp to val: " val ", because probability is zero!\n")))
                         'fail)
                  (begin (when (*global-debug*) (for-each display (list "    setting erp to val: " val "\n")))
                         (pair 
                          (build-apply-node operator operands val val-prob (get-subtraces trace))
                          (list operand-wildcards) )))))
           
           ((xrp? operator) 
            (when (and (*global-debug*) (*constraint-debug*)) (for-each display (list "    pushing through xrp... \n")))
            (let* ((hyperparams (xrp->hyperparams operator))
                   (old-stats (xrp->stats operator))
                   (old-value (trace->value trace))
                   ;;unsample, remove current val from current stats:
                   (base-stats (apply-church-procedure (xrp->unsampler operator) (list old-value old-stats hyperparams) ))
                   ;;update stats st val is the sample (non-random!):
                   (new-stats (apply-church-procedure (xrp->unsampler operator) (list val base-stats hyperparams 'increment-stats) ))
                   (new-score (apply-church-procedure (xrp->scorer operator) (list new-stats hyperparams) )))
              
              ;;check that this is possible, if so commit stats and build a new node:
              (if (= new-score LOG-PROB-0) 
                  (begin (when (*global-debug*) (for-each display (list "    couldn't set xrp to val: " val ", because probability is zero!\n")))
                         'fail)
                  (begin (when (*global-debug*) (for-each display (list "    setting xrp to val: " val "\n")))
                         (update-xrp-stats operator new-stats)
                         (pair 
                          (build-apply-node operator operands val LOG-PROB-0 (get-subtraces trace))
                          (list operand-wildcards) )))))
           
           
           ;;drop a frame in the forcing-stack filled with wildcard values -- these will be overidden on variable lookup forcing -- then push down eval subtrace, 
           ;;on return pop the frame into the required operand values...
           ;;this will handle "linear" variable lookup dependencies: those within the procedure body of the lambda that binds them,
           ;;won't handle "crossing" dependencies that result from lambda creation and later application....
           ;      ((compound-procedure? operator)
           ;       (when (and (*global-debug*) (*constraint-debug*)) (for-each display (list "    pushing through compound procedure... \n")))
           ;       (let* ((new-bindgings (bindings (procedure-parameters operator) operand-wildcards))
           ;              (new-forcing-stack (extend-environment (first new-bindgings) (rest new-bindgings) forcing-stack))
           ;              (try (push-val-down-branch 1 val trace new-forcing-stack)))
           ;         (when (and (*global-debug*) (*constraint-debug*)) 
           ;           (for-each display (list "      popped operand values: " (first-frame-values new-forcing-stack) 
           ;                                   " from bindings: " (bindings (procedure-parameters operator) operand-wildcards) "\n")))
           ;         (pair try (list (first-frame-values new-forcing-stack))
           ;               )))
           ;      
           ;      (else 'fail ))))
           
           
           
           ((compound-procedure? operator)
            (when (and (*global-debug*) (*constraint-debug*)) (for-each display (list "    pushing through compound procedure... \n")))
            (let* ((new-bindgings (bindings (procedure-parameters operator) operand-wildcards))
                   (new-forcing-stack (extend-environment (first new-bindgings) (rest new-bindgings) forcing-stack))
                   (try (push-val-down-branch 1 val trace new-forcing-stack))
                   (required-operand-vals (unwrap-values-from-bindings (procedure-parameters operator) (first-frame-values new-forcing-stack)))
                   )
              (when (and (*global-debug*) (*constraint-debug*)) 
                (for-each display (list "      popped operand values: " required-operand-vals ;(first-frame-values new-forcing-stack) 
                                        " from bindings: " (procedure-parameters operator) "\n")))
              (pair try (list required-operand-vals))
              ))
           
           (else 'fail )))))
 
 ;;
 (define (unwrap-values-from-bindings params vals)
   (cond
     ((symbol? params) (first vals))
     ((null? params) '() )
     (else (pair (first vals) (unwrap-values-from-bindings (tail params) (tail vals))))))
 
 
 ;;scan for variable on stack, if it's found try to merge the value with whatever is aready there.
 (define (push-variable-value! var val stack)
   (let* ((current-val (with-exception-handler
                        (lambda (e) 
                          (begin (when (and (*global-debug*) (*constraint-debug*)) 
                                   (for-each display (list "    Failed to set variable " var " : not in forcing-stack. \n")))
                                 'fail))
                        (lambda () (lookup-variable-value var stack))))
          (merged-val (merge val current-val)))
     (if (or (fail? current-val) (fail? merged-val))
         (begin (when (and (*global-debug*) (*constraint-debug*)) 
                  (for-each display (list "    Failed to set variable: unable to merge. " val " with " current-val "\n")))
                'fail)
         (set-variable-value! var merged-val stack))))
 

 ;; FIXME: unify the following functions with the new
 ;; sequential inverses in primitive-inverses.ss
 
 (define (invert-primitive operator val operands)
   (if (invertible? operator)
       (apply (get-inverse operator) (pair val operands))
       (begin (when (and (*global-debug*) (*constraint-debug*)) 
                (for-each display (list "    No inverse for this primitive!\n")))
              'fail)))
 
 (define (invertible? operator)
   (and (< 2 (length operator)) (eq? (first (third operator)) 'deterministic-inverse)))
 
 (define (get-inverse operator)
   (second (third operator)))

 (when (> (*verbosity*) 12)
   (display "loaded trace-constraint-propagation.ss\n"))
 )