#!r6rs

;; created on Jun 17, 2008 by NDG
;; authors: noah goodman, daniel roy, andreas stuhlmueller

(library (church mcmc queries gradient-mh-query)

         (export gradient-query hmc-query visited-gradient-query visited-hmc-query make-gradient-kernel)

         (import (rename (church utils rnrs)
                         (real? rnrs-real?))
                 (_srfi :1)
                 (church utils utils)
                 (church constraint-propagation trace-constraint-propagation)
                 (church mcmc mcmc-core)
                 (church readable-scheme)
                 (church church-eval logmath)
                 (church church-eval syntax)
                 (church church-eval church-eval)
                 (church church-eval trace-eval)
                 (church church-eval traces)
                 (church external math-env)
                 (church utils AD)
                 (church utils mega-comparator)
                 )

         ;;for detecting continuous erps...
         (define (continuous? x) (and (real? x) (not (fixnum? x))))

         (define (gradient-query num-samples lag dt normal-form-query-expr env)
           (let ((tapify (make-tapifier)))
             (parameterize ([*erp-interpretation*
                             (lambda (x)
                               (let ((val (untapify x)))
                                 (if (continuous? val)
                                     (tapify val)
                                     val)))])
               (repeated-mcmc-query-core default-initialize
                                         (repeat-kernel lag
                                              (cycle-kernel
                                               (make-gradient-kernel dt)
                                               (make-mh-kernel (make-trace-walk-proposal (lambda (erp-entry)
                                                                                           (not (tape? (rest erp-entry))))) ;;only propose to non-taped erps.
                                                               trace-score-proc
                                                               "trace-walk-kernel")))
                                         num-samples
                                         (lambda (s) true) ;;every time a cycle is up, take a sample
                                         (lambda (v) (untapify (trace-finish v)))
                                         normal-form-query-expr
                                         env))))

         (define (visited-gradient-query num-samples lag dt visitor normal-form-query-expr env)
           (let ((tapify (make-tapifier)))
             (parameterize ([*erp-interpretation*
                             (lambda (x)
                               (let ((val (untapify x)))
                                 (if (continuous? val)
                                     (tapify val)
                                     val)))])
               (repeated-mcmc-query-core default-initialize
                                         (repeat-kernel lag
                                              (cycle-kernel
                                               (make-gradient-kernel dt)
                                               (make-mh-kernel (make-trace-walk-proposal (lambda (erp-entry)
                                                                                           (not (tape? (rest erp-entry))))) ;;only propose to non-taped erps.
                                                               trace-score-proc
                                                               "trace-walk-kernel")))
                                         num-samples
                                         (lambda (s) true) ;;every time a cycle is up, take a sample
                                         (lambda (v)
                                           (let ((sample (untapify (trace-finish v))))
                                             (apply-church-procedure visitor (list sample))
                                             sample))
                                         normal-form-query-expr
                                         env))))

         (define (hmc-query num-samples lag dt leapfrog-steps normal-form-query-expr env)
           (let* ((tapify (make-tapifier)))
             (parameterize ([*erp-interpretation*
                             (lambda (x)
                               (let ((val (untapify x)))
                                 (if (continuous? val)
                                     (tapify val)
                                     val)))])
               (repeated-mcmc-query-core default-initialize 
                                         (repeat-kernel lag
                                                        (cycle-kernel
                                                         (make-hmc-kernel dt leapfrog-steps)
                                                         (make-mh-kernel (make-trace-walk-proposal (lambda (erp-entry)
                                                                                                     (not (tape? (rest erp-entry))))) ;;only propose to non-real erps.
                                                                         trace-score-proc
                                                                         "trace-walk-kernel")))
                                         num-samples
                                         (lambda (s) true) ;;every time a cycle is up, take a sample
                                         (lambda (v) (untapify (trace-finish v)))
                                         normal-form-query-expr
                                         env))))

         (define (visited-hmc-query num-samples lag dt leapfrog-steps visitor normal-form-query-expr env)
           (let* ((tapify (make-tapifier)))
             (parameterize ([*erp-interpretation*
                             (lambda (x)
                               (let ((val (untapify x)))
                                 (if (continuous? val)
                                     (tapify val)
                                     val)))])
               (repeated-mcmc-query-core default-initialize 
                                         (repeat-kernel lag
                                                        (cycle-kernel
                                                         (make-hmc-kernel dt leapfrog-steps)
                                                         (make-mh-kernel (make-trace-walk-proposal (lambda (erp-entry)
                                                                                                     (not (tape? (rest erp-entry))))) ;;only propose to non-real erps.
                                                                         trace-score-proc
                                                                         "trace-walk-kernel")))
                                         num-samples
                                         (lambda (s) true) ;;every time a cycle is up, take a sample
                                         (lambda (v)
                                           (let ((sample (untapify (trace-finish v))))
                                             (apply-church-procedure visitor (list sample))
                                             sample))
                                         normal-form-query-expr
                                         env))))
         
         ;;worry about the multipath problem that comes if the set of real-valued erps changes (due to trace-update).

         ;;compute the gradient of the score of a trace-container wrt any tapified erp values.
         (define (trace-container->gradient tc)
           (first
            (second
             (xy-gradient-R (lambda (f erps) (filter-map (lambda (e) (if (tape? (rest e)) (pair (first e) (f (rest e))) false)) erps)) ;map-independent
                            (trace-container->erps tc) ;x-reverse
                            (trace-container->score tc) ;y-reverse
                            (*erp-interpretation*)))))

         (define (make-gradient-kernel dt) (make-mh-kernel (make-gradient-trace-walk-proposal dt) trace-score-proc "gradient-trace-walk-kernel"))
         
         ;;proposed-erps is a list of (erp-address . proposed-value), ie. the new erp values.
         (define (make-gradient-trace-walk-proposal dt)
           (lambda (trace-container env)
             (when (or (*global-debug*) (*mh-steps*)) (display "gradient-trace-walk-proposal..\n"))
             (let*-values ([(world) (trace-container->erps trace-container)]
                           [(proposed-erps proposal-fw-prob) (make-gradient-proposal world trace-container dt env)]
                           [(new-trace-container forw-prob rev-prob)
                            (parameterize ([*fixed-erps* proposed-erps])
                              (church-eval trace-container env 'propose proposed-erps))]
                           [(proposal-rev-prob) (rev-prob-of-proposal proposed-erps new-trace-container dt env)])

               (when (*global-debug*)
                 (for-each display (list "current target-val: " (untapify (mcmc-tc->query-val trace-container)) ", "
                                         "proposal target-val: " (if (eq? (trace-container->value new-trace-container) 'zero-probability-proposal-exception)
                                                                     "zero-probability-proposal-exception"
                                                                     (untapify (mcmc-tc->query-val new-trace-container)))
                                         ", "
                                         "proposal conditioner value: " (untapify (mcmc-tc->cond-val new-trace-container)) "\n" )))
               (when (or (*global-debug*) (*mh-steps*))
                 (for-each display (list "     rev-prob: "  (untapify rev-prob)
                                         " proposal-rev-prob: " (untapify proposal-rev-prob)
                                         " proposal score: " (untapify (trace-container->score new-trace-container)) "\n"
                                         "     forw-prob: "  (untapify forw-prob)
                                         " proposal-fw-prob: " (untapify proposal-fw-prob)
                                         " old score: " (untapify (trace-container->score trace-container)) "\n \n")))

               (make-move (+ forw-prob proposal-fw-prob)
                          (+ rev-prob proposal-rev-prob)
                          new-trace-container ))))
 
         (define (dt->variance dt)
           (* dt dt))
         
         (define (make-gradient-proposal world tc dt env)
           (let-values (((real-world discrete-world) (partition (lambda (erp-entry) (tape? (rest erp-entry))) world)))
             (let* ((real-world-names (map first real-world))
                    (real-world-vals (map rest real-world))
                    (gradient (norm-to-length (map rest (trace-container->gradient tc)) dt))
                    (variance (dt->variance dt))
                    (perturbations (repeat (length real-world) (lambda () (sample-gaussian 0 variance ))))
                    (log-prob-of-perturbations (sum (map (lambda (perturbation) (gaussian-lnpdf perturbation 0 variance)) perturbations)))
                    (new-real-world-vals (map + real-world-vals gradient perturbations))
                    (new-real-world (map pair real-world-names new-real-world-vals)))

;               (when (not (equal? real-world-names (map first (trace-container->gradient tc))))
;                 (display "WAAAAAAAAAAAAAARRRRRRRRRRRRRRRRGGGGGGGGGGHHHHHHHHHHHH!\n")
;                 (display real-world-names)(newline)
;                 (exit)
;                 )
               
               (begin
                 (when (or (*global-debug*) (*mh-steps*))
                   (for-each display (list " gradient: " gradient "\n"
                                           " perturbations: " perturbations "\n"
                                           " old-world-vals:" (untapify real-world-vals) "\n"
                                           " new-world-vals: " (untapify new-real-world-vals) "\n")))
                 (values new-real-world log-prob-of-perturbations)))))
  
         (define (sort-erps erp-list)
           (list-sort mega-comparator erp-list))
         
         (define (rev-prob-of-proposal real-old-world new-tc dt env)

           (let*-values 
               (
                ((new-world) (trace-container->erps new-tc))
                ((real-new-world discrete-new-world) (partition (lambda (erp-entry) (tape? (rest erp-entry))) new-world))
                
                ;; XXX TODO: note that we could check to see if the
                ;; names of row and rnw are equal.  If they are, we
                ;; don't need to sort them.
                ((real-new-world) (sort-erps real-new-world))
                ((real-old-world) (sort-erps real-old-world))

                ;; XXX TODO: similarly, we can check to see if the
                ;; names of row and rnw are the same AFTER sorting.
                ;; If they aren't, then we know that new erps have
                ;; been created / deleted.  this would be a more
                ;; general version of the check below, where the
                ;; lengths are compared, because it would additionally
                ;; catch the case where exactly n erps were created
                ;; and n erps were deleted, resulting in two sets of
                ;; erps with the same cardinality but different
                ;; members.

                )
             
             (if (not (= (length real-old-world) (length real-new-world)))
                 LOG-PROB-0 ;;set of real erps changed, probably due to zero-probability-proposal-exception?
                 (let* ((real-new-world-names (map first real-new-world))
                        (real-old-world-names (map first real-old-world))
                        (real-new-world-vals (map rest real-new-world))
                        (real-old-world-vals (map rest real-old-world))
                        (variance (dt->variance dt))                        
                        (gradient (norm-to-length (map rest (sort-erps (trace-container->gradient new-tc))) dt))
                        (perturbations (map - (map + real-new-world-vals gradient) real-old-world-vals)))
                   
;                   (when (not (equal? real-new-world-names real-old-world-names))
;                     (display "WAAAAAAAAAAAAAARRRRRRRRRRRRRRRRGGGGGGGGGGHHHHHHHHHHHH!\n")
;                     (display real-new-world-names)(newline)
;                     (display real-old-world-names)(newline)
;                     (exit)
;                     )
                   
                   (sum (map (lambda (perturbation) (gaussian-lnpdf perturbation 0 variance)) perturbations))))))

         (define (norm-to-length L e)
           (let* ((s (sqrt (apply + (map (lambda (x) (expt x 2)) L))))
                  (se (/ e s)))
             (map (lambda (x) (* se x)) L)))

         ;;;;;;;;;;;;;;;;;;;hamiltonian MC algorithm
         ;;the hmc kernel:
         ;;  augments the tc with a momentum vector of the appropriate size
         ;;  applies the leapfrog kernel, which is an MH kernel for hamiltonian score with proposals:
         ;;    take a leapfrog step in trace-momentum space (ie. update momentum or real-erp values, then trace-update)
         ;;
         ;;FIXME: what do we do if erps change in a leapfrog step? continuous? discrete?

         (define (make-hmc-kernel dt leapfrog-steps)
           (let ((leapfrog-kernel (make-leapfrog-kernel dt leapfrog-steps)))
             (lambda (tc env)
               (let ((momenta (momentum-prior tc))) ;;generate appropriate momenta from the momentum prior (this is secretly a kernel on momenta)
                 (second (leapfrog-kernel (list momenta tc) env)))))) ;;apply leapfrog kernel, return the trace (if we kept the momenta we'd reverse them).

         (define (momentum-prior tc)
           (let ((real-world (filter (lambda (erp-entry) (tape? (rest erp-entry))) (trace-container->erps tc))))
             (repeat (length real-world) (lambda () (sample-gaussian 0.0 1.0)))))

         (define (make-leapfrog-kernel dt leapfrog-steps) (make-mh-kernel (make-leapfrog-proposal dt leapfrog-steps) hamiltonian-score "leapfrog-kernel"))

         (define (hamiltonian-score p-tc)
           (when (*mh-steps*) (display "computing hamiltonian.\n"))
           (let ((p (first p-tc))
                 (tc (second p-tc)))
             (if (and (not (eq? (trace-container->value tc) 'zero-probability-proposal-exception))
                      (eq? true (mcmc-tc->cond-val tc)))
               (+ (trace-container->score tc)
                  (* -0.5 (apply + (map (lambda (x) (expt x 2)) p))) ;;FIXME: inv-mass terms
                  ) ;;FIXME: term for norm of mass.
               LOG-PROB-0 )))
         
         ;;because the leapfrog integrator is reversible (and volume preserving) the only f/b-probs come from trace-updates.
         (define (make-leapfrog-proposal dt steps)
           (lambda (p-tc env)
             (let-values ([ (tc-final p-final fw-prob rev-prob) (leapfrog (second p-tc) (first p-tc) dt steps env)])
               (make-move fw-prob
                          rev-prob
                          (list p-final tc-final)
                          ))))

         (define (leapfrog tc p dt steps env . f/b-probs)
           (let ((forw-prob (if (null? f/b-probs) LOG-PROB-1 (first f/b-probs)))
                 (rev-prob (if (null? f/b-probs) LOG-PROB-1 (second f/b-probs)))
                 (m-inv (make-list (length p) 1.0))) ;;FIXME: these should be parameters, or set cleverly....
             (if (= steps 0)
                 (values tc p forw-prob rev-prob)
                 (let-values ([ (tc-next p-next additional-fw-prob additional-rev-prob) (leapfrog-step tc p m-inv dt env)])
                   (leapfrog tc-next p-next dt (- steps 1) env (+ forw-prob additional-fw-prob) (+ rev-prob additional-rev-prob))))))
             
         ;;one step of the leapfrog integrator.
         ;;note: this will cause extra evaluations of gradU unless gradU caches (as tc->gradient does).
         (define (leapfrog-step q p m-inv dt env)
           (when (*mh-steps*)
             (let-values ([(real-world discrete-world) (partition (lambda (erp-entry) (tape? (rest erp-entry))) (trace-container->erps q))])
               (for-each display (list "leapfrog step, momentum: " p "\n    real-world: " real-world "\n    discrete-world " discrete-world "\n"))))
           (let*-values ([ (p-half) (list+ p (scalar* (/ dt 2) (grad-score q)))]
                         [ (q-next fw-prob rev-prob) (increment-real-world q (scalar* dt (list* m-inv p-half)) env)]
                         [ (p-next) (list+ p-half (scalar* (/ dt 2) (grad-score q-next)))])
             (values q-next p-next fw-prob rev-prob)))
         
         (define (grad-score tc) (map rest (trace-container->gradient tc)))
         (define (scalar* sc lst) (map (lambda (x) (* sc x)) lst))
         (define (list+ a b) (map + a b))
         (define (list* a b) (map * a b))
         
         ;;this helper function takes a trace-container and increments its real-erp values by a given amount, returning new tc and any f/b-probs
         (define (increment-real-world tc increment env)
           (let-values ([(real-world discrete-world) (partition (lambda (erp-entry) (tape? (rest erp-entry))) (trace-container->erps tc))])
             (let* ((real-world-names (map first real-world))
                    (real-world-vals (map rest real-world))
                    (new-real-world-vals (map + real-world-vals increment))
                    (new-real-world (map pair real-world-names new-real-world-vals)))
               (parameterize ([*fixed-erps* new-real-world])
                 (church-eval tc env 'propose new-real-world)))))

         
         (when (> (*verbosity*) 12)
           (display "loaded gradient-query\n"))

         )
