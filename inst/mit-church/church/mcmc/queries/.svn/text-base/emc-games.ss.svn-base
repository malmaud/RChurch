#!r6rs

(library (church mcmc queries emc-games)

         (export make-evolutionary-tempered-kernel
                 emc-lex-query)
         
         (import (_srfi :1)
                 (_srfi :43)
                 (_srfi :69)
                 (church external math-env)
                 (church utils rnrs)
                 (church church-eval environments-lexical)
                 (church church-eval logmath)
                 (church church-eval traces)
                 (church church-eval laziness)
                 (church readable-scheme)
                 (church church-eval church-eval)
                 (church church-eval syntax)
                 (church mcmc mcmc-core)
                 (church mcmc queries temperature-games))
         
         (define (emc-lex-query in-vars ladders N M L lex-exp query-exp cond-exp env)
           (let ((zipped-ladders (if (eq? (first in-vars) 'posterior-temperature) 
                                     (apply zip ladders) 
                                     (apply zip (cons (make-list (length (first ladders)) 1) ladders))))
                 (vars (if (eq? (first in-vars) 'posterior-temperature) in-vars (cons 'posterior-temperature in-vars)))
                 (normal-form-query-exp (convert-lex lex-exp query-exp cond-exp))) ;; FIXME: allow crossovers at the root node
             (let ((count -1)
                   (samples '()))
               (mcmc-query-core (make-parallel-initializer vars zipped-ladders)
                                (make-evolutionary-tempered-kernel vars zipped-ladders M L env)
                                (lambda (trace) 
                                  (if (= N -1)
                                      (begin (set! count 0) false)
                                      (if (< count N)
                                          (begin (set! count (+ count 1))
                                                 (set! samples (append samples (list (keep-first-parallel-finish trace))))
                                                 false)
                                          true)))
                                (lambda (trace) samples)
                                normal-form-query-exp
                                (extend-environment vars (map (lambda (x) +nan.0) vars) env) ) )))
                               
         (define (make-evolutionary-tempered-kernel vars zipped-ladders M L env . visitor)
           (if (null? visitor)
               (repeat-kernel L (cycle-kernel (repeat-kernel M (make-parallel-kernel vars zipped-ladders env))
                                              (make-crossover-kernel vars zipped-ladders env)))
               (repeat-kernel L (cycle-kernel (repeat-kernel M (cycle-kernel (parallel-visitor (first visitor)) (make-parallel-kernel vars zipped-ladders env)))
                                              (make-crossover-kernel vars zipped-ladders env)))))
         
         (define (make-crossover-kernel vars zipped-ladders env)
           (let ((kernel-name (make-kernel-name "crossover-kernel" (zip vars (apply zip zipped-ladders)))))
             (make-mh-kernel (make-crossover-proposal vars zipped-ladders env) (make-parallel-scorer zipped-ladders) kernel-name)))
         
         ;;;simplest crossover policy -- crossover at any expression
         ;(define (registry-filter-fn trace) true)
         
         ;;;;somewhat smarter policy -- don't crossover at leaf nodes (which are variable lookup, etc, but not erps, which have a an apply subnode):
         ;(define (registry-filter-fn trace) (not (null? (get-subtraces trace))))
         
         ;;;better version of smarter policy -- don't crossover at which dominate no erps:
         ;(define (registry-filter-fn trace) (not (null? (get-dominated-erps trace))))
         
         ;;;explicit marking policy -- only crossover at expressions wrapped in (crossover-here ....):
         (define (registry-filter-fn trace) (and (syntax:application? (get-syntax trace)) (eq? (first (get-original-expr trace)) 'crossover-here) ))
         
         ;;FIXME: write down details of math for this kernel -- show that multiple-paths isn't a problem.... (does this men that subtrace re-gen at internal node is ok too?)
         ;;TODO: speed this up by maintaining registry in trace-eval, and by targeting the church-eval update to the swapped subtraces.
         ;;NOTE: at the moment this does crossover only at nodes with same lexical context.
         (define (make-crossover-proposal vars zipped-ladders env)
           (lambda (traces) 
             (when (or (*mh-steps*) (*global-debug*)) (for-each display (list "Attempting crossover...\n")))
             (let* ((swap-at (uniform-draw (iota (- (length traces) 1) 1)))
                    (bottom-trace (list-elt traces swap-at))
                    (top-trace (list-elt traces (+ 1 swap-at)))
                    (bottom-env (extend-environment vars (list-elt zipped-ladders swap-at) env))
                    (top-env (extend-environment vars (list-elt zipped-ladders (+ swap-at 1)) env))
                    (subtraces-and-paths (select-subtraces-for-crossover (fill-registry bottom-trace (make-hash-table) '() registry-filter-fn) 
                                                                         (fill-registry top-trace (make-hash-table) '() registry-filter-fn)    ))
                    (prop-bottom-trace (church-eval (deep-replace-subtrace bottom-trace (first (second subtraces-and-paths)) (rest (first subtraces-and-paths)))
                                                    bottom-env
                                                    'force-complete-eval true)) ;;given path to subtrace, can make this more refined....
                    (prop-top-trace (church-eval (deep-replace-subtrace top-trace (first (first subtraces-and-paths)) (rest (second subtraces-and-paths)))
                                                 top-env
                                                 'force-complete-eval true))
                    (prop-bottom-trace-f/b (proposal->forward/backward-score prop-bottom-trace))
                    (prop-top-trace-f/b (proposal->forward/backward-score prop-top-trace))
                    (forw-prob (+ (force-lrb (forward/backward-score->forward-score prop-bottom-trace-f/b)) 
                                  (force-lrb (forward/backward-score->forward-score prop-top-trace-f/b)) ))
                    (subtraces-forw-prob (third subtraces-and-paths))
                    (rev-prob (+ (force-lrb (forward/backward-score->backward-score prop-bottom-trace-f/b)) 
                                 (force-lrb (forward/backward-score->backward-score prop-top-trace-f/b)) ))
                    ;;this is ridiculously wasteful -- should fill registries on church-eval, so don't have to walk traces extra times...
                    (subtraces-rev-prob (third (select-subtraces-for-crossover (fill-registry (proposal->trace prop-bottom-trace) (make-hash-table) '() registry-filter-fn)
                                                                               (fill-registry (proposal->trace prop-top-trace) (make-hash-table) '() registry-filter-fn))))
                    )
               (make-move (+ forw-prob subtraces-forw-prob) 
                          (+ rev-prob subtraces-rev-prob)
                          (append (take traces (- swap-at 1)) 
                                  (list (proposal->trace prop-bottom-trace)
                                        (proposal->trace prop-top-trace))
                                  (drop traces (+ swap-at 1)))))))
         
         ;;return a list of  ((first-subtrace . path-to-first-subtrace) (scond-subtrace . path-to-second-subtrace) log-prob-of-choices).
         ;;each registry is a hash table with expression keys whose entries are lists of (node . path-to-node) with that expression.
         ;;this function selects a pair of nodes with the same expression uniformly at random (to do so make a list for each registry of number of nodes for each expression, then use the product to choose a pair).
         ;;FIXME: this doesn't look through memoized procedures, so doesn't make crossover proposals within mem....
         (define (select-subtraces-for-crossover registry1 registry2)
           (let* ((al1 (hash-table->alist registry1))
                  (traces1 (map (lambda (key-val) (rest key-val)) al1))
                  (traces2 (map (lambda (key-val) (hash-table-ref registry2 (first key-val) (lambda () '()))) al1))
                  (counts1 (map (lambda (tr) (length tr)) traces1))
                  (counts2 (map (lambda (tr) (length tr)) traces2))
                  (pairs (map * counts1 counts2))
                  (total-pairs (apply + pairs)))
             (if (= 0 total-pairs)
                 (list '() '() LOG-PROB-0) ;;what to do if there are no possible crossovers? this is fine in rescoring, but not so good in proposing.
                 (let* ((probs (map (lambda (x) (/ x total-pairs)) pairs))
                        (selected-expression (sample-discrete probs)))
                   ;(when (or (*mh-steps*) (*global-debug*)) (for-each display (list "  Crossover at nodes with expression: " (first (list-ref al1 selected-expression))  "\n")))
                   ;(display counts1)(newline) (display counts2)(newline) (display pairs)(newline)(newline)
                   (list (uniform-draw (list-ref traces1 selected-expression)) (uniform-draw (list-ref traces2 selected-expression)) (- (log total-pairs)))
                   ))))
         
         ;;;log-probability of choosing a given pair -- they are chosen uniformly, so just -log(number of possibilities).
         ;(define (probability-of-subtrace-choices registry1 registry2)
         ;  (let* ((al1 (hash-table->alist registry1))
         ;         (traces1 (map (lambda (key-val) (rest key-val)) al1))
         ;         (traces2 (map (lambda (key-val) (hash-table-ref registry2 (first key-val) (lambda () '()))) al1))
         ;         (counts1 (map (lambda (tr) (length tr)) traces1))
         ;         (counts2 (map (lambda (tr) (length tr)) traces2))
         ;         (pairs (map * counts1 counts2))
         ;         (total-pairs (apply + pairs)))
         ;    (- (log total-pairs))) 
         ;  )
         

         ;;walk down a trace, building a hash table with (raw) expression-id keys, that contains a list of (pointers to) nodes with that name, and for each node the path to that node in the trace.
         ;;filter-fn is a trace-->bool function used to decide which nodes should be out in registry (and therefore available for crossover).
         (define (fill-registry trace registry path-so-far filter-fn)
           (let ((paths (map (lambda (i) (append path-so-far (list i))) (iota (length (get-subtraces trace))))))
             (when (and (is-eval-node? trace) (filter-fn trace))
               (hash-table-update!/default registry 
                                           (syntax->id (get-syntax trace))  ;index by syntax lexical id -- thus crossover at node with same position in program.
                                           (lambda (entry) (pair (pair trace path-so-far) entry))
                                           '()))
             (map (lambda (t p) (fill-registry t registry p filter-fn)) (get-subtraces trace) paths)
             registry))
         
         ;;replace a trace at a given location deep in the trace
         (define (deep-replace-subtrace trace new-subtrace path-to-old-subtrace)
           (if (null? path-to-old-subtrace)
               new-subtrace
               (pure-replace-subtrace (+ (first path-to-old-subtrace) 1) ;;because paths are 0-indexed
                                      (deep-replace-subtrace (get-subtrace (first path-to-old-subtrace) trace) 
                                                             new-subtrace (rest path-to-old-subtrace))
                                      trace)))
         
         ;;copy the trace, then replace the subtrace.
         (define (pure-replace-subtrace n new-subtrace trace)
           (let ((trace (vector-copy trace)))
             (vector-set! trace 7 
                          (append (take (get-subtraces trace) (- n 1)) (list new-subtrace) (drop (get-subtraces trace) n)) )
             trace))
                 
         (when (> (*verbosity*) 12)
           (display "loaded evolutionary tempering (emc-query from emc-games.ss)\n"))
         
)