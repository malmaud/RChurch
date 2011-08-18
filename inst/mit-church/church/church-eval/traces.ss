#!r6rs

(library (church church-eval traces)

         (export is-trace?
                 is-eval-node?
                 is-apply-node?
                 verify-trace
                 
                 ;;building traces:
                 build-eval-node                 
                 build-apply-node
                 update-eval-node                 
                 update-apply-node
                 delete-trace-node
                 Zero-probability-proposal-exception-node
                 
                 ;;manipulating traces:
                 replace-subtrace!
                 
                 ;;looking at trace nodes:
                 get-expr
                 get-original-expr
                 get-syntax
                 get-env
                 get-operator
                 get-operands
                 trace->value
                 get-val
                 trace->score
                 get-score
                 get-dominated-erps
                 get-subtraces
                 get-subtrace
                 get-name
                                  
                 ;;annoying adts:
                 make-proposal
                 is-proposal?
                 proposal->value
                 proposal->trace
                 proposal->forward/backward-score
                 verify-proposal
                 make-forward/backward-score
                 is-forward/backward-score?
                 forward/backward-score->forward-score
                 forward/backward-score->backward-score
                 verify-score)
         
         (import (church utils rnrs)
                 (church readable-scheme)
                 (church church-eval laziness)
                 (church church-eval logmath)
                 (church church-eval syntax)
                 (church church-eval environments-lexical)
                 (_srfi :43))

         (define (is-trace? t) 
           (and (vector? t)
                (> (vector-length t) 0)
                (or (eq? (vector-ref t 0) 'eval-node)
                    (eq? (vector-ref t 0) 'apply-node) 
                    (eq? (vector-ref t 0) 'mem-node) 
                    ) ))

         (define (is-eval-node? t) 
           (and (is-trace? t)
                (eq? (vector-ref t 0) 'eval-node) ))
         
         (define (is-apply-node? t) 
           (and (is-trace? t)
                (eq? (vector-ref t 0) 'apply-node) ))
         
         (define (is-mem-node? t) 
           (and (is-trace? t)
                (eq? (vector-ref t 0) 'mem-node) ))
         
         (define (verify-trace trace)
           (assert-with-message (lambda () (is-trace? trace))
                   (lambda () "expecting trace") ))
         
;         (define UNINITIALIZED-VAL (gensym))
         
;         (define (is-trace-initialized? trace)
;           (not (eq? (get-val trace) UNINITIALIZED-VAL)))
         
         ;;;traces are built out of eval-nodes, apply-nodes, and mem-nodes.
         ;eval nodes:
         (define (build-eval-node-long expr env val score dominated-erps subtraces specific-info dispatch-fn name/path)
           (assert-with-message (lambda () (not (is-trace? val))) (lambda () "traces arent values!") )
           (assert-with-message (lambda () (syntax? expr)) (lambda () "expected syntax"))
           (vector 'eval-node expr env val score dominated-erps '() subtraces specific-info dispatch-fn name/path))
         
         (define (build-eval-node in-expr env val subtraces . dominated-erps)
           (let ((dominated-erps (if (null? dominated-erps)
                                     (eager-append (map get-dominated-erps subtraces))
                                     (first dominated-erps))))
             (build-eval-node-long in-expr
                                   env val 
                                   (eager-sum (map trace->score subtraces)) 
                                   dominated-erps
                                   subtraces
                                   '() ;;deprecated: specific info
                                   '() ;;deprecated: dispatch fn
                                   (gensym 'tracenode) ;;name -- not currently used except in visualization
                                   )))
         
         (define update-eval-node (lambda args (apply build-eval-node (rest args)))) ;;for tree-based traces updating is same as building.
         
         (define Zero-probability-proposal-exception-node
           (build-eval-node-long (make-syntax 'zero-probability-proposal-exception 
                                              'zero-probability-proposal-exception 
                                              'zero-probability-proposal-exception)
                                 '() 
                                 (pair 'zero-probability-proposal-exception false)
                                 LOG-PROB-0 
                                 '() '() '() '() '()))
         
         ;;aplication nodes:
         (define (build-apply-node operator operands val score subtraces . dominated-erps) 
           (assert-with-message (lambda () (not (is-trace? val)))
                   (lambda () "traces aren't values") )
           (let ((dominated-erps (if (null? dominated-erps)
                                     (eager-append (map get-dominated-erps subtraces))
                                     ;(begin (when (list? operator) (display (first operator)))(write (first dominated-erps)) (newline)
                                       (first dominated-erps))));)
             (vector 'apply-node operator operands val score
                     dominated-erps
                     '()  
                     subtraces '() '() (gensym 'tracenode))))
         
         (define update-apply-node (lambda args (apply build-apply-node (rest args)))) ;;for tree-based traces updating is same as building.
             
         
         (define (delete-trace-node trace) 'hi-mom) ;;nothing need be done on deletion - node will be GCed. 
         
         
         ;changes to existing trace nodes:
                  
         ;this is dangerous! currently used only in constraint-propogation (and should keep it that way).
         (define (replace-subtrace! n new-subtrace trace)
           (vector-set! trace 7 
                        (append (take (get-subtraces trace) (- n 1)) (list new-subtrace) (drop (get-subtraces trace) n)) )
           trace)
         
         
         ;get info from the root node of a trace:
         ;i'm a little sloppy about not casing on apply-node vs eval-node, since they store most info in the same places....
               
         (define (get-expr node)
           (assert-with-message (lambda () (is-eval-node? node)) (lambda () "expecting eval node"))
           (syntax->expr (vector-elt node 2)) )
         
         (define (get-original-expr node)
           (assert-with-message (lambda () (is-eval-node? node)) (lambda () "expecting eval node"))
           (syntax->original-expr (vector-elt node 2)) )
         
         (define (get-syntax node)
           (assert-with-message (lambda () (is-eval-node? node)) (lambda () "expecting eval node"))
           (vector-elt node 2) )
         
         (define (get-env node)
           (assert-with-message (lambda () (is-eval-node? node)) (lambda () "expecting eval node"))
           (vector-elt node 3))
         
         (define (get-operator node)
           (assert-with-message (lambda () (is-apply-node? node)) (lambda () "expecting apply node"))
           (vector-elt node 2))
         
         (define (get-operands node)
           (assert-with-message (lambda () (is-apply-node? node)) (lambda () "expecting apply node"))
           (vector-elt node 3))
         
         (define (trace->value trace) 
           (verify-trace trace)
           (vector-elt trace 4) )
         (define get-val trace->value)
         
         (define (trace->score trace) 
           (verify-trace trace)
           (vector-elt trace 5) )
         (define get-score trace->score)
         
         (define (get-dominated-erps node)
           (verify-trace node)
           (vector-elt node 6))
         
         (define (get-subtraces node)
           (verify-trace node)
           (vector-elt node 8))
         
         (define (get-subtrace n node)
           (verify-trace node)
           (list-ref (vector-elt node 8) n))
         
;         (define (get-type-specific-info node)
;           (verify-trace node)
;           (vector-elt node 9))
         
;         (define (get-existing-dispatch-fn node) 
;           (verify-trace node)
;           (vector-elt node 10))
         
         (define (get-name node) 
           (verify-trace node)
           (vector-elt node 11))
         
         
;         ;this just makes it easier to look at traces:
;         (define (trace-map op trace)
;           (if (null? (get-subtraces trace))
;               (op trace)
;               (list (op trace) (map (lambda (t) (trace-map op t)) (get-subtraces trace))))) 
         
         
         
         
         
         ;;;;;;proposal manipulators.
         ;; opaque ADT using procedures to hide structure
         (define (make-proposal trace forward/backward-score)
           (verify-score forward/backward-score)
           (verify-trace trace)
           (lambda (message)
             (cond ((eq? message 'get-type) 'proposal)
                   ((eq? message 'get-trace) trace)
                   ((eq? message 'get-score) forward/backward-score)
                   (else (error "invalid access")) )))
         (define (is-proposal? p) (and (procedure? p) (eq? (p 'get-type) 'proposal)))
         (define (proposal->trace p) (verify-proposal p) (p 'get-trace))
         (define (proposal->forward/backward-score p) (verify-proposal p) (p 'get-score))
         (define (proposal->value p) (trace->value (proposal->trace p)))         
         
         (define (verify-proposal p)
           (assert-with-message (lambda () (is-proposal? p))
                   (lambda () "not proposal") )) 
          
         (define (make-forward/backward-score f b)
           (assert-with-message (lambda () (and (or (number? f) (lazy-reduce-box? f)) (or (number? b) (lazy-reduce-box? b)) ))
            ;(lambda () (and (or (number? f) (procedure? f))
             ;                                   (or (number? b) (procedure? f)) ))
                   (lambda () "mal-formed score")
                   (lambda () (for-each display (list "args: " f "," b))) )
           (list 'score f b))
         (define (is-forward/backward-score? s) (tagged-list? s 'score))
         (define (forward/backward-score->forward-score s) (verify-score s) (second s))
         (define (forward/backward-score->backward-score s) (verify-score s) (third s))
         (define (verify-score s)
           (assert-with-message (lambda () (is-forward/backward-score? s))
                   (lambda () "not score") ))
         
)