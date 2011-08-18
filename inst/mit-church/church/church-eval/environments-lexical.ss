#!r6rs

;; created Jun 27, 2008 by noah goodman
;; authors: noah goodman, daniel roy

(library (church church-eval environments-lexical)
         
         (export enclosing-environment
                 address-in-enclosing-environment
                 first-frame
                 the-empty-environment
                 make-frame
                 frame-variables
                 frame-values
                 frame-unchanged
                 first-frame-values
                 extend-environment
                 lookup-variable-value-and-id
                 lookup-value-by-id
                 lookup-variable-value
                 add-binding-to-frame!
                 define-variable!
                 set-variable-value!
                 relative-env-equiv
                 frames-equiv?)
         
         (import (church utils rnrs)
                 (church readable-scheme)
                 (_srfi :1)
                 (_srfi :43)
                 )
         
         ;; this is a version of environment handling code that uses lexical addressing.
         
         (define (enclosing-environment env) (mtail env))
         
         ;;adjust a lexical address for passing through a labmda: pop top frame.
         ;;if frame was already 0 (first frame), return null.
         (define (address-in-enclosing-environment lexical-address)
           (if (not (pair? lexical-address)) ; if its not a lexical address, just return it...
               lexical-address
               (if (= (head lexical-address) 0)
                   false
                   (pair (- (head lexical-address) 1) (tail lexical-address)))))
         
         (define (first-frame env) (mhead env))
         
         (define the-empty-environment (list)) ;; we leave this a list so that mutation fails, as it should
         
         (define (make-frame variables values unchanged)
           (vector (list->vector variables) (list->vector values) (list->vector unchanged)))
         
         (define (frame-variables frame) (vector-ref frame 0))
         (define (frame-values frame) (vector-ref frame 1))
         (define (frame-unchanged frame) (vector-ref frame 2))
         
         (define (first-frame-values env)
           (vector->list (frame-values (first-frame env))))
         
         (define (extend-environment vars vals base-env . unchanged)
           (let ((unchanged (if (null? unchanged) 
                                (make-list (length vals) false)
                                (head unchanged))))
             (mpair (make-frame vars vals unchanged) base-env)))
         
         
         ;;this returns both the value and the address as a pair (val . addr), where address is (frame-loc . var-loc)
         (define (lookup-variable-value-and-id var top-env)
           (let ((frame-loc 0))
             (define (env-loop env)
               (define (find-var vars vals)
                 (let ((var-index (vector-index (lambda (v) (eq? v var)) vars)))
                   (if (not var-index);ie. (false? var-index)
                       (begin (set! frame-loc (+ frame-loc 1))
                              (env-loop (enclosing-environment env)))
                       (pair (vector-ref vals var-index) (pair frame-loc var-index)))))
               (if (eq? env the-empty-environment)
                   (raise-continuable "Unbound variable")
                   (let ((frame (first-frame env)))
                     (find-var (frame-variables frame)
                               (frame-values frame) ))))
             (env-loop top-env) ))
         
         ;;this uses a lexical address to get a value in an environment, returns both the value and the address
         (define (lookup-value-by-id address env)
           (pair (vector-ref (frame-values (mlist-ref env (head address)))
                             (tail address))
                 address))
         
         (define (lookup-variable-value var env)
           (let ((value-and-id (lookup-variable-value-and-id var env)))
             (if (fail? value-and-id) 'fail (head value-and-id))))
         
         ;;;;below here are the parts that use mutation (used only for define)
         
         (define (add-binding-to-frame! var val frame)
           (vector-set! frame 0 (vector-append (vector var) (frame-variables frame)))
           (vector-set! frame 1 (vector-append (vector val) (frame-values frame)))
           (vector-set! frame 2 (vector-append (vector false) (frame-unchanged frame)))) ;;defined variables are treated as possibly changed.
         
         ;;this is used in trace-eval.ss to implement define..
         (define (define-variable! var val env)
           (let* ((frame (first-frame env))
                  (var-index (vector-index (lambda (v) (eq? v var)) (frame-variables frame))))
             (if (not var-index);ie. (false? var-index)
                 (add-binding-to-frame! var val frame)
                 (begin (vector-set! (frame-values frame) var-index val)
                        (vector-set! (frame-unchanged frame) var-index false)))))
         
         ;;this one is used only in trace-constraint-propagation.ss
         (define (set-variable-value! var val env)
           (define (env-loop env)
             (define (set-var vars vals)
               (let ((var-index (vector-index (lambda (v) (eq? v var)) vars)))
                 (if (not var-index);ie. (false? var-index)
                     (env-loop (enclosing-environment env))
                     (vector-set! vals var-index val))))    
             (if (eq? env the-empty-environment)
                 (error "Unbound variable -- SET!" var)
                 (let ((frame (first-frame env)))
                   (set-var (frame-variables frame)
                            (frame-values frame)))))
           (env-loop env))
         
         
         ;;;;tools for checking environment equivalence relative to a set of bindings
         
         ;relevant-bindings should be a list of lists the same length as new-env, each of which is the index of variable bindings to pay attention to.
         (define (relative-env-equiv new-env old-env relevant-bindings)
           (if (or (null? relevant-bindings) (eq? new-env old-env))
               true
               (if (or (eq? new-env the-empty-environment) (eq? old-env the-empty-environment))
                   false
                   (if (frames-equiv? (frame-values (first-frame new-env)) 
                                      (frame-values (first-frame old-env))
                                      (frame-unchanged (first-frame new-env))
                                      (first relevant-bindings))
                       (relative-env-equiv (enclosing-environment new-env)
                                           (enclosing-environment old-env)
                                           (tail relevant-bindings))
                       false))))
         
         ;set this to false for less fine-grained, but possibly faster eq?-based env-equiv.
         (define CHECK-VALUES-WITH-EQUAL true)
         
         (define (frames-equiv? new-frame-values old-frame-values unchanged relevant-indices)
           (if (null? relevant-indices)
               true
               (if (or (vector-ref unchanged (first relevant-indices)) ;;this uses binding-time eq check....
                       (if CHECK-VALUES-WITH-EQUAL
                           (equal? (vector-ref new-frame-values (first relevant-indices)) 
                                   (vector-ref old-frame-values (first relevant-indices)))
                           false)
                       )
                   (frames-equiv? new-frame-values old-frame-values unchanged (tail relevant-indices))
                   false)))
         
         
)