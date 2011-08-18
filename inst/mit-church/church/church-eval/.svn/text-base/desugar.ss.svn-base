#!r6rs

(library (church church-eval desugar)
         
         (export de-sugar
                 de-sugar-all
                 register-sugar!)
         
         (import (church utils rnrs)
                 (church readable-scheme)
                 (church external include-paths)
                 (_srfi :1))
         
         
         ;;;;-----------
         ;;;Gimme some sugar!
         
         ;;de-sugaring code:
         (define sugar-registry '())
         (define (register-sugar! pattern translator)
           (set! sugar-registry (cons (cons pattern translator) sugar-registry)) )
         (define sugar-pattern car)
         (define sugar-translator cdr)
         
         (define (de-sugar expr)
           (define unchanged (gensym))
           (define (try expr sugar-list)
             (if (null? sugar-list)
                 unchanged
                 (if ((sugar-pattern (first sugar-list)) expr)
                     ((sugar-translator (first sugar-list)) expr)
                     (try expr (rest sugar-list)) )))
           (let loop ((expr expr))
             (let ((new-expr (try expr sugar-registry)))
               (if (eq? new-expr unchanged)
                   expr
                   (loop new-expr) ))))

         (define (de-sugar-all sexpr)
           (let ((new-sexpr (de-sugar sexpr)))
             (if (list? new-sexpr)
                 (map de-sugar-all new-sexpr)
                 new-sexpr)))

         ;; (begin ...)
         
         (define (begin-wrap exprs)
           (if (null? (rest exprs))
               (first exprs)
               `(begin ,@exprs)))
         ;; (begin ...) is now a special form!
         ;(define (desugar-begin expr)
         ;  (last expr))
         ;(register-sugar begin? desugar-begin)
         
         ;; (let (var-bindings) expr1 ... exprN)
         (define (let? expr) (and (tagged-list? expr 'let) (list? (second expr))))
         (define (let->lambda expr) 
           (let* ((bindings (second expr))
                  (vars (map first bindings))
                  (value-exprs (map second bindings))
                  (body (begin-wrap (drop expr 2))))
             `((lambda ,vars ,body) ,@value-exprs) ))
         
         ;; (let loop (var-bindings) expr1 ... exprN)
         
         (define (named-let? expr) (and (tagged-list? expr 'let) (symbol? (second expr))))
         
         (define (named-let->lambda expr)
           (let* ((proc-name (second expr))
                  (let-conversion (let->lambda (rest expr))))
             `((Y (lambda (,proc-name) ,(first let-conversion))) ,@(rest let-conversion)) ))
         
         ;; (let* ...)
         (define (let*? expr) (tagged-list? expr 'let*))
         (define (desugar-let* expr) 
           (let ((bindings (second expr))
                 (body (begin-wrap (drop expr 2))))
             (if (null? bindings)
                 body
                 (let* ((binding (first bindings))
                        (var (first binding))
                        (value-exprs (second binding)) )
                   `((lambda (,var) (let* ,(rest bindings) ,body)) ,value-exprs) ))))
         
         ;; (case ...)
         (define (case? expr) (tagged-list? expr 'case))
         (define (desugar-case expr)
           (let ((key-symbol (gensym))
                 (key-expr (second expr))
                 (value-exprs (drop expr 2)) )
             `(let ((,key-symbol ,key-expr))
                (cond ,@(map (lambda (value-expr) 
                               (let ((values (first value-expr))
                                     (val-expr (rest value-expr)) )
                                 (cond ((list? values)
                                        `((any (list ,@(map (lambda (val) `(equal? ,key-symbol ,val)) values) ))
                                          ,@val-expr ) )
                                       ((equal? values 'else)
                                        `(else ,@val-expr) )
                                       (else (error "Invalid case expression." values)) ) ))
                             value-exprs ))) ))
         
         ;; (cond ...)
         (define (cond? expr) (tagged-list? expr 'cond))
         (define (desugar-cond expr)
           (let loop ((conditions (rest expr)))
             (if (null? conditions)
                 '(void)
                 (let* ((condition (first conditions))
                        (test (first condition)))
                   (if (equal? test 'else)
                       (if (not (null? (rest conditions)))
                           (error "else clause in cond expression must be last.")
                           (begin-wrap (rest condition)) )
                       `(if ,test 
                            ,(begin-wrap (rest condition))
                            ,(loop (rest conditions)) ) )))))

         ;; (mem-rec A (lambda args body))
         ;;     mem-rec is sugar for creating a memoized recursive function
         (define (mem-rec? expr) (tagged-list? expr 'mem-rec))
         (define (desugar-mem-rec expr)
           (if (not (eq? (first (third expr)) 'lambda))
               (error "second argument to mem-rec must be lambda expression.") 
               (let ((fn-symbol (second expr))
                     (fn-args (second (third expr)))
                     (fn-body (third (third expr)))
                     (F (gensym))
                     (G (gensym)))
                 `(let ((,F
                         (mem
                          (lambda ,(pair G fn-args)
                            (let ((,fn-symbol (lambda ,fn-args (,G ,G ,@fn-args))))
                              ,fn-body)))))
                    (lambda ,fn-args (,F ,F ,@fn-args))))))

         ;;define sugar: (define (foo x y) ...)
         (define (define-fn? expr) (and (tagged-list? expr 'define) (not (symbol? (second expr)))))
         (define (desugar-define-fn expr)
           (let ((def-var (first (second expr)))
                 (def-params (rest (second expr)))
                 (def-body (rest (rest expr))))
           `(define ,def-var (lambda ,def-params ,@def-body))))


         ;;load sugar.
         (define (seq-with-load? expr) (and (list? expr)
                                            (fold (lambda (subexpr accum) (or (tagged-list? subexpr 'load) accum)) false expr)))
         (define (expand-loads expr)
           (apply append (map (lambda (subexpr) (if (load? subexpr) (file->list (open-included-file (second subexpr))) (list subexpr))) expr)))
         (define (file->list filehandle)
           (let ((next (read filehandle)))
             (if (eof-object? next) '() (cons next (file->list filehandle)))))
         (define (load? expr) (tagged-list? expr 'load))

         
         (define (rejection? expr) (tagged-list? expr 'rejection-query))
         (define (desugar-rejection expr)
           `(nfqp-rejection-query (lambda () (begin ,@(drop-right (rest expr) 2)
                                                    (pair ,(list-ref expr (- (length expr) 2)) ,(last expr))))))
         
         
         ; @form (let ((var val) ...) expr ...)
         ; @desc
         ; Let binds variables in the scope of the body of the let.
         ; @param assignments An expression '((var val) ...)
         ; @param exprs Body expressions that are evaluated within the environment where variables are assigned.
         ; @return the result of evaluating the last body expr
         (register-sugar! let? let->lambda)
         
         ; @form (let* ((var val) ...) expr ...)
         ; @desc
         ; Let* binds variables in the scope of the body of the let.
         ; Each assignment has access to the variables bound earlier on in the same let*.
         ; @param assignments An expression '((var val) ...)
         ; @param exprs Body expressions that are evaluated within the environment where variables are assigned.
         ; @return the result of evaluating the last body expr
         (register-sugar! let*? desugar-let*)

         (register-sugar! named-let? named-let->lambda)         
         (register-sugar! case? desugar-case)
         (register-sugar! cond? desugar-cond)
         (register-sugar! mem-rec? desugar-mem-rec)

         (register-sugar! define-fn? desugar-define-fn)
         (register-sugar! seq-with-load? expand-loads)

         (register-sugar! rejection? desugar-rejection)

)