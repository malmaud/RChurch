#!r6rs

;; authors: noah goodman, daniel roy

(library (church church-eval syntax)

         (export syntax:self-evaluating?
                 self-evaluating?
                 syntax:quoted?
                 quoted?
                 ;text-of-quotation
                 quote-syntax->text-of-quotation
                 variable?
                 syntax:variable?
                 begin?
                 syntax:begin?
                 definition?
                 syntax:definition?
                 definition-syntax->definition-variable
                 definition-syntax->definition-value
                 ;definition-variable
                 ;definition-value
                 lambda?
                 syntax:lambda?
                 lambda-syntax->lambda-parameters
                 lambda-syntax->lambda-body
                 ;lambda-parameters
                 ;lambda-body
                 make-lambda
                 if?
                 syntax:if?
                 ;if-predicate
                 if-syntax->if-predicate
                 if-syntax->if-consequent
                 if-syntax->if-alternative
                 get-env?
                 syntax:get-env?
                 ;if-alternative
                 ;make-if
                 application?
                 syntax:application?
                 ;operator
                 ;operands
                 ;no-operands?
                 ;first-operand
                 ;rest-operands
                 make-syntax
                 sexpr->syntax
                 syntax->type
                 syntax->id
                 syntax->details
                 syntax->original-expr
                 syntax->expr
                 syntax?
                 extract-defined-vars
                 write-with-syntax-ids)
                          
         (import (church utils rnrs)
                 (_srfi :1) ; lists
                 (church readable-scheme)
                 (church church-eval desugar)
                 (church church-eval environments-lexical)
                 (church external include-paths))
         
         ;; syntax ADT
         (define (make-syntax type original-expr expr . type-specific)
           (vector 'syntax type (gensym) type-specific original-expr expr) )
         (define (syntax? s) (and (vector? s) (eq? 'syntax (vector-ref s 0))))
         (define (syntax->type s) (vector-ref s 1)) 
         (define (syntax->id s) (vector-ref s 2))
         (define (syntax->details s) (vector-ref s 3))
         (define (syntax->original-expr s) (vector-ref s 4))
         (define (syntax->expr s) (vector-ref s 5)) ;;this is a list of syntax objects comming from the original sub-exprs.
         
         (define (syntax:is-type sym) (lambda (sobj) (eq? sym (syntax->type sobj))))
         (define syntax:self-evaluating? (syntax:is-type 'self-evaluating))
         (define syntax:quoted? (syntax:is-type 'quoted))
         (define syntax:variable? (syntax:is-type 'variable))
         (define syntax:begin? (syntax:is-type 'begin))
         (define syntax:definition? (syntax:is-type 'definition))
         (define syntax:lambda? (syntax:is-type 'lambda))
         (define syntax:if? (syntax:is-type 'if))
         (define syntax:get-env? (syntax:is-type 'get-env))
         (define syntax:application? (syntax:is-type 'application))
         
         (define (self-evaluating? exp)
           (cond ((number? exp) true)
                 ((string? exp) true)
                 ((char? exp) true)
                 ((eq? exp 'mem) true) ;;mem, eval, apply, create-xrp are treated as self-evaluating, but application is treated specially.
                 ((eq? exp 'eval) true)
                 ((eq? exp 'apply) true)
                 ((eq? exp 'make-xrp) true)
                 ((equal? exp '()) true) 
                 (else false)))
         
         (define (quoted? exp)
           (tagged-list? exp 'quote))
         
         (define (text-of-quotation exp) (cadr exp))
         (define (quote-syntax->text-of-quotation syntax) (text-of-quotation (syntax->expr syntax)))
         
         (define (variable? exp) (symbol? exp))
         
         (define (begin? expr)  (tagged-list? expr 'begin))
         
         (define (definition? exp)
           (tagged-list? exp 'define))
         
         (define (definition-variable exp)
           (if (symbol? (cadr exp))
               (cadr exp)
               (caadr exp)))
         (define (definition-syntax->definition-variable syntax) (second (syntax->expr syntax)))           
         
         (define (definition-value exp)
           (if (symbol? (cadr exp))
               (caddr exp)
               (make-lambda (cdadr exp)
                            (cddr exp))))
         (define (definition-syntax->definition-value syntax) (third (syntax->expr syntax)))        
         
         
         (define (lambda? exp) (tagged-list? exp 'lambda))
         
         (define (lambda-parameters exp) (cadr exp))
         (define (lambda-body exp) (caddr exp))
         (define (lambda-syntax->lambda-parameters syntax) (lambda-parameters (syntax->expr syntax)))
         (define (lambda-syntax->lambda-body syntax) (lambda-body (syntax->expr syntax)))
         
         (define (make-lambda parameters body)
           (cons 'lambda (cons parameters body)))
         
         (define (if? exp) (tagged-list? exp 'if))
         
         (define (if-predicate exp) (cadr exp))
         (define (if-syntax->if-predicate syntax) (if-predicate (syntax->expr syntax)))
         
         (define (if-consequent exp) (caddr exp))
         (define (if-syntax->if-consequent syntax) (if-consequent (syntax->expr syntax)))
         
         (define (get-env? exp)
           (tagged-list? exp 'get-current-environment))
         
         (define (if-alternative exp)
           (if (not (null? (cdddr exp)))
               (cadddr exp)
               'false))
         (define (if-syntax->if-alternative syntax) (if-alternative (syntax->expr syntax)))
         
         ;(define (make-if predicate consequent alternative)
         ;  (list 'if predicate consequent alternative))
         
         (define (application? exp) (pair? exp))
         ;(define (operator exp) (car exp))
         ;(define (operands exp) (cdr exp))
         
         ;(define (no-operands? ops) (null? ops))
         ;(define (first-operand ops) (car ops))
         ;(define (rest-operands ops) (cdr ops))
         
         ;(define (load? expr) (tagged-list? expr 'load))
         ;(define (load-file expr) (second expr))
         ;(define (file->list filehandle)
         ;  (let ((next (read filehandle)))
         ;    (if (eof-object? next) '() (cons next (file->list filehandle))))) 
         
         (define (top-level? expr) (tagged-list? expr 'top-level))
         (define top-level-sequence rest)
         
         (define (plist->list params)
           (cond ((symbol? params) (list params))
                 ((null? params) (list))
                 (else (pair (head params)
                             (plist->list (tail params)) ))))

         
         (define (sexpr->syntax sugared-sexpr env)  
           
           (with-exception-handler
            
            (lambda (exn) (begin (display "\nexception within sexpr->syntax on expression: ")
                                 (write sugared-sexpr)
                                 (newline)
                                 (raise-continuable exn) ))
            
            (lambda ()
              
              (let ((sexpr (de-sugar sugared-sexpr))
                    (recurse (lambda (subexpr) (sexpr->syntax subexpr env)) ))
                (cond

                  ;;begin allows internal defines with mutual recursion, so here extend env with all defined vars:
                  ((begin? sexpr)
                   (let* ((defined-variables (extract-defined-vars sexpr))
                          (env (extend-environment defined-variables defined-variables env)))
                     (make-syntax 'begin
                                  sugared-sexpr
                                  (pair 'begin (map-in-order (lambda (subexpr) (sexpr->syntax subexpr env)) (rest sexpr)))
                                  defined-variables)))
                  
                  ((self-evaluating? sexpr) (make-syntax 'self-evaluating sugared-sexpr sexpr) )
                  ((variable? sexpr) (let ((lexical-address (tail (lookup-variable-value-and-id sexpr env))))
                                       (make-syntax 'variable sugared-sexpr sexpr lexical-address) )) 
                  ((quoted? sexpr) (make-syntax 'quoted sugared-sexpr sexpr))
                  ((lambda? sexpr) (let* ((formal-parameters (lambda-parameters sexpr))
                                          (body (sexpr->syntax (lambda-body sexpr) 
                                                               (extend-environment (plist->list formal-parameters) 
                                                                                   (plist->list formal-parameters)
                                                                                   env ))))
                                     (make-syntax 'lambda sugared-sexpr `(lambda ,formal-parameters ,body)) ))                                   
                  ((get-env? sexpr) (make-syntax 'get-env sugared-sexpr sexpr))
                  ((definition? sexpr) (let* ((dvar (definition-variable sexpr)))
                                         (define-variable! dvar dvar env)
                                         (let ((dvalue (recurse (definition-value sexpr))))
                                           (make-syntax 'definition sugared-sexpr `(define ,dvar ,dvalue)) )))
                  ((if? sexpr) (make-syntax 'if sugared-sexpr  (cons 'if (map recurse (tail sexpr)))))
                  ((application? sexpr) (make-syntax 'application sugared-sexpr (map recurse sexpr)))
                  (else (error "Unknown expression type:" sexpr)) )))))

         (define (extract-defined-vars seq) (filter-map (lambda (expr) (if (definition? expr) (definition-variable expr) false)) seq))

         (define (expand-loads expr)
           (let ((expanded (apply append (map (lambda (subexpr) (if (load? subexpr) (file->list (open-included-file (second subexpr))) (list subexpr))) expr))))
             (if (equal? expanded expr)
                 expr
                 (expand-loads expanded))))
         (define (load? expr) (tagged-list? expr 'load))
         (define (file->list filehandle)
           (let ((next (read filehandle)))
             (if (eof-object? next) '() (cons next (file->list filehandle))))) 


         (define (write-with-syntax-ids syntax . depth)
           (if (syntax? syntax)
             (let ((depth (if (null? depth) 0 (first depth))))
               (display (syntax->id syntax))
               (display ": ")
               (display (syntax->original-expr syntax))
               (display "\n")
               (if (list? (syntax->expr syntax))
                   (map (lambda (s) (write-with-syntax-ids s (+ 1 depth))) (rest (syntax->expr syntax)))
                   '()))
             '()))
               
           
         
         
)