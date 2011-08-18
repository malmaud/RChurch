#!r6rs

;; created Jun 23, 2008 by NDG
;; authors: noah goodman, daniel roy

(library (church register-primitives)

         (export register-primitive-procedure!
                 register-primitive-erp!
                 register-primitive-constant!
                 register-query!

                 all-primitive-names
                 all-primitive-objects
                 )

         (import (church utils rnrs)
                 (_srfi :1)                 
                 (church readable-scheme)
                 (church church-eval desugar)
                 )

         
         (define primitive-procedures '())
         (define (register-primitive-procedure! name proc . inverse)
           (if (null? inverse)
               (set! primitive-procedures (cons (list name proc) primitive-procedures ))
               (set! primitive-procedures (cons (list name proc (first inverse)) primitive-procedures ))))
         
         (define primitive-constants '())
         (define (register-primitive-constant! name val)
           (set! primitive-constants (cons (list name val) primitive-constants )))
         
         (define primitive-erps '())
         (define (register-primitive-erp! name erp)
           (set! primitive-erps (cons (list name erp) primitive-erps )))

         ;;expects query-proc to be a scheme procedure that expects call form (query-proc arg1 arg2 ... normal-form-query-expression env)
         ;;transforms into a def-query that expects church code of form: (query-name arg1 arg2 ... (define ...) ... query-expr condition-expr) 
         (define (register-query! query-name query-proc)
           (define raw-form-name (gensym))
           (define (query? expr) (tagged-list? expr query-name))
           (define (desugar-query expr)
             (let*-values ([ (control-part defs) (break (lambda (subexpr) (tagged-list? subexpr 'define)) (drop-right expr 2))]
                           [ (control-args) (rest control-part)]
                           [ (query-exp cond-exp) (apply values (take-right expr 2))])
               `(,raw-form-name ,@control-args '(begin ,@defs (pair ,query-exp ,cond-exp)) (get-current-environment))))
           (register-primitive-procedure! raw-form-name query-proc)
           (register-sugar! query? desugar-query))
         
         (define (primitive-names primitive-list)
           (map first primitive-list))

         (define (all-primitive-names)
           (append (primitive-names primitive-procedures) (primitive-names primitive-constants) (primitive-names primitive-erps) ))

         (define (all-primitive-objects)
           (append (primitive-procedure-objects) (map second primitive-constants) (map second primitive-erps)))
         
         (define (primitive-procedure-objects)
           (map (lambda (proc) (pair 'primitive (rest proc)))
                primitive-procedures))


         )