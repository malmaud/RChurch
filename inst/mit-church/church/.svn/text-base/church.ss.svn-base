;; created Jun 22, 2008 by noah goodman
;; authors: noah goodman, daniel roy, elliot uvero
#!r6rs

(library (church church)
         
         (export church
                 church-with-env
                 lazy-church
                 lazy-church-top-level-eval
                 setup-environment
                 church-top-level-eval
                 scored-value->value 
                 ;scored-value->score
                 register-primitive-procedure!
                 register-primitive-erp!
                 register-query!
                 marg)
         
         (import (church standard-env)
                 (church utils rnrs)
                 (church utils utils)
                 (church church-eval syntax)
                 (church church-eval environments-lexical)
                 (church church-eval church-eval)
                 (church church-eval trace-eval)
                 (church readable-scheme)
                 (church mcmc mcmc-core)
                 (visualization trace-to-dot)
                 )
                  
         (define-syntax church
           (syntax-rules ()
             ((church exprs ...) (church-top-level-eval '(exprs ...)))))

         (define-syntax lazy-church
           (syntax-rules ()
             ((lazy-church exprs ...) (lazy-church-top-level-eval '(exprs ...)))))
         
         (define-syntax church-with-env
           (syntax-rules ()
             ((church-with-env exprs ... env) 
              (scored-value->value
               (church-top-level-eval (append '((debug-mode 'verbosity 11)) 
                                              '(exprs ...) 
                                              '((debug-mode 'verbosity 3)(get-current-environment))) 
                                      env )))))

          (define (lazy-church-top-level-eval top-seq . optional-env)
            (parameterize ([*lazy* true])
                          (apply church-top-level-eval (pair top-seq optional-env))))

         ;;evaluate a sequence of defines, followed by an expression of interest. called with a list of expressions.
         (define (church-top-level-eval top-seq . optional-env)
           (let* ((initial-environment (if (null? optional-env)
                                           (setup-environment)
                                           (first optional-env) ))       
                  (top-seq (if (null? optional-env)
                               (pair '(load "standard-preamble.church")
                                     top-seq)
                               top-seq ))
                  (top-syntax (sexpr->syntax `(begin ,@top-seq) initial-environment)))

            ;; (display top-syntax) (newline)
             
             (reset-proposal-statistics!)
             
             (let*-values ( ( (trace-container forw-score rev-score) (church-eval top-syntax initial-environment))
                            ( (value) (trace-container->value trace-container)))
                               (when (> (*verbosity*) 10) (for-each display (list value "\n")) )
                               (when (or (*global-debug*) (*mh-statistics*) (*mh-steps*)) (display-proposal-statistics))
                               (when (*pretty-pictures*)
                                 (trace->dot trace-container "pretty-trace")
                                 (display "Made dot file, use something like '/usr/local/bin/dot -Tpdf pretty-trace.dot -o pretty-trace.pdf' to render.\n"))
                               value)))

          (define (scored-value->value sv) sv)

  
         (when (> (*verbosity*) 9)
           (display "Loaded Church v0.1.2+ (trunk)\n\n"))
          
)
