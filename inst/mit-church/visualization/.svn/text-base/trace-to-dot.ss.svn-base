#!r6rs

;;make a dot file from a trace, for visualization.

(library (visualization trace-to-dot)

         (export trace->dot)

         (import (church utils rnrs)
                 (_srfi :69) ; hash tables
                 (church readable-scheme)
                 (church church-eval traces)
                 (church church-eval syntax)
                 (church church-eval church-eval)
                 (church utils utils))

         (define node-names '())
         (define max-id '())

         (define (trace->dot trace-container filename-base)
           (let ((trace (trace-container->trace trace-container)))
             (display "\nMaking dot file... ")

             (set! node-names (make-hash-table))
             (set! max-id 0)

             (with-output-to-file (string-append filename-base ".dot")
               (lambda ()
                 (for-each display
                           (append '("digraph G {\n")
                                   (link-statements trace)
                                   (node-statements trace)
                                   '("}\n")
                                   ))))
             (for-each display (list "output written to " filename-base ".dot \n\n"))))

         (define (trace-id trace)
           (hash-table-ref node-names (get-name trace) (lambda () (let ((new-name (begin (set! max-id (+ max-id 1)) max-id)))
                                                                    (hash-table-set! node-names (get-name trace) new-name)
                                                                    new-name))))

         (define (link-statements trace)
           (let ((subtraces (extended-get-subtraces trace)))
             (if (null? subtraces) '()
                 (apply append
                        (cons
                         (append (list (trace-id trace)  " -> {")
                                 (apply append (map (lambda (x) (list " " x " ") ) (map trace-id subtraces) ) )
                                 (list "}\n"))
                         (map link-statements subtraces))))))

         (define (node-statements trace)
           (apply append
                  (cons
                   (append (list (trace-id trace) " [")
                           (if (is-apply-node? trace)
                               (list "shape=box, "
                                     (cond
                                      ((elementary-random-procedure? (get-operator trace)) "color=red, label=\" ")
                                      ((memoized-procedure? (get-operator trace)) "color=green, label=\" ")
                                      ((mem? (get-operator trace)) "color=blue, label=\" ")
                                      (else "label=\" "))
                                     )
                               (list
                                "label=\"" (cropped-write-to-string (get-original-expr trace) 50)))
                           (list "\\n value: " (prettify-value (get-val trace)))
                           (list "\\n score: " (get-score trace))
                           (if (and (is-apply-node? trace) (compound-procedure? (get-operator trace)))
                               (list "\\n bindings:" (procedure-parameters (get-operator trace)))
                               '())
                           '("\" ];\n")
                           )
                   (map node-statements (extended-get-subtraces trace)))))

         (define (procedure-parameters p) (cadr p))
         (define (compound-procedure? p) (tagged-list? p 'procedure))
         (define (elementary-random-procedure? p) (tagged-list? p 'erp))
         (define (memoized-procedure? p) (tagged-list? p 'memoized-procedure))
         (define (primitive-procedure? proc) (tagged-list? proc 'primitive))
         (define (mem? p) (eq? p 'mem))

         (define (prettify-value val)
           (cond
            ((compound-procedure? val) "procedure")
            ((elementary-random-procedure? val) "erp")
            ((primitive-procedure? val) "primitive")
            ((memoized-procedure? val) "memoized procedure")
            (else (cropped-write-to-string val 20))))

         (define (extended-get-subtraces trace)
           (if (and (is-apply-node? trace) (mem? (get-operator trace)))
               (map proposal->trace (hash-table-values (third (get-val trace)))) ;;get subtraces from memoizer hash-table...
               (get-subtraces trace)))

         )

                                        ;         (define (trace->pdf trace . filename-base-base)
                                        ;           (define filename-base (if (null? filename-base-base)
                                        ;                                     (symbol->string (gensym))
                                        ;                                     (first filename-base-base)))
                                        ;           (trace->dot trace filename-base)
                                        ;           (display "\nRunning dot... ")
                                        ;           (system (string-append "/usr/local/bin/dot -Tpdf " filename-base ".tmp -o " filename-base ".pdf")) ;;note: adjust path to your local copy of dot
                                        ;           (display "\nRemoving dot file... ")
                                        ;           (system (string-append "rm " filename-base ".tmp"))
                                        ;           (for-each display (list "output written to " filename-base ".pdf \n\n")))
