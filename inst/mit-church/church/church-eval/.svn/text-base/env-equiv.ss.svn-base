#!r6rs

(library (church church-eval env-equiv)
         
         (export syntax->env-equiv)
         
         (import (church utils rnrs)
                 (_srfi :69)
                 (_srfi :1)
                 (church readable-scheme)
                 (church church-eval syntax)
                 (church church-eval environments-lexical))

         (define *all-addresses* (gensym 'alladdr))
         
         
         ;;try to get the env-equiv function for a syntax element. if it's not in the table, build it.
         (define (syntax->env-equiv syntax table)
           (let ((env-equiv (hash-table-ref table (syntax->id syntax) (lambda () false))))
             (if (not env-equiv)
                 (begin (build-env-equivs syntax table)
                        (hash-table-ref table (syntax->id syntax)))
                 env-equiv)))
         
         
         ;;builds an env-equiv function for this syntax and stores it in the hash-table (indexed by syntax id).
         ;;since this is recursive it will also build and store env-equiv functions for the sub-syntax.
         ;;assumes that lexical addresses have already been built (by sexpr->syntax) relative to appropriate envs.
         ;;this returns a list of lexical addresses required by the syntax, but usually we actually want the updated hash table.
         ;;NOTE: this doesn't handle top-level syntax or defines, since we never need env-equiv there....
         (define (build-env-equivs syntax table)
           
           ;get lists of lexical addresses from sub expressions
           (let* ((recurse (lambda (s) (build-env-equivs s table)))
                  (lex-addr-lists (cond
                                    ((syntax:begin? syntax) (map (lambda (s) (filter-map address-in-enclosing-environment (recurse s)))
                                                                 (rest (syntax->expr syntax))))
                                    ((syntax:self-evaluating? syntax) '() )
                                    ((syntax:variable? syntax) (list (syntax->details syntax)))
                                    ((syntax:quoted? syntax) '() )
                                    ((syntax:lambda? syntax) (list (filter-map address-in-enclosing-environment 
                                                                               (recurse (lambda-syntax->lambda-body syntax)))))
                                    ((syntax:get-env? syntax) (list (list *all-addresses*)))
                                    ((syntax:definition? syntax) (list (recurse (definition-syntax->definition-value syntax))))
                                    ((syntax:if? syntax) (map recurse (tail (syntax->expr syntax))))
                                    ((syntax:application? syntax) (map recurse (syntax->expr syntax)))
                                    (else (error "get-env" "Can't handle syntax type:" (syntax->type syntax))) ))
                  (lex-addr (apply append lex-addr-lists))) ;;should merge-sorted-lists for speed?

               ;store the env-equiv in table and return the lexical addresses this depends on:    
               (hash-table-set! table (syntax->id syntax) (build-env-equiv lex-addr))
               
               lex-addr))
         
         
         ;;build an env-equiv function relative to a list of lexical addresses (possibly including *all-addresses*):
         (define (build-env-equiv lex-addrs) 
           (let ((relevant-bindings (build-relevant-bindings lex-addrs 0)))
             (if (eq? relevant-bindings *all-addresses*)
                 (lambda (new-env old-env) (equal? new-env old-env))
                 (lambda (new-env old-env) (relative-env-equiv new-env old-env relevant-bindings)))))
         

         (define (build-relevant-bindings lex-addrs base)
           (if (list-index (lambda (a) (eq? *all-addresses* a)) lex-addrs)
               *all-addresses*
               (if (null? lex-addrs)
                   '()
                   (let-values (((this-frame other-frames) (partition (lambda (addr) (= (head addr) base)) lex-addrs)))
                     (pair (delete-duplicates (map tail this-frame)) ;;FIXME: delete-dupicates is O(n^2), should be able to do better since order doesn't matter.
                           (build-relevant-bindings other-frames (+ base 1)))))))

)