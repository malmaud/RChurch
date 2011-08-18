#!r6rs

;;written by NDG on July 3, 2008
;;contains partial inverses for some primitive functions, to be used in standard-env.ss.

(library (church constraint-propagation primitive-inverses)

         (export  merge

                  has-inverse
                  has-seq-inverse
                  get-seq-inverse

                  and-inverse
                  or-inverse
                  not-inverse
                  xor-inverse
                  pair-inverse
                  list-inverse
                  first-inverse
                  rest-inverse
                  equal?-inverse
                  list-elt-inverse
                  list-ref-inverse
                  plus-inverse
                  identity-inverse
                  null?-inverse
                  append-inverse)

         (import (church utils rnrs)
                 (_srfi :1)
                 ;;(church constraint-propagation trace-constraint-propagation)
                 ;;(church church-eval trace-eval)
                 (church readable-scheme)
                 (church constraint-propagation constraints))

         
         ;; attempt to merge values (which may include wildcards)
         ;; recursively -- seeking a common refinement of the two value templates.

         (define (merge val1 val2)
           (cond
            ((eq? val1 *wildcard-value*) val2)
            ((eq? val2 *wildcard-value*) val1)
            ((equal? val1 val2) val1)
            ((and (pair? val1) (pair? val2))
             (let ((first-merged (merge (head val1) (head val2)))
                   (tail-merged (merge (tail val1) (tail val2))))
               (if (or (eq? first-merged 'fail) (eq? tail-merged 'fail)) 
                   'fail 
                   (pair first-merged tail-merged) )))
            (else 'fail) ))

         
          ;; Generic functions for dealing with inverses

         (define (has-inverse op)
           (> (length op) 2))

         (define (has-seq-inverse inv)
           (> (length inv) 2))

         (define proc->inverse third)
         
         (define inv->seq-inverse third)

         (define (default-inverse constraint args-so-far num-args)
           (list *wildcard-value*))

         (define (get-seq-inverse proc)
           (if (and (has-inverse proc)
                    (has-seq-inverse (proc->inverse proc)))
               (inv->seq-inverse (proc->inverse proc))
               default-inverse))

         
         ;; each inverse function f-inv:: val [init-args] --> list-of-args-lists
         ;;   takes a return value as input and gives a list of lists of possible
         ;;   argument values as output.
         ;;   an optional "current arguments" input can be thought of as an
         ;;   initializer for inversion...
         ;; inverses don't have to return all possible inverses, and can return the
         ;; empty list when they know no inverses.
         ;; return value lists can include *wildcard-value* entries, which means any
         ;; value for this entry will give the desired ouput.
         
         (define (single-permutations len default-entry special-entry)
           (map
            (lambda (x)
              (append
               (make-list x default-entry)
               (list special-entry)
               (make-list (- len x 1) default-entry)))
            (iota len)))

         (define and-inverse
           (list 'deterministic-inverse
                 (lambda (val . init-args)
                   (let ((num-operands (length init-args)))
                     (if (eq? val true)
                         (list (make-list num-operands true) ) ;(push-val-down-branch 3 val (push-val-down-branch 4 val trace))
                         (if (eq? val false) (single-permutations num-operands *wildcard-value* false)
                             '() ))))))

         (define or-inverse
           (list 'deterministic-inverse
                 (lambda (val . init-args)
                   (let ((num-operands (length init-args)))
                     (if (eq? val true) (single-permutations num-operands *wildcard-value* true)
                         (if (eq? val false) (list (make-list num-operands false))
                             '() ))))
                 (lambda (constraint args-so-far num-args)
                   (cond ((eq? constraint true)
                          (cond ((any (lambda (x) x) args-so-far) (list true false))
                                ((equal? (+ (length args-so-far) 1) num-args) (list true))
                                (else (list true false))))
                         ;((eq? constraint false)
                         ; (if (any (lambda (x) x) args-so-far)
                         ;     '()
                         ;     (list false)))
                         ((eq? constraint false)
                          (list false))
                         ((eq? constraint *wildcard-value*)
                          (list *wildcard-value*))
                         (else '())))))
                  
         (define not-inverse
           (list 'deterministic-inverse
                 (lambda (val . init-args) (list (list (not val) ) ))))

         (define identity-inverse
           (list 'deterministic-inverse
                 (lambda (val . init-args) (list (list val) ))))

         (define xor-inverse
           (list 'deterministic-inverse
                 (lambda (val . init-args)
                   (if (eq? val true)
                       (list (list true false) (list false true) )
                       (list (list true true) (list false false) )
                       ))))

         (define pair-inverse
           (list 'deterministic-inverse
                 (lambda (val . init-args)
                   (if (pair? val)
                       (list (list (head val) (tail val)))  ;(push-val-down-branch 4 (rest val) (push-val-down-branch 3 (first val) trace))
                       '() ))))

         (define list-inverse
           (list 'deterministic-inverse
                 (lambda (val . init-args)
                   (if (proper-list? val)
                       (list val)
                       (if (eq? (mylast val) *wildcard-value*)
                           (list (listify val))
                           '() )))))
         (define (mylast lst) (if (pair? lst) (mylast (rest lst)) lst))
         (define (listify lst)
           (if (pair? lst)
               (cons (first lst) (listify (rest lst)))
               '()))

         (define null?-inverse
           (list 'deterministic-inverse
                 (lambda (val . init-args)
                   (if (eq? val true)
                       (list (list '() ))
                       '() ))))

         (define first-inverse
           (list 'deterministic-inverse
                 (lambda (val . init-args)
                   (list (list (pair val *wildcard-value*) ) ) )))

         (define rest-inverse
           (list 'deterministic-inverse
                 (lambda (val . init-args)
                   (list (list (pair *wildcard-value* val) ) ) )))

         ;; this inverse for equal? just returns the two inverses for true that
         ;; result from trying to make both arguments be equal to one of the existing arguments.
         (define equal?-inverse
           (list 'deterministic-inverse
                 (lambda (val . init-args)
                   (cond ((eq? val true)
                          (list (list (first init-args) (first init-args) )
                                (list (second init-args) (second init-args) ) ))
                         ((eq? val *wildcard-value*)
                          (list (list *wildcard-value* *wildcard-value*)))
                         (else '() )))
                 (lambda (constraint args-so-far num-args)
                   (if (eq? #t constraint)
                       (if (null? args-so-far)
                           (list *wildcard-value*)
                           (list (first args-so-far)))
                       (list *wildcard-value*)))))
         
         ;; this gives two inverses if possible: one that preserves index (doesn't try to
         ;; look through current list for an index that would satisfy val),
         ;; and one that preserves list, changing index.
         (define list-elt-inverse
           (list 'deterministic-inverse
                 (lambda (val . init-args)
                   (let* ((cur-index (second init-args))
                          (cur-list (first init-args))
                          (inv-index (list-index (lambda (x) (not (eq? 'fail (merge val x)))) cur-list)))
                     (if (not (false? inv-index))
                         (list (list cur-list (+ 1 inv-index))
                               (list (append (make-list (- cur-index 1) *wildcard-value*) (pair val *wildcard-value*)) cur-index )
                               )
                         (list (list (append (make-list (- cur-index 1) *wildcard-value*) (pair val *wildcard-value*)) cur-index ) )) ))))

         (define list-ref-inverse
           (list 'deterministic-inverse
                 (lambda (val . init-args)
                   (let* ((cur-index (second init-args))
                          (cur-list (first init-args))
                          (inv-index (list-index (lambda (x) (not (eq? 'fail (merge val x)))) cur-list)))
                     (if (not (false? inv-index))
                         (list (list cur-list inv-index)
                               (list (append (make-list cur-index *wildcard-value*) (pair val *wildcard-value*)) cur-index )
                               )
                         (list (list (append (make-list cur-index *wildcard-value*) (pair val *wildcard-value*)) cur-index ) )) ))))

         ;;this inverse for + just returns the two inverses that preserve one of the existing arguments
         (define plus-inverse
           (list 'deterministic-inverse
                 (lambda (val . init-args)
                   (let ((a (first init-args))
                         (b (second init-args)))
                     (list (list a (- val a))
                           (list (- val b) b))))))

         ;;this inverse for append returns all inverses with correct number of parts for number of arguments.
         (define append-inverse
           (list 'deterministic-inverse
                 (lambda (val . init-args)
                   (let* ((n-parts (length init-args))
                          (n-elts (length val))
                          (parts  (contiguous-partitions n-elts n-parts)))
                     (map (lambda (part) (break-up val part)) parts)))))

         (define (break-up lst segment-lngths)
           (if (null? segment-lngths)
               '()
               (pair (take lst (first segment-lngths))
                     (break-up (drop lst (first segment-lngths) ) (rest segment-lngths)))))

         ;;enumerate all the contiguous partitions of n-elts elements into n-part partitions. return a list of lists of segment sizes.
         ;; eg. (contiguous-partitions 3 2) returns ((1 2) (2 1)).
         (define (contiguous-partitions n-elts n-parts)
           (if (= 1 n-parts)
               (list (list n-elts))
               (apply append
                      (map (lambda (first-seg-lngth)
                             (map (lambda (rest-part)
                                    (pair first-seg-lngth rest-part))
                                  (contiguous-partitions (- n-elts first-seg-lngth) (- n-parts 1)))) ;;all the partitions of the remainder
                           (iota (- n-elts (- n-parts 1)) 1))))) ;;all the possible first segment lengths.

         (when (> (*verbosity*) 12)
               (display "loaded primitive-inverses.ss\n"))
         )
