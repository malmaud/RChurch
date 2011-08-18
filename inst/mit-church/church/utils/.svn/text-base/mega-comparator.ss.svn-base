#!r6rs

(library (church utils mega-comparator)

         (export mega-comparator)

         (import (church utils rnrs)
                 (church readable-scheme)
                 (only (_srfi :43) vector-length)
                 )

         ;;
         ;; Our name comparator knows how to handle four kinds of
         ;; things: strings, numbers, vectors and lists.  Anything
         ;; else needs to be converted.  To simplify things a bit, we
         ;; include type information.  This allows us to compare items
         ;; of different types efficiently.
         ;; 
         ;; The types are:
         ;;   0 - number
         ;;   1 - string
         ;;   2 - list
         ;;   3 - vector
         ;;
         
         (define (process-arg a)
           (cond
            ; ints, flonums, exacts, inexacts
            ((number? a) (cons 0 a))
            ; these things are strings
            ((string? a) (cons 1 a))
            ((symbol? a) (cons 1 (symbol->string a))) ;; gensyms are symbols, too
            ; other native types
            ((list? a) (cons 2 a))
            ((vector? a) (cons 3 a))
            ; types we can't handle yet
            ((hashtable? a) (error 'process-arg "cant handle hashtables yet" a))
            ; else, try to make it into a string.
            ; XXX hopefully this works for complicated types; seems to work for records
            (else (cons 1 (write-to-string a)))))

         ;;
         ;; Compare two objects.  This is a tri-valued comparator,
         ;; which returns #t if (< a b), #f if (> a b), and 3 if (= a
         ;; b).  Suitable for recursive use.
         ;;
         (define (recursive-comparator a b)

           (let* (
                  (amod (process-arg a))
                  (bmod (process-arg b))
                  (a-type (car amod))
                  (b-type (car bmod))
                  (a-val (cdr amod))
                  (b-val (cdr bmod))
                  )

             (if (not (equal? a-type b-type))
                 ; if they're different types of things, we order them
                 ; by directly comparing their type ids.  this gives number < string < list < vector
                 (< a-type b-type)

                 ; they're the same type.
                 (cond

                  ; numbers
                  ((= a-type 0)
                   (if (equal? a-val b-val)
                       3
                       (< a-val b-val)))
                  
                  ; strings
                  ((= a-type 1)
                   (if (string=? a-val b-val)
                       3
                       (string<? a-val b-val)))
                  
                  ; it's a list
                  ((= a-type 2)
                   (let ((a-len (length a-val))
                         (b-len (length b-val)))
                   (if (not (= a-len b-len)) ; start by checking lengths
                       (< a-len b-len)  
                       ; equal lengths?
                       (if (= a-len 0)
                           ; if they're both empty, they're equal
                           3
                           ; otherwise, compare the first element.
                           (let ((tmp (recursive-comparator (car a-val) (car b-val))))
                             (if (not (equal? tmp 3))
                                 ; the first value is < or >.  that's the result.
                                 tmp
                                 ; the first values are equal.  recurse to the next value.
                                 (recursive-comparator (cdr a-val) (cdr b-val))
                                 ))))))

                  ; it's a vector
                  ((= a-type 3)
                   (let ((a-len (vector-length a-val))
                         (b-len (vector-length b-val)))
                   (if (not (= a-len b-len)) ; start by checking lengths
                       (< a-len b-len)  
                       ; equal lengths?
                       (if (= a-len 0)
                           ; if they're both empty, they're equal
                           3
                           ; otherwise, iterate and compare elements
                           (let loop ((ind 0))

                             (let ((tmp (recursive-comparator (vector-ref a-val ind) (vector-ref b-val ind)) ))
                               (if (not (equal? tmp 3))
                                   tmp ; if this element is either < or >, that's the answer
                                   (if (< ind (- a-len 1))
                                       ; check the next element
                                       (loop (+ 1 ind))
                                       ; all of the elements are the same
                                       3)
                                   )))
                           )
                       )))
                           
                  (else (error 'recursive-comparator "unknown type!" a-type))

                  )
                 )
             ))

         ;;
         ;; Takes the real tri-valued comparator and returns
         ;; either #t or #f, per requirements of list-sort.
         ;;
         (define (mega-comparator a b)
           (let ((rval (recursive-comparator a b)))
             (if (equal? rval 3)
                 #f
                 rval)))

) ; end library
