#!r6rs

(import (church readable-scheme)
        (church utils rnrs)
        (church adis context))

(define (display-nl x)
  (begin
    (display x)
    (display "\n")))

(define test-connections
  (list
   (list 'a 'b 1 .4)
   (list 'a 'c 2 .6)
   (list 'b 'd 1 .2)
   (list 'b 'e 2 .3)
   (list 'b 'f 3 .5)
   (list 'c 'g 1 1.)
   (list 'd 'g 1 1.)
   (list 'e 'g 1 1.)
   (list 'f 'g 1 1.)))

(define (run-tests)
  (for-each (lambda (connection) (apply add-context-pair! connection))
            test-connections)

  ;; four predecessors 'c 'd 'e 'f
  (display-nl (get-predecessors 'g))

  ;; (1 . (.4 b)), (2. (.6 c))
  (display-nl (get-descendants-alist 'a))

  ;; 'a
  (display-nl (get-predecessors 'b))

  ;; (1 . (.2 d)) (2 . (.3 e)) (3 . (.5 f))
  (display-nl (get-descendants-alist 'b)))

(run-tests)