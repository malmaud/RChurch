#!r6rs

(import (church readable-scheme)
        (church utils rnrs)
        (church adis context)
        (church adis bounds))

(define (get-context-graph context-edge)
  (pair
   (list
    (descendant-edge->choice context-edge)
    (descendant-edge->prior context-edge)
    (get-context-bound (descendant-edge->context context-edge)))
   (map get-context-graph (get-descendants-alist context-edge))))

(define (display-context-graph root-context)
  (let ([root-descendants-alist (get-descendants-alist root-context)])
    (pretty-print
     (pair
      (list 'root (get-context-bound root-context))
      (map get-context-graph root-descendants-alist)))))

(define test-connections
  (list
   (list 'a 'b 1 .4)
   (list 'a 'c 2 .6)))

(define (run-tests)
  ;; add context connections
  (for-each (lambda (connection) (apply add-context-pair! connection))
            test-connections)
  (display-context-graph 'a)

  ;; set first leaf bound, propagate
  (set-context-bound! 'b (make-bound 1. 1.))
  (propagate-bounds! 'b)
  (display-context-graph 'a)

  ;; set second leaf bound, propagate
  (set-context-bound! 'c (make-bound 0. 0.))
  (propagate-bounds! 'c)
  (display-context-graph 'a))

(run-tests)