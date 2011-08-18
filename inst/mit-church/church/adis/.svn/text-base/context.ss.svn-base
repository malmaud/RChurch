#!r6rs

(library

 (church adis context)

 (export *context-equal*
         *context-hash*
         *context-graph*
         make-context-table
         *no-neighbors*
         get-context-neighbors
         get-context-neighbor-values
         set-context-neighbors!
         add-context-pair!
         get-predecessors
         get-descendants
         get-descendants-alist
         descendant-edge->context
         descendant-edge->prior
         descendant-edge->choice)

 (import (church utils rnrs)
         (church utils utils)
         (church readable-scheme)
         (_srfi :1)
         (_srfi :69))

 ;; predecessors is a list of contexts
 ;; descendants-alist is an association list
 ;; (outcome . (score . context)) ...
 (define (make-neighbors predecessors descendants-alist)
   (list 'neighbors predecessors descendants-alist))

 (define neighbors->predecessors second)
 
 (define neighbors->descendants-alist third)

 (define *context-equal* equal?)

 (define (*context-hash* obj bound)
   (inexact->exact (hash obj bound)))
   ;; need inexact->exact since srfi hashtable implementation
   ;; complains about non-int vector indizes otherwise
 
 (define (make-context-table)
    (make-hash-table *context-equal* *context-hash*))
  
 (define *context-graph*
   (make-parameter (make-context-table)))
 
 (define *no-neighbors*
   (make-neighbors '() '()))
 
 (define (get-context-neighbors context)
   (hash-table-ref/default (*context-graph*)
                           context
                           *no-neighbors*))
 
 (define (get-context-neighbor-values context)
   (let ([neighbors (get-context-neighbors context)])
     (values (neighbors->predecessors neighbors)
             (neighbors->descendants-alist neighbors))))

 (define (set-context-neighbors! context predecessors descendants)
   (hash-table-set! (*context-graph*)
                    context
                    (make-neighbors predecessors descendants)))

 (define (add-context-pair! prev-context cur-context prev-outcome prior)
   (let*-values ([(prev-predecessors prev-descendants) (get-context-neighbor-values prev-context)]
                 [(cur-predecessors cur-descendants) (get-context-neighbor-values cur-context)]
                 [(new-cur-predecessors) (lset-adjoin *context-equal* cur-predecessors prev-context)]
                 [(new-descendant) (pair prev-outcome (pair prior cur-context))]
                 [(new-prev-descendants) (lset-adjoin *context-equal* prev-descendants new-descendant)])
     (let ([stored-descendant (assoc prev-outcome prev-descendants)])
       (if (not (or (false? stored-descendant)
                    (equal? stored-descendant new-descendant)))
           (error (list stored-descendant new-descendant) "tried to overwrite existing choice+descendant with different descendant value!")
           (begin
             ;; add prev-context as a predecessor to cur-context
             (set-context-neighbors! cur-context new-cur-predecessors cur-descendants)
             ;; add (prev-outcome . (prior . cur-context)) as a descendant to prev-context
             (set-context-neighbors! prev-context prev-predecessors new-prev-descendants))))))

 (define (get-predecessors context)
   (neighbors->predecessors (get-context-neighbors context)))

 (define (get-descendants context)
   (map cddr (get-descendants-alist context)))

 ;; get-descendants-alist returns an association list [(val . context) ...]
 ;; which stores for each descendant which value leads to it
 (define (get-descendants-alist context)
   (neighbors->descendants-alist (get-context-neighbors context)))

 (define descendant-edge->choice car)
 (define descendant-edge->prior cadr)
 (define descendant-edge->context cddr)

 )