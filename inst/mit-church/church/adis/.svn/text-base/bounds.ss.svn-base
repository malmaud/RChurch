#!r6rs

;; adaptation/bound propagation
;;
;; TODO: Add threshold for change to limit queue size.
;;       More generally, do pruning if queue gets too big.

(library

 (church adis bounds)

 (export make-bound
         bound->lower
         bound->upper
         *bound-table*
         *default-bound*
         get-context-bound
         get-context-bound-values
         set-context-bound!
         propagate-bounds!)

 (import (church readable-scheme)
         (church utils rnrs)
         (church utils utils)
         (church adis context)
         (church adis queue)
         (_srfi :1)
         (_srfi :69))

 (define (make-bound lower upper)
   (list 'bound lower upper))

 (define bound->lower second)

 (define bound->upper third)

 (define *bound-table*
   (make-parameter (make-context-table)))

 (define *default-bound*
   (make-bound 0.0 1.0))

 (define (get-context-bound context)
   (hash-table-ref/default (*bound-table*)
                           context
                           *default-bound*))

 (define (get-context-bound-values context)
   (let ([bound (get-context-bound context)])
     (values
      (bound->lower bound)
      (bound->upper bound))))

 (define (set-context-bound! context bound)
   (hash-table-set! (*bound-table*)
                    context
                    bound))
  
 (define (weighted-mean values weights)
   (sum (map (lambda (v p) (* v p)) values (normalize weights))))

 (define (bound-convex-combination bounds probs)
   (let* ([lowers (map bound->lower bounds)]
          [uppers (map bound->upper bounds)])
     (make-bound (weighted-mean lowers probs) (weighted-mean uppers probs))))

 (define (get-descendant-priors-and-bounds context)
   (let* ([descendants-alist (get-descendants-alist context)]
          [descendants-priors (map cadr descendants-alist)]
          [descendants-bounds (map (lambda (a) (get-context-bound (descendant-edge->context a))) descendants-alist)]
          [prior-mass (sum descendants-priors)])
     (when (> prior-mass 1.0)
           (error descendants-priors "sum of priors > 1.0!"))
     (if (< prior-mass 1.0)
         (values
          (append descendants-priors (- 1.0 prior-mass))
          (append descendants-bounds *default-bound*))
         (values
          descendants-priors
          descendants-bounds))))

 (define (propagate-bounds! initial-context)
   (let ([queue (make-queue (get-predecessors initial-context))])
     (let loop ()
       (when (not (queue-empty? queue))
             (let*-values ([(context) (dequeue! queue)]
                           [(priors bounds) (get-descendant-priors-and-bounds context)]
                           [(mean-bound) (bound-convex-combination bounds priors)])
               (when (not (equal? mean-bound (get-context-bound context)))
                     (set-context-bound! context mean-bound)
                     (enqueue-all! queue (get-predecessors context)))
               (loop))))))

 )