#!r6rs

(library

 (church church-eval logmath)
 
 (export LOG-PROB-0
         LOG-PROB-1
         log-sum-exp
         log-normalize
         log-mean
         log-variance
         log-1-minus)
 
 (import (church utils rnrs)
         (church readable-scheme))
 
 (define LOG-PROB-0 -inf.0)
 (define LOG-PROB-1 0.0)

 (define (log-sum-exp . log-vals)
   (let* ([max-log-val (apply max log-vals)])
     (if (equal? max-log-val -inf.0)
         -inf.0
         (+ (log (exact->inexact (sum (map (lambda (val) (exp (- val max-log-val))) log-vals))))
            max-log-val))))

 (define (log-normalize log-scores)
   (let* ([score-sum (apply log-sum-exp log-scores)])
     (map (lambda (score) (- score score-sum)) log-scores)))

 (define (log-mean log-scores)
   (assert (> (length log-scores) 0))
   (- (apply log-sum-exp log-scores) (log (length log-scores))))

 (define (log-variance log-scores)
   (assert (> (length log-scores) 0))
   ;; (display "\ncomputing variance of ")
   ;; (display log-scores)
   ;; (display "\n")
   (let ([m (exp (log-mean log-scores))])
     (log (/ (sum (map (lambda (s) (expt (- (exp s) m) 2))
                       log-scores))
             (length log-scores)))))

 (define (log-1-minus a)
   (log (- 1 (exp a))))

 )