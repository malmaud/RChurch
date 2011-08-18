#!r6rs

;;
;;  This is a multi-sample version of the dirichlet discrete.
;;
;;  Samples look like
;;

(library 
 (church xrp-lib ms-dirichlet-discrete)
 
 (export init-ms-dirichlet-discrete)
 
 (import (church utils rnrs)
         (church register-primitives)
         (_srfi :1)
         (church readable-scheme)
         (church external math-env)
         (church constraint-propagation constraints)
         (church church-eval trace-eval)
         (church church-eval traces)
         (church church-eval logmath)
         )

 (define (init-ms-dirichlet-discrete) 'foo)
 
 (define (zeros x)
   (repeat x (lambda() 0)))

 (define (one-hot val len)
   (append (zeros val) (list 1) (zeros (- len val 1))))
 
 ;;stats are (counts score)
 
 (define (ms-dirichlet-discrete-init-stats hyperparams)
   (list (make-list (length hyperparams) 0.0)
         LOG-PROB-1));(- (lngamma (apply + hyperparams)) (apply + (map lngamma hyperparams)))))
 
 ;sample conditioned on previous samples, return list of sampled value and new stats
 ;;FIXME: cache some of the partial sums (eg. sum of hyperparams).
 (define ms-dirichlet-discrete-sampler
   (lambda (stats hyperparams)
     (display "XXXXXXXXXX ms-dirichlet-discrete-sampler: ");(display stats)(newline)
     (list (zeros (length (first stats))) stats)))

 (define (madd a b) (apply + (map + a b)))
 
 ;unsample a particular sample, return new stats:
 (define ms-dirichlet-discrete-unsampler
   (lambda (value stats hyperparams . flag) 
;     (if (null? flag) (display "unsample: ") (display "unsample (increment): "))(display stats)(newline)(display value)(newline)
     (let* ((oldcounts (first stats))
            (old-score (second stats))
            (newcounts (if (null? flag)
                           (map - oldcounts value)
                           (map + oldcounts value)
                           ))
            (oldD (lngamma (+ 1 (madd oldcounts hyperparams))))  ; CACHEME
            (newD (lngamma (+ 1 (madd newcounts hyperparams))))
            (oldA (apply + (map (lambda(x) (lngamma (+ x 1))) (map + hyperparams oldcounts)) ))  ; CACHEME
            (newA (apply + (map (lambda(x) (lngamma (+ x 1))) (map + hyperparams newcounts)) ))
            (newscore (+ old-score oldD newA (- newD) (- oldA)))
            )
       (list newcounts newscore)
       )))
 
 ;score. updated in un/sampler, so here just look up:
 (define ms-dirichlet-discrete-scorer
   (lambda (stats hyperparams) 
     ;(display "score: ")(display stats)(newline)
     (second stats)))
 
 ;; XXX this is an identity proposer.  No changes ever made...
 (define ms-dirichlet-discrete-uniform-proposer
   (lambda (operator operands old-value-oh)
     (display "XXXXXXXXXX ms-dirichlet-discrete-uniform-proposer: ");(display old-value-oh)(newline)
     (values old-value-oh LOG-PROB-1 LOG-PROB-1)))

 (define (apply-with-church op args)
   (proposal->value (trace-apply op args '() *wildcard-constraint-set*)))

 (define (logsumexp x) 
   (log (apply + (map exp x))))


 (register-primitive-procedure! 'ms-dirichlet-discrete-init-stats ms-dirichlet-discrete-init-stats)
 (register-primitive-procedure! 'ms-dirichlet-discrete-sampler ms-dirichlet-discrete-sampler)
 (register-primitive-procedure! 'ms-dirichlet-discrete-unsampler ms-dirichlet-discrete-unsampler)
 (register-primitive-procedure! 'ms-dirichlet-discrete-scorer ms-dirichlet-discrete-scorer)
 (register-primitive-procedure! 'ms-dirichlet-discrete-uniform-proposer ms-dirichlet-discrete-uniform-proposer)
 
  
 )
