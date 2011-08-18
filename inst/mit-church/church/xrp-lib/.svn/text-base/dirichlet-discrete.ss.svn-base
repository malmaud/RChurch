#!r6rs

(library 
 (church xrp-lib dirichlet-discrete)
 
 (export init-dirichlet-discrete)
 
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
 
 (define (init-dirichlet-discrete) 'foo)
 
 ;;stats are (counts score)
 
 (define (dirichlet-discrete-init-stats hyperparams)
   (list (make-list (length hyperparams) 0.0)
         LOG-PROB-1));(- (lngamma (apply + hyperparams)) (apply + (map lngamma hyperparams)))))
 
 ;sample conditioned on previous samples, return list of sampled value and new stats
 ;;FIXME: cache some of the partial sums (eg. sum of hyperparams).
 (define dirichlet-discrete-sampler (lambda (stats hyperparams)
                                      ;(display "sample: ")(display stats)(newline)
                                      (let* ((rawcounts (first stats))
                                             (old-score (second stats))
                                             (counts (map + rawcounts hyperparams))
                                             (total-counts (apply + counts))
                                             (probs (map (lambda (c) (/ c total-counts)) counts))
                                             (value (sample-discrete probs))
                                             (new-rawcounts (append (take rawcounts value)
                                                                    (list (+ 1 (list-ref rawcounts value)))
                                                                    (drop rawcounts (+ 1 value))))
                                             (new-score (+ old-score
                                                           (log (+ (list-ref rawcounts value) (list-ref hyperparams value)))
                                                           (- (log (+ (apply + rawcounts) (apply + hyperparams))))))
                                             )
                                        (list value (list new-rawcounts new-score)))))
 
 ;unsample a particular sample, return new stats:
 (define dirichlet-discrete-unsampler (lambda (value stats hyperparams . flag) 
                                        ;(if (null? flag) (display "unsample: ") (display "unsample (increment): "))(display stats)(newline)
                                        (let ((rawcounts (first stats))
                                              (old-score (second stats)))
                                          (if (null? flag)
                                              (list 
                                               (append (take rawcounts value)
                                                       (list (- (list-ref rawcounts value) 1))
                                                       (drop rawcounts (+ 1 value)))
                                               (- old-score
                                                  (log (+ -1 (list-ref rawcounts value) (list-ref hyperparams value)))
                                                  (- (log (+ -1 (apply + rawcounts) (apply + hyperparams)))) ;;FIXME: cache.
                                                  ))
                                              
                                              ;;if there's a flag, increment the stats:
                                              (list
                                               (append (take rawcounts value)
                                                       (list (+ (list-ref rawcounts value) 1))
                                                       (drop rawcounts (+ 1 value)))
                                               (+ old-score
                                                           (log (+ (list-ref rawcounts value) (list-ref hyperparams value)))
                                                           (- (log (+ (apply + rawcounts) (apply + hyperparams))))))
                                              ))))
 
 ;score. updated in un/sampler, so here just look up:
 (define dirichlet-discrete-scorer (lambda (stats hyperparams) 
                                     ;(display "score: ")(display stats)(newline)
                                     (second stats)))
 ;                                             (- (- (lngamma (+ (apply + stats) (apply + hyperparams)))
 ;                                                   (apply + (map lngamma (map + stats hyperparams))))
 ;                                                (- (lngamma (apply + hyperparams))
 ;                                                   (apply + (map lngamma hyperparams))))))
 
 
 ;;this proposer samples uniformly from all the possible values except the current value.
 ;;NOTE: this doesn't know what to do if there's only one item in the discrete list (i.e. (discrete '(1)))....
 ;;      also doesn't work right if any hyperparam is 0 (which is ill-defined anyway).
 (define dirichlet-discrete-uniform-proposer (lambda (operator operands old-value)
                                               ;(display "dirichlet-discrete-uniform-proposer\n")(newline)
                                               (let* ((hyperparams (xrp->hyperparams operator))
                                                      (old-stats (xrp->stats operator))
                                                      ;;unsample the current value:
                                                      (base-stats (apply-with-church (xrp->unsampler operator) 
                                                                                 (list old-value old-stats hyperparams) 
                                                                                 ))
                                                      ;;choose a new value (uniformly)
                                                      (num-vals (length (first old-stats)))
                                                      (possible-values (append (iota old-value) 
                                                                               (iota (- num-vals old-value 1) (+ 1 old-value) )))
                                                      ;(possible-values (append (iota num-vals) ))
                                                      (new-value (uniform-draw possible-values))
                                                      ;;increment the stats for this new value
                                                      (new-stats (apply-with-church (xrp->unsampler operator) 
                                                                                (list new-value 
                                                                                      base-stats 
                                                                                      hyperparams 
                                                                                      'increment-stats) 
                                                                                )))
                                                 ;;update stats:
                                                 (update-xrp-stats operator new-stats)
                                                 ;;return new value, f/b cancel since this proposal is symmetric.
                                                 (values new-value LOG-PROB-1 LOG-PROB-1))))
 
 
 ;;proportional version: sample proportional to score, but don't sample current value.
 (define dirichlet-discrete-weighted-proposer (lambda (operator operands old-value)
                                               ;(display "dirichlet-discrete-uniform-proposer\n")(newline)
                                               (let* ((hyperparams (xrp->hyperparams operator))
                                                      (old-stats (xrp->stats operator))
                                                      ;;unsample the current value:
                                                      (base-stats (apply-with-church (xrp->unsampler operator) 
                                                                                     (list old-value old-stats hyperparams) 
                                                                                     ))
                                                      ;;choose a new value (uniformly)
                                                      (num-vals (length (first old-stats)))
                                                      (possible-values (append (iota old-value) 
                                                                               (iota (- num-vals old-value 1) (+ 1 old-value) )))
                                                      ;(possible-values (iota num-vals) )
                                                      (scores (map (lambda (i) 
                                                                     (apply-with-church 
                                                                      (xrp->scorer operator) 
                                                                      (list (apply-with-church 
                                                                             (xrp->unsampler operator) 
                                                                             (list i base-stats hyperparams 'increment-stats) 
                                                                             )
                                                                            hyperparams)
                                                                      ))
                                                                   possible-values))
                                                      (norm (logsumexp scores))
                                                      (normed-scores (map (lambda (s) (exp (- s norm))) scores))
                                                      (new-value (sample-discrete normed-scores))
                                                      ;;increment the stats for this new value
                                                      (new-stats (apply-with-church (xrp->unsampler operator) 
                                                                                    (list new-value 
                                                                                          base-stats 
                                                                                          hyperparams 
                                                                                          'increment-stats) 
                                                                                    )))
                                                 
                                                 ;;update stats:
                                                 (update-xrp-stats operator new-stats)
                                                 (values new-value 
                                                         (log (list-ref normed-scores new-value)) 
                                                         (log (list-ref normed-scores old-value))))))

 (define (apply-with-church op args)
   (proposal->value (trace-apply op args '() *wildcard-constraint-set*)))

 (define (logsumexp x) 
   (log (apply + (map exp x))))


 (register-primitive-procedure! 'dirichlet-discrete-init-stats dirichlet-discrete-init-stats)
         (register-primitive-procedure! 'dirichlet-discrete-sampler dirichlet-discrete-sampler)
         (register-primitive-procedure! 'dirichlet-discrete-unsampler dirichlet-discrete-unsampler)
         (register-primitive-procedure! 'dirichlet-discrete-scorer dirichlet-discrete-scorer)
         (register-primitive-procedure! 'dirichlet-discrete-uniform-proposer dirichlet-discrete-uniform-proposer)
  
 )
