#!r6rs

(library 
 (church xrp-lib normal-normal-gamma)
 
 (export init-normal-normal-gamma)
 
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

 (define (init-normal-normal-gamma) 'foo)
 
 ;;this is an xrp for a collapsed normal-normal-gamma conjugate model. (See K. Murphy notes "Normal Gamma Model", Oct. 2007.)
 ;;hyperparams are (mu kappa alpha beta)
 ;;the sufficient statistics are sample size, mean, and variance. we keeps stats (sample-size sum-samples sum-squared-samples) 
 ;;  which can be easily updated, and used to compute other stats.
 ;;score is updated when stats change, for efficiency. (FIXME: is there an incremental score update? use more caching?)
 
 (define (make-hyperparams mu kappa alpha beta) (list mu kappa alpha beta))
 (define (mu hyperparams) (first hyperparams))
 (define (kappa hyperparams) (second hyperparams))
 (define (alpha hyperparams) (third hyperparams))
 (define (beta hyperparams) (fourth hyperparams))
 
 (define (posterior-hyperparams sample-size sum-samples sum-squared-samples hyperparams) 
   (let ((sample-mean (if (= 0 sample-size) 1 (/ sum-samples sample-size))))
     (make-hyperparams
      (/ (+ (* (kappa hyperparams) (mu hyperparams)) (* sample-size sample-mean))
         (+ (kappa hyperparams) sample-size))
      (+ (kappa hyperparams) sample-size)
      (+ (alpha hyperparams) (/ sample-size 2))
      (+ (beta hyperparams) 
         (/ (+ sum-squared-samples (* sample-size sample-mean sample-mean) (* -2 sample-mean sum-samples)) 2)
         (/ (* (kappa hyperparams) sample-size (expt (- sample-mean (mu hyperparams)) 2))
            (* 2 (+ (kappa hyperparams) sample-size)))))))
 
 (define (score sample-size sum-samples sum-squared-samples hyp)
   (let ((post-hyp (posterior-hyperparams sample-size sum-samples sum-squared-samples hyp)))
     (+ (lngamma (alpha post-hyp))
        (- (lngamma (alpha hyp)))
        (* (alpha hyp) (log (beta hyp)))
        (- (* (alpha post-hyp) (log (beta post-hyp))))
        (* 0.5 (log (kappa hyp)))
        (- (* 0.5 (log (kappa post-hyp))))
        (- (* (/ sample-size 2) (log (* 2 pi)))))))
 
 (define (normal-normal-gamma-init-stats hyperparams)
   (list 0 0.0 0.0 (score 0 0.0 0.0 hyperparams)))
 
 ;sample conditioned on previous samples, the posterior-predictive is a generalized t-distribution.
 ;return list of sampled value and new stats
 (define normal-normal-gamma-sampler (lambda (stats hyperparams)
                                       ;(display "sample: ")(display stats)(newline)
                                       (let* ((post-hyp (posterior-hyperparams (first stats) (second stats) (third stats) hyperparams))
                                              (nu (* 2 (alpha post-hyp)))
                                              (mu (mu post-hyp))
                                              (sigma-sq (/ (* (beta post-hyp) (+ (kappa post-hyp) 1)) (* (alpha post-hyp) (kappa post-hyp))))
                                              (value (sample-generalized-tdist nu mu sigma-sq)))
                                         (list value
                                               (let ((new-sample-size (+ (first stats) 1))
                                                     (new-sum-samples (+ (second stats) value))
                                                     (new-sum-squared-samples (+ (third stats) (expt value 2))))
                                                 (list new-sample-size 
                                                       new-sum-samples 
                                                       new-sum-squared-samples
                                                       (score new-sample-size new-sum-samples new-sum-squared-samples hyperparams)))))))
                                       
 
 ;unsample a particular sample, return new stats:
 (define normal-normal-gamma-unsampler (lambda (value stats hyperparams . flag) 
                                         ;(if (null? flag) (display "unsample: ") (display "unsample (increment): "))(display stats)(newline)
                                         (let ((sample-size (first stats))
                                               (sum-samples (second stats))
                                               (sum-squared-samples (third stats)))
                                           (if (null? flag)
                                               
                                               (let ((new-sample-size (- sample-size 1))
                                                     (new-sum-samples (- sum-samples value))
                                                     (new-sum-squared-samples (- sum-squared-samples (expt value 2))))
                                                 (list new-sample-size 
                                                       new-sum-samples 
                                                       new-sum-squared-samples
                                                       (score new-sample-size new-sum-samples new-sum-squared-samples hyperparams)))
                                               
                                               ;;if there's a flag, increment the stats:
                                               (let ((new-sample-size (+ sample-size 1))
                                                     (new-sum-samples (+ sum-samples value))
                                                     (new-sum-squared-samples (+ sum-squared-samples (expt value 2))))
                                                 (list new-sample-size 
                                                       new-sum-samples 
                                                       new-sum-squared-samples
                                                       (score new-sample-size new-sum-samples new-sum-squared-samples hyperparams)))
                                               ))))
 
 ;score. updated in un/sampler, so here just look up:
 (define normal-normal-gamma-scorer (lambda (stats hyperparams) 
                                      ;(display "score: ")(display stats)(newline)
                                      (fourth stats)))
 
 
; ;;this proposer samples uniformly from all the possible values except the current value.
; ;;NOTE: this doesn't know what to do if there's only one item in the discrete list (i.e. (discrete '(1)))....
; ;;      also doesn't work right if any hyperparam is 0 (which is ill-defined anyway).
; (define normal-normal-gamma-uniform-proposer (lambda (operator operands old-value)
;                                                ;(display "normal-normal-gamma-uniform-proposer\n")(newline)
;                                                (let* ((hyperparams (xrp->hyperparams operator))
;                                                       (old-stats (xrp->stats operator))
;                                                       ;;unsample the current value:
;                                                       (base-stats (apply-with-church (xrp->unsampler operator) 
;                                                                                      (list old-value old-stats hyperparams) 
;                                                                                      ))
;                                                       ;;choose a new value (uniformly)
;                                                       (num-vals (length (first old-stats)))
;                                                       (possible-values (append (iota old-value) 
;                                                                                (iota (- num-vals old-value 1) (+ 1 old-value) )))
;                                                       ;(possible-values (append (iota num-vals) ))
;                                                       (new-value (uniform-draw possible-values))
;                                                       ;;increment the stats for this new value
;                                                       (new-stats (apply-with-church (xrp->unsampler operator) 
;                                                                                     (list new-value 
;                                                                                           base-stats 
;                                                                                           hyperparams 
;                                                                                           'increment-stats) 
;                                                                                     )))
;                                                  ;;update stats:
;                                                  (update-xrp-stats operator new-stats)
;                                                  ;;return new value, f/b cancel since this proposal is symmetric.
;                                                  (values new-value LOG-PROB-1 LOG-PROB-1))))
; 
; 
; ;;proportional version: sample proportional to score, but don't sample current value.
; (define dirichlet-discrete-weighted-proposer (lambda (operator operands old-value)
;                                                ;(display "dirichlet-discrete-uniform-proposer\n")(newline)
;                                                (let* ((hyperparams (xrp->hyperparams operator))
;                                                       (old-stats (xrp->stats operator))
;                                                       ;;unsample the current value:
;                                                       (base-stats (apply-with-church (xrp->unsampler operator) 
;                                                                                      (list old-value old-stats hyperparams) 
;                                                                                      ))
;                                                       ;;choose a new value (uniformly)
;                                                       (num-vals (length (first old-stats)))
;                                                       (possible-values (append (iota old-value) 
;                                                                                (iota (- num-vals old-value 1) (+ 1 old-value) )))
;                                                       ;(possible-values (iota num-vals) )
;                                                       (scores (map (lambda (i) 
;                                                                      (apply-with-church 
;                                                                       (xrp->scorer operator) 
;                                                                       (list (apply-with-church 
;                                                                              (xrp->unsampler operator) 
;                                                                              (list i base-stats hyperparams 'increment-stats) 
;                                                                              )
;                                                                             hyperparams)
;                                                                       ))
;                                                                    possible-values))
;                                                       (norm (logsumexp scores))
;                                                       (normed-scores (map (lambda (s) (exp (- s norm))) scores))
;                                                       (new-value (sample-discrete normed-scores))
;                                                       ;;increment the stats for this new value
;                                                       (new-stats (apply-with-church (xrp->unsampler operator) 
;                                                                                     (list new-value 
;                                                                                           base-stats 
;                                                                                           hyperparams 
;                                                                                           'increment-stats) 
;                                                                                     )))
;                                                  
;                                                  ;;update stats:
;                                                  (update-xrp-stats operator new-stats)
;                                                  (values new-value 
;                                                          (log (list-ref normed-scores new-value)) 
;                                                          (log (list-ref normed-scores old-value))))))
 
 (define (apply-with-church op args)
   (proposal->value (trace-apply op args '() *wildcard-constraint-set*)))

 (define (logsumexp x) 
   (log (apply + (map exp x))))

 (register-primitive-procedure! 'normal-normal-gamma-init-stats normal-normal-gamma-init-stats)
         (register-primitive-procedure! 'normal-normal-gamma-sampler normal-normal-gamma-sampler)
         (register-primitive-procedure! 'normal-normal-gamma-unsampler normal-normal-gamma-unsampler)
         (register-primitive-procedure! 'normal-normal-gamma-scorer normal-normal-gamma-scorer)
 
 )
