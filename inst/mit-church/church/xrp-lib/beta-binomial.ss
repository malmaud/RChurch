#!r6rs

(library (church xrp-lib beta-binomial)
         
         (export beta-binomial-sampler beta-binomial-unsampler beta-binomial-scorer)
         
         (import (church utils rnrs)
                 (_srfi :1)
                 (church readable-scheme)
                 (church external math-env))
         
         ;;FIXME: cache scores in stats on sample/unsample.
         
         ;sample conditioned on previous samples, return list of sampled value and new stats
         (define beta-binomial-sampler (lambda (stats hyperparams)
                                         ;(display "sample: ")(display stats)(newline)
                                         (let* ((weight (/ (+ (first hyperparams) (first stats))
                                                           (+ (first hyperparams) (second hyperparams) 
                                                              (first stats) (second stats))))
                                                (value (flip weight))
                                                (new-stats (if (true? value) 
                                                               (list (+ 1 (first stats)) (second stats))
                                                               (list (first stats) (+ 1 (second stats))))))
                                           (list value new-stats))))
         
         ;unsample a particular sample, return new stats:
         (define beta-binomial-unsampler (lambda (value stats hyperparams . flag) 
                                           ;(display "unsample: ")(display stats)(newline)
                                           (if (null? flag)
                                               (if (true? value)
                                                   (list (- (first stats) 1) (second stats))
                                                   (list (first stats) (- (second stats) 1)))
                                               (if (true? value)
                                                   (list (+ (first stats) 1) (second stats))
                                                   (list (first stats) (+ (second stats) 1))))))
         
         ;score. ;;FIXME: do domain checking...?
         (define beta-binomial-scorer (lambda (stats hyperparams) 
                                        ;(display "score: ")(display stats)(newline)
                                        (let ((a (first stats))
                                              (b (second stats))
                                              (alpha (first hyperparams))
                                              (beta (second hyperparams)))
                                          (+ (lngamma (+ a b alpha beta))
                                             (- (lngamma (+ a alpha)))
                                             (- (lngamma (+ b beta)))
                                             (lngamma (+ alpha beta))
                                             (- (lngamma alpha))
                                             (- (lngamma beta))))))
         
         
         )
