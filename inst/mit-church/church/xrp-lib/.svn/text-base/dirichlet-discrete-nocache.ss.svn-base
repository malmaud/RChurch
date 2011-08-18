#!r6rs

(library (church xrp-lib dirichlet-discrete)
         
         (export dirichlet-discrete-sampler dirichlet-discrete-unsampler dirichlet-discrete-scorer)
         
         (import (church utils rnrs)
                 (_srfi :1)
                 (church readable-scheme)
                 (church external math-env))
         
         ;;FIXME: cache scores in stats on sample/unsample.
                  
         ;sample conditioned on previous samples, return list of sampled value and new stats
         (define dirichlet-discrete-sampler (lambda (stats hyperparams)
                                         ;(display "sample: ")(display stats)(newline)
                                         (let* ((counts (map + stats hyperparams))
                                                (total-counts (apply + counts))
                                                (probs (map (lambda (c) (/ c total-counts)) counts))
                                                (value (sample-discrete probs))
                                                (new-stats (append (take stats value)
                                                                   (list (+ 1 (list-ref stats value)))
                                                                   (drop stats (+ 1 value)))))
                                           (list value new-stats))))
         
         ;unsample a particular sample, return new stats:
         (define dirichlet-discrete-unsampler (lambda (value stats hyperparams . flag) 
                                           ;(display "unsample: ")(display stats)(newline)
                                                (if (null? flag)
                                                    (append (take stats value)
                                                            (list (- (list-ref stats value) 1))
                                                            (drop stats (+ 1 value)))
                                                    
                                                    ;;if there's a flag, increment the stats:
                                                    (append (take stats value)
                                                            (list (+ (list-ref stats value) 1))
                                                            (drop stats (+ 1 value)))
                                                    )))
         
         ;score. ;;FIXME: do domain checking...?
         (define dirichlet-discrete-scorer (lambda (stats hyperparams) 
                                        ;(display "score: ")(display stats)(newline)
                                             (- (- (lngamma (+ (apply + stats) (apply + hyperparams)))
                                                   (apply + (map lngamma (map + stats hyperparams))))
                                                (- (lngamma (apply + hyperparams))
                                                   (apply + (map lngamma hyperparams))))))
         
         
         )
