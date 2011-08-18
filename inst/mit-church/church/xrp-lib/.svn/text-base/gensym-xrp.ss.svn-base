#!r6rs

;;by making gensym into an xrp we are able to condition on a call to gensym returning a particular symbol.
;;the hyperparams is a superordinate identifier (ie. like (gensym 'kind)).
;;the stats is a list of symbols returned so far.

;;sampler returns a fresh symbol of the appropriate kind.
;;score is 1 if there are no repeats in the stats list, 0 otherwise.


(library (church xrp-lib gensym-xrp)

         (export init-gensym-xrp)
         
         (import (church utils rnrs)
                 (church register-primitives)
                 (_srfi :1)
                 (church readable-scheme)
                 (church church-eval logmath)
                 (church church-eval trace-eval))

         (define (init-gensym-xrp) 'bar);;must export something or you don't get loaded :(
                           
         ;sample conditioned on previous samples, return list of sampled value and new stats
         (define gensym-xrp-sampler (lambda (stats hyperparams)
                                         ;(display "sample: ")(display stats)(newline)
                                         (let* ((value (list hyperparams (gensym)))
                                                (new-stats (pair value stats)))
                                           (list value new-stats))))
         
         ;unsample a particular sample, return new stats:
         (define gensym-xrp-unsampler (lambda (value stats hyperparams . flag) 
                                           ;(display "unsample: ")(display stats)(newline)
                                                (if (null? flag)
                                                    (filter (lambda (v) (not (eq? value v))) stats)
                                                    
                                                    ;;if there's a flag, increment the stats:
                                                    (pair value stats)
                                                    )))
         
         ;score. ;;FIXME: do domain checking...?
         (define gensym-xrp-scorer (lambda (stats hyperparams) 
                                        ;(display "score: ")(display stats)(newline)
                                     (if (duplicates? stats)
                                         LOG-PROB-0
                                         LOG-PROB-1)))

         (define (duplicates? lst)
           (if (null? lst)
               false
               (or (memq (first lst) (rest lst))
                   (duplicates? (rest lst)))))

         (define (isa kind) (lambda (value) (tagged-list? value (xrp->hyperparams kind))))
         (register-primitive-procedure! 'isa isa)
         
         (register-primitive-procedure! 'gensym-xrp-sampler gensym-xrp-sampler)
         (register-primitive-procedure! 'gensym-xrp-unsampler gensym-xrp-unsampler)
         (register-primitive-procedure! 'gensym-xrp-scorer gensym-xrp-scorer)
         
         )
