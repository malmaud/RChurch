(import

 (except (rnrs) real? negative? positive? zero? >= <= > < = atan cos sin expt log exp sqrt / * - + min)
 (church AD) ;;provides overloaded math ops. should only use when we're doing hmc?
 (rename (except (scheme-tools math) lngamma) (sample-discrete discrete-sampler)) ;;this provides the gsl sampling/scoring functions.
 
 (rnrs mutable-pairs) ;;because the header uses set-car!
 (scheme-tools srfi-compat :1) ;;provides some list functions that are used.
 (srfi :19) ;;date+time for inference timing
 
 (rename (only (church readable-scheme)
               gensym ;;this is needed.
               pretty-print
               exact->inexact
               inexact->exact
               )
         (gensym scheme-gensym))

 (church trie)

 ;;to provide eval in church:
 (church compiler)
 (rnrs eval)

 ;(except (scheme-tools srfi-compat :69) string-ci-hash string-hash) ;;used for CGIS, can comment out otherwise...

 (only (scheme-tools) normalize string-sort bin)
 )

;;for score gradients:
(define (*with-score-gradient*) #f)
(define tapify (make-tapifier))
(define (min a b) (if (< a b) a b)) ;;FIXME: proper dmin?
(define (continuous? x) (and (real? x) (not (fixnum? x))))