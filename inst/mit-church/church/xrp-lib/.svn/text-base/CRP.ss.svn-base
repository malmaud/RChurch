#!r6rs

;;The pieces needed for a Chinese Restaurant Process xrp. This is distribution on assignments of symbols to samples
; (i.e. 'partitions'). it returns gensyms, and is only meaningful up to equality of gensyms.

;;the statefull information (stats) stores counts for each symbol that has non-zero counts. 
;currently using an a-list, but this should probably be switched to some faster (pure) data structure.
;stats also stores the score of these counts (in order to do incremental updates to score).

;;the hyperparams is a single number (the concentration parameter).

(library 
 (church xrp-lib CRP)
 
 (export init-CRP canonicalize-symbol-list)
 
 (import (church utils rnrs)
         (_srfi :1)
         (church readable-scheme)
         (church external math-env)
         (church constraint-propagation constraints)
         (church church-eval logmath)
         (church church-eval trace-eval)
         (church church-eval traces)
         (church register-primitives)
         )
 
 (define (init-CRP)'foo)
 
 (define CRP-init-stats (list '() LOG-PROB-1))
 
 ;sample conditioned on previous samples, return list of sampled value and new stats.
 ;incrementally update score.
 (define CRP-sampler (lambda (stats hyperparam)
                       ;(display "sample ")(write stats) (newline) 
                       (let* ((count-map (first stats))
                              (old-score (second stats))
                              (counts (pair hyperparam (map rest count-map)))
                              (total-counts (apply + counts))
                              (probs (map (lambda (c) (/ c total-counts)) counts))
                              (table-index (sample-discrete probs)))
                         (if (= table-index 0)
                             ;;this customer sits at a new table:
                             (let* ((table-symbol (gensym 'CRP))
                                    (new-count-map (pair (pair table-symbol 1) count-map)))
                               (list table-symbol (list new-count-map (+ old-score (list-ref probs table-index)) )))
                             ;;this customer sits at an existing table:
                             (let* ((table-symbol (first (list-ref count-map (- table-index 1))))
                                    (table-count (+ 1 (rest (list-ref count-map (- table-index 1)))))
                                    (new-count-map (append (take count-map (- table-index 1))
                                                           (list (pair table-symbol table-count))
                                                           (drop count-map table-index))))
                               (list table-symbol (list new-count-map (+ old-score (list-ref probs table-index)) )))
                       ))))
 
 ;unsample a particular sample, return new stats.
 ;incrementally update score.
 (define CRP-unsampler (lambda (value stats hyperparam . flag)
                         ;(display "unsample ")(write stats) (newline) 
                         (if (null? flag)
                             
                         (let* ((count-map (first stats))
                                (old-score (second stats))
                                (counts (map rest count-map))
                                (table-index (list-index (lambda (c) (eq? value (first c))) count-map))
                                (table-count (rest (list-ref count-map table-index)))
                                (new-table-count (- table-count 1))
                                (new-count-map (if (= 0 new-table-count)
                                                   (append (take count-map table-index)
                                                           (drop count-map (+ 1 table-index)))
                                                   (append (take count-map table-index)
                                                           (list (pair value new-table-count))
                                                           (drop count-map (+ 1 table-index)))))
                                (new-score (if (= 0 new-table-count)
                                               (- old-score (/ hyperparam (+ hyperparam (apply + counts) (- 1))))
                                               (- old-score (/ new-table-count (+ hyperparam (apply + counts) (- 1)))))))
                           (list new-count-map new-score) )
                         
                         ;;increment stats:
                         (let* ((count-map (first stats))
                                (old-score (second stats))
                                (counts (map rest count-map))
                                (table-index (list-index (lambda (c) (eq? value (first c))) count-map)))
                           (if (false? table-index) 
                               ;;this value is a new table:
                               (list (pair (pair value 1) count-map)
                                     (+ old-score (/ hyperparam (+ hyperparam (apply + counts)))))
                               ;;this value is an existing table:
                               (let* ((table-count (rest (list-ref count-map table-index)))
                                      (new-table-count (+ table-count 1))
                                      (new-count-map (append (take count-map table-index)
                                                             (list (pair value new-table-count))
                                                             (drop count-map (+ 1 table-index))))
                                      (new-score (+ old-score (/ table-count (+ hyperparam (apply + counts))))))
                                 (list new-count-map new-score)) ))
                         
                         )))
 
 ;score.
 ;scoring is done incrementally, in sample and unsample, so here we just look up the stored score.
 (define CRP-scorer (lambda (stats hyperparams) 
                      ;(display "score ")(write stats) (newline) 
                      (second stats)))


 ;;this proposer samples uniformly from existing values or new value except the current value.
 (define CRP-uniform-proposer (lambda (operator operands old-value)
                                               ;(display "CRP-uniform-proposer\n")(newline)
                                               (let* ((hyperparam (xrp->hyperparams operator))
                                                      (old-stats (xrp->stats operator))
                                                      ;;unsample the current value:
                                                      (base-stats (apply-with-church (xrp->unsampler operator) 
                                                                                     (list old-value old-stats hyperparam) ))
                                                      (count-map (first base-stats))
                                                      (table-symbols (map first count-map))
                                                      (possible-table-symbols (pair (gensym 'CRP) ;;new table
                                                                                    (delete old-value table-symbols))) ;;existing symbols except current.
                                                      (new-table-symbol (uniform-draw possible-table-symbols))
                                                      (new-stats (apply-with-church (xrp->unsampler operator) 
                                                                                               (list new-table-symbol 
                                                                                                     base-stats 
                                                                                                     hyperparam
                                                                                                     'increment-stats)))
                                                      ;;  (dummy (for-each display
;;                                                                        (list (canonicalize-symbol-list (pair old-value (map first (first old-stats))))
;;                                                                              (canonicalize-symbol-list (pair new-table-symbol possible-table-symbols))
;;                                                                              (canonicalize-symbol-list (pair new-table-symbol (map first (first new-stats))))
;;                                                                              "\n")))
                                                       )
                                                 ;;increment and update the stats for this new value
                                                 (update-xrp-stats operator new-stats)
                                                 ;;return new value, f/b
                                                 (values new-table-symbol
                                                         (- (log (length possible-table-symbols)))
                                                         (- (log (+ 1 (length (delete new-table-symbol table-symbols)))))
                                                         ))))

 
 ;;a utility for displaying lists of gensyms, such as those returned by CRP:
 (define (canonicalize-symbol-list lst)
   (map (lambda (sym) (list-index (lambda (s) (eq? s sym)) lst)) lst))

 (define (apply-with-church op args )
   (proposal->value (trace-apply op args '() *wildcard-constraint-set*)))

 (register-primitive-procedure! 'CRP-sampler CRP-sampler)
 (register-primitive-procedure! 'CRP-unsampler CRP-unsampler)
 (register-primitive-procedure! 'CRP-scorer CRP-scorer)
 (register-primitive-constant! 'CRP-init-stats CRP-init-stats)
 (register-primitive-procedure! 'CRP-uniform-proposer CRP-uniform-proposer)
 


 )