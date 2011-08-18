#!r6rs

;;multiple-try metropolis query

(library (church mcmc queries mtm-query)
         
         (export mtm-lex-query)
         
         (import (church utils rnrs)
                 (_srfi :1)
                 (church utils utils)
                 (church constraint-propagation trace-constraint-propagation)
                 (church mcmc mcmc-core)
                 (church readable-scheme)
                 (church external math-env)
                 ;(church church-eval laziness)
                 (church church-eval logmath)
                 (church church-eval syntax)
                 (church church-eval church-eval)
                 ;(church church-eval traces))
                 )
         
         ;; note that mh-query takes a conditioning expression (not procedure), which must be a lambda.
         ;; mh-lex-query works as expected.
         ;; repeated-mh-lex-query lets you get multiple samples from a single chain.
         ;; see also tempered and annealed versions in temperature-games.ss
         
         ;; basic queries (for more, see temperature-games):
         
         (define (mtm-lex-query M N tries lex-exp query-exp cond-exp env)
           (let ((count -1)
                 (samples '()))
             (mcmc-lex-query default-initialize 
                             (repeat-kernel N (make-mtm-kernel (make-trace-walk-proposal env) trace-score-proc tries "mtm-kernel"))
                             (lambda (trace) 
                               (if (= count -1)
                                   (begin (set! count 0) false)
                                   (if (< count M)
                                       (begin (set! count (+ count 1))
                                              (set! samples (append samples (list (trace-finish trace))))
                                              (when (> (*verbosity*) 11) (display "   Sample ")(display count)(newline))
                                              false)
                                       true)))
                             (lambda (trace) samples)
                             lex-exp query-exp cond-exp env)))
         
         
         (define (make-mtm-kernel proposal score-proc tries kernel-name) 
           (lambda (state)
             (let* ((old-score (score-proc state))
                    (Y-moves (map proposal (make-list tries state)))
                    ;(move-forw-scores (map move-fp moves))
                    (Y-rev-scores (map move-rp Y-moves))
                    (Y-scores (map (compose score-proc move-state) Y-moves))
                    (Y-weights (map exp (map + Y-scores Y-rev-scores)))
                    (Y-weight-normalizer (apply + Y-weights)))
               (if (= Y-weight-normalizer 0.0)
                   (begin
                     (when (or (*global-debug*) (*mh-steps*) (*mh-statistics*))
                       (update-proposal-statistics! kernel-name false))
                     state)
                   (let* 
                       ((selected-Y-index (sample-discrete (map (lambda (w) (/ w Y-weight-normalizer)) Y-weights)))
                        (selected-Y (move-state (list-ref Y-moves selected-Y-index)))
                        
                        (balance-moves (map proposal (make-list (- tries 1) selected-Y)))
                        (balance-rev-scores (map move-rp balance-moves))
                        (balance-scores (map (compose score-proc move-state) balance-moves))
                        (balance-weights (map exp (map + balance-scores balance-rev-scores)))
                        (balance-weight-normalizer (+ (apply + balance-weights)
                                                      (exp (+ old-score (move-fp (list-ref Y-moves selected-Y-index))))))
                        
                        (acceptance-prob (min 1
                                              (/ Y-weight-normalizer balance-weight-normalizer))) ;;FIXME: in log-space?
                        (accept (flip acceptance-prob)) )
                     
                     ; update proposal acceptance statistics
                     (when (or (*global-debug*) (*mh-steps*) (*mh-statistics*))
                       (update-proposal-statistics! kernel-name accept))
                     
                     (when (or (*global-debug*) (*mh-steps*))
                       (let ((kernel-statistics (get-proposal-statistics kernel-name)))
                         (for-each display
                                   (list 
                                    kernel-name "\n"
                                    (if accept "  accept." "  reject.")
                                    "  acceptance probability: " acceptance-prob " \n"
                                    "  accepted: " (num-accepted kernel-statistics) "/" (num-proposals kernel-statistics)
                                    "\n\n\n"))))
                     
                     (if accept selected-Y state)
                     )))))
         
         
         
         (when (> (*verbosity*) 12)
           (display "loaded mtm-query\n"))
         
         )