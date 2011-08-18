#!r6rs

;; created on Jun 17, 2008 by NDG
;; authors: noah goodman, daniel roy, andreas stuhlmuller

(library (church mcmc queries mh-query)

         (export repeated-mh-lex-query ;;deprecated
                 mh-lex-query  ;;deprecated
                 repeated-mh-def-query  ;;deprecated
                 )
         
         (import (church utils rnrs)
                 (_srfi :1)
                 (church utils utils)
                 (church constraint-propagation trace-constraint-propagation)
                 (church mcmc mcmc-core)
                 (church readable-scheme)
                 ;(church church-eval laziness)
                 (church church-eval logmath)
                 (church church-eval syntax)
                 (church church-eval church-eval)
                 ;(church church-eval traces)
                 )
         
         ;; note that mh-query takes a conditioning expression (not procedure), which must be a lambda.
         ;; mh-lex-query works as expected.
         ;; repeated-mh-lex-query lets you get multiple samples from a single chain.
         ;; see also tempered and annealed versions in temperature-games.ss
         
         ;; basic queries (for more, see temperature-games):
         
         (define (repeated-mh-lex-query M N lex-exp query-exp cond-exp env)
           (let ((count -1)
                 (samples '()))
             (mcmc-lex-query default-initialize 
                             (repeat-kernel N (make-mh-kernel (make-trace-walk-proposal) trace-score-proc "trace-walk-kernel (rmhl)"))
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
         
         ;; (define (mh-query N query-exp filter-exp env)
;;            (mcmc-query default-initialize 
;;                        (repeat-kernel N (make-mh-kernel (make-trace-walk-proposal) trace-score-proc "trace-walk-kernel (mh)"))
;;                        (let ((count -1))
;;                          (lambda (trace) 
;;                            (if (= count -1)
;;                                (begin (set! count 0) false)
;;                                true))) ;;stop after the first kernel application, because the iterations are handle via a repeat-kernel  
;;                        trace-finish
;;                        query-exp filter-exp env))

         (define (mh-lex-query N lex-exp query-exp cond-exp env)
           (first ;;first because mh-lex-query is defined to return a single sample, not a list with one sample in it.
            (repeated-mh-lex-query 1 N lex-exp query-exp cond-exp env)))



         (define (repeated-mh-def-query M N . args);defs query-exp cond-exp env)
           (let* ((nargs (length args))
                 (env (list-ref args (- nargs 1)))
                 (cond-exp (list-ref args (- nargs 2)))
                 (query-exp (list-ref args (- nargs 3)))
                 (defs (take args (- nargs 3))))
             (mcmc-def-query default-initialize 
                             (repeat-kernel N (make-mh-kernel (make-trace-walk-proposal) trace-score-proc "trace-walk-kernel (rmhl)"))
                             M
                             (lambda (s) true) ;;every time a cycle is up, take a sample
                             trace-finish 
                             defs query-exp cond-exp env)))

         
         (when (> (*verbosity*) 12)
           (display "loaded mh-query\n"))

)