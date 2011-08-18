#!r6rs

;; created on June 28, 2008 by Noah Goodman
;; authors: noah goodman

(library (church church-eval laziness)
         
         (export delay-sum
                 eager-sum
                 delay-append
                 eager-append
                 force-lrb
                 force-lrb-fresh
                 lazy-reduce-box?)
         
         (import (church utils rnrs)
                 (_srfi :1)
                 (church readable-scheme))
         
         ;;this file contains utilities to handle lazy objects in traces, 
         ;; which we use in order to delay score, etc, computation for 
         ;; exchangeable erps and mem until all computation in the trace is finished.
         
         ;;this fully delays the expression
         (define-syntax delay-sum
           (syntax-rules ()
             ((delay-sum exp)
              (lazy-reduce-box 0 (list (lambda () exp)) + 0))))
         
         ;;this evaluates let's the expression be eagerly evaluated, but doesn't force lazy elements of the list
         ;; if there are lazy elements it sums any non-lazy values eagerly and re-packages the rest in a lrb,
         ;; if there aren't any lazy elements it returns the sum.
         (define (eager-sum lst)
           (let-values ( ((lrbs vals) (partition lazy-reduce-box? lst)) )
             (if (null? lrbs)
                 (apply + lst)
                 (let ((eager-vals (map lrb->eager-val lrbs))
                       (thunks (apply append (map lrb->thunks lrbs))))
                 (lazy-reduce-box (apply + (append vals eager-vals)) thunks + 0)))))
         
         
         (define-syntax delay-append
           (syntax-rules ()
             ((delay-append exp)
              (lazy-reduce-box '() (list (lambda () exp)) append '()))))
         
         (define (eager-append lst)
           (let-values ( ((lrbs vals) (partition lazy-reduce-box? lst)) )
             (if (null? lrbs)
                 (apply append lst)
                 (let ((eager-vals (map lrb->eager-val lrbs))
                       (thunks (apply append (map lrb->thunks lrbs))))
                   (lazy-reduce-box (apply append (append vals eager-vals)) thunks append '())))))
         
         
         
         ;;a lazy reduce box is: an eagerly reduced value, a list of list-producing thunks, a procedure for reducing lists, an init, an mlist that can store a computed value from the reduce on the thunk.
         (define (lazy-reduce-box val thunks proc init) (list 'lazy-reduce-box val thunks proc init (mlist '())))
         (define (lazy-reduce-box? x) (tagged-list? x 'lazy-reduce-box))
         (define (lrb->eager-val x) (second x))
         (define (lrb->thunks x) (third x))
         (define (lrb->reduce-proc x) (fourth x))
         (define (lrb->reduce-init x) (fifth x))
         (define (lrb->stored-value x) (mcar (sixth x)))
         (define (set-lrb-stored-value! val x) (set-mcar! (sixth x) val))
         
         
         (define (force-lrb-fresh x)
           (if (lazy-reduce-box? x)
               (begin
                 (set-lrb-stored-value! (reduce (lrb->reduce-proc x)
                                                (lrb->reduce-init x)
                                                (pair (lrb->eager-val x)
                                                      (map force-lrb-fresh 
                                                           (apply append
                                                                  (map (lambda (t) (t)) (lrb->thunks x))))))
                                        x)
                 (lrb->stored-value x))
               x))

         (define (force-lrb x)
           (if (lazy-reduce-box? x)
               (if (null? (lrb->stored-value x))
                   (force-lrb-fresh x)
                   (lrb->stored-value x))
               x))

;;          (define (force-lrb-fresh x)
;;            (display "flrb-f on:  ")(display x)(newline)
;;            (let ((ans
;;            (if (lazy-reduce-box? x)
;;                (begin
;;                  (set-lrb-stored-value! (reduce (lrb->reduce-proc x)
;;                                                 (lrb->reduce-init x)
;;                                                 (pair (lrb->eager-val x)
;;                                                       (map force-lrb-fresh 
;;                                                            (apply append
;;                                                                   (map (lambda (t) (t)) (lrb->thunks x))))))
;;                                         x)
;;                  (lrb->stored-value x))
;;                x))
;;                  )
;;              (display "  result: ")(display ans)(newline)
;;              ans))

         
         
;;          (define (force-lrb x)
;;            (display "flrb on: ")(display x)(newline)
;;            (if (lazy-reduce-box? x)
;;                (if (null? (lrb->stored-value x))
;;                    (force-lrb-fresh x)
;;                    (lrb->stored-value x))
;;                x))

)