#!r6rs

(import (church utils rnrs)
       (rnrs eval)
       (church church)
       (church utils utils)
       (church readable-scheme))


(define *display-expr* (make-parameter #t))
(define total-average-time 0)
(define failed-tests (list))
(define eval-env (environment '(rnrs)))

(define (test-mh samples samples-before-swap swaps runs definitions lex-query-triple expectation-fn true-expectation tolerance . message)
 (let ((expr `(tempered-repeated-mh-lex-query ',tempering-vars ',tempering-temps ,samples ,samples-before-swap ,swaps 
                                               ,@lex-query-triple (get-current-environment) ) ))
   (for-each display 
             `("\n\nM-H Test: " ,@message 
               "   samples: " ,samples 
               "   lag: " ,(* samples-before-swap swaps) 
               "   chains: " ,(length (first tempering-temps))
               "   runs: " ,runs "\n"))
   (when (*display-expr*)
     (begin 
       (for-each (lambda (letbinding) 
                   (for-each display (list "let " (first letbinding) " = "))
                   (pretty-print (second letbinding)) )
                 (eval (first lex-query-triple) eval-env) )
       (display "sample: ")
       (pretty-print (eval (second lex-query-triple) eval-env))
       (display "given: ")
       (pretty-print (eval (third lex-query-triple) eval-env))
       ))
   
   (let* ((church-program (append (list '(load "xrps.church")) definitions (list expr)))
          (mh-proc (lambda () (scored-value->value (church-top-level-eval church-program)) ) )
          (info (repeat runs (lambda () (timer mh-proc))))
          (estimates (map (lambda (run)
                            (exact->inexact (mean (map expectation-fn (first run)))))
                          info ))
          (cpu-time (exact->inexact (mean (map second info))))
          (real-time (exact->inexact (mean (map third info))))
          (gc-time (exact->inexact (mean (map fourth info))))
          
          (errors (map (lambda (estimate) (abs (- estimate true-expectation))) estimates))
          (mean-abs-error (mean errors))          
          (std-error (sqrt (/ (mean (map (lambda (x) (* x x)) errors))
                              (length errors)) ) )
          )
     
     (when (> mean-abs-error tolerance)
       (set! failed-tests (cons message failed-tests)) )
     
     (set! total-average-time (+ total-average-time cpu-time))
     (for-each display (list "\n mean expected value: " (mean estimates) 
                             "\n            variance: " (variance estimates) "   " estimates 
                             "\n true expected value: " (exact->inexact true-expectation)
                             "\n     mean abs. error: " mean-abs-error
                             "\n      standard error: " std-error
                             (if (<= mean-abs-error tolerance) "" "\n      POSSIBLE FAILURE!\n")
                             "\n average cpu time used: " (truncate cpu-time) " s"
                             "\n average real time used: " (truncate real-time) " s"
                             "\n average gc time used: " (truncate  gc-time) " s"
                             "\n\n"
                             )))))

;;get a list of consecutive integers:

(define (mean lst)
 (/ (apply + lst) (length lst)))

(define (variance lst)
 (let ((mn (mean lst)))
   (mean (map (lambda (x) (expt (- mn x) 2)) lst))))

(define (std lst) 
 (sqrt (variance lst)))

(define (equal?-test test-name test-value true-value)
 (let* ((test-value (scored-value->value test-value)) ;; grab value form (value . prob) pair returned by church-eval
        (result (equal? test-value true-value))) ;; success?
   (when (not result)
     (set! failed-tests (cons test-name failed-tests)) )
   
   (for-each display (list test-name ": " test-value  (if result " = " " != ") true-value 
                           (if result "  OK!" "  FAILED!") "\n")) ))


;;=== test settings for stochastic tests ===         

(define samples 150)
(define samples-before-swap 5)
(define swaps 4)
(define runs 5)
(define error-tolerance 0.07)

(define tempering-vars '(posterior-temperature noise)) 
(define tempering-temps '((1 10) (0 0.1)))

;;=== stochastic test cases ====

(test-mh samples samples-before-swap swaps runs '()
        '( '((bb (make-dirichlet-discrete (list 0.5 0.5 0.5))))
               '(= (bb) (bb))
               'true )
        (lambda (b) (if b 1 0))
        (/ (+ 1 0.5) (+ 1 (* 3 0.5)))
        error-tolerance
        "symmetric dirichlet-discrete, unconditioned." )

(test-mh samples samples-before-swap swaps runs '()
        '( '((bb (make-dirichlet-discrete (list 0.5 0.5))))
               '(= 0 (bb))
               '(= 0 (bb)) )
        (lambda (b) (if b 1 0))
        (/ (+ 1 0.5) (+ 1 (* 2 0.5)))
        error-tolerance
        "symmetric dirichlet-discrete, conditioned." )

(define crp-param 0.5)
(test-mh samples samples-before-swap swaps runs '()
        `( '((draw-type (make-CRP ,crp-param));(CRPmem 1.0 gensym))
             (class (mem (lambda (x) (draw-type)))))
           '(eq? (class 'bob) (class 'mary))
           '(eq? (class 'bob) (class 'jim)))
        (lambda (x) (if x 1 0))
        (/ 2.0 (+ 2.0 crp-param))
        error-tolerance
        "CRP third customer at first table, conditioned on second customer at first table." )

(test-mh samples samples-before-swap swaps runs '()
        '( '((draw-type (CRPmem 1.0 gensym))
             (class (mem (lambda (x) (draw-type)))))
           '(eq? (class 'bob) (class 'mary))
           'true)
        (lambda (x) (if x 1 0))
        0.5
        error-tolerance
        "CRPmem of gensym, unconditioned." )

(define dirichlet-param 0.01)
(define CRP-param 1.0)
(test-mh samples samples-before-swap swaps runs '()
        `( '((draw-type (make-CRP ,CRP-param))
             (obs (mem (lambda (type) (make-symmetric-dirichlet-discrete 3 ,dirichlet-param)))))
           '(= (sample (obs (draw-type))) (sample (obs (draw-type))))
           'true)
        (lambda (x) (if x 1 0))
        (+ (* (/ 1 (+ 1 CRP-param))  (/ (+ 1 dirichlet-param) (+ 1 (* 3 dirichlet-param))))   ;same crp table, same dirichlet draws
           (* (/ CRP-param (+ 1 CRP-param))   (/ 1 3))) ;different crp tables, same dirichlet draws...
        error-tolerance
        "varying numbers of xrps inside mem." )


;; (define dirichlet-param 0.1)
;; (define CRP-param 1.0)
;; (test-mh samples samples-before-swap swaps runs '()
;;         `( '(;(draw-type (make-CRP ,CRP-param))
;;              ;(a (make-symmetric-dirichlet-discrete 3 ,dirichlet-param))
;;              ;(b (make-symmetric-dirichlet-discrete 3 ,dirichlet-param)))
;;              ;(make-dd (lambda () (let ((weights (dirichlet (list ,dirichlet-param ,dirichlet-param ,dirichlet-param))))
;;              ;                      (lambda () (sample-discrete weights)))))
;;              ;(obs (mem (lambda (type) (make-dd)))))
;;              (obs (mem (lambda (type) (make-symmetric-dirichlet-discrete 3 ,dirichlet-param)))))
;;            '(= (sample (obs true)) (sample (obs (flip ,(/ 1 (+ 1 CRP-param))))))
;;            'true)
;;         (lambda (x) (if x 1 0))
;;         (+ (* (/ 1 (+ 1 CRP-param))  (/ (+ 1 dirichlet-param) (+ 1 (* 3 dirichlet-param))))   ;same crp table, same dirichlet draws
;;            (* (/ CRP-param (+ 1 CRP-param))   (/ 1 3))) ;different crp tables, same dirichlet draws...
;;         error-tolerance
;;         "varying numbers of xrps inside mem." )


;;;;;;;;;;;;;;
;; now see which tests failed

(for-each display
         (list "\n\n Total average time for test suite: " total-average-time "\n\n") )

(if (null? failed-tests)
   (display "All tests passed.\n")
   (begin (display "Some tests failed.\n")
          (pretty-print (reverse failed-tests)) ))

(exit)