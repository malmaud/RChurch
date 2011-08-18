#!r6rs

(import (church utils rnrs)
        (rnrs eval)
        (church church)
        (church utils utils)
        (church readable-scheme)
        (church mcmc queries gradient-mh-query)
        (_srfi :1))


;; settings

(define query 'gradient-query)
(define num-samples 10000)
(define lag 1)
(define dt 0.1)
(define num-lp-steps 5)
(define runs 5)


;; queries

(register-primitive-procedure! 'gradient-query gradient-query)
(register-primitive-procedure! 'hmc-query hmc-query)


;; infrastructure

(define failed-tests '())

(define (run-test num-samples lag dt num-lp-steps runs model expectation-fn true-expectation tolerance message)
    (let* ((init-expr
            '((debug-mode 'verbosity 5)
              (define (gaussian-lnpdf val mu sigma) (* -0.5 (+ (+ 1.8378770664093453 (log sigma)) (/ (* (- val mu) (- val mu)) sigma) )) )
              (define (noisy= x y noise) (log-flip (gaussian-lnpdf (- x y) 0.0 noise)))))

           ;; run test with gradient query
           ;(grad-eval-expr (append init-expr (list `(,query ,num-samples ,lag ,dt ,num-lp-steps ,@def-exps ,query-exp ,cond-exp))))
           (grad-eval-expr (append init-expr (list `(,query ,num-samples ,lag ,dt ,@model))))
           (grad-results (repeat runs (lambda () (scored-value->value (church-top-level-eval grad-eval-expr)))))
           (grad-estimates (map (lambda (run) (exact->inexact (mean (map expectation-fn run)))) grad-results))

           ;; for comparison, also run test with standard mh
           (base-eval-expr (append init-expr (list `(mh-query ,num-samples ,lag ,@model))))
           (base-results (repeat runs (lambda () (scored-value->value (church-top-level-eval base-eval-expr)))))
           (base-estimates (map (lambda (run) (exact->inexact (mean (map expectation-fn run)))) base-results))
           
           (errors (map (lambda (estimate) (abs (- estimate true-expectation))) grad-estimates))
           (mean-abs-error (mean errors))          
           (std-error (sqrt (/ (mean (map (lambda (x) (* x x)) errors)) (length errors)))))
      (when (> mean-abs-error tolerance)
            (set! failed-tests (cons message failed-tests)))
      (for-each display (list
                         "\n\nTest: " message 
                         "   samples: " num-samples 
                         "   lag: " lag
                         "   dt: " dt
                         "   num-lp-steps: " num-lp-steps
                         "   runs: " runs "\n"
                         "\n            mean expected value: " (mean grad-estimates)
                         "\n                       variance: " (variance grad-estimates) "   " grad-estimates
                         "\n mean expected value (w/o grad): " (mean base-estimates)                         
                         "\n            variance (w/o grad): " (variance base-estimates) "   " base-estimates 
                         "\n            true expected value: " (exact->inexact true-expectation)
                         "\n                mean abs. error: " mean-abs-error
                         "\n                 standard error: " std-error
                         (if (<= mean-abs-error tolerance) "" "\n      POSSIBLE FAILURE!\n")))))

(define (mean lst)
 (/ (apply + lst) (length lst)))

(define (variance lst)
 (let ((mn (mean lst)))
   (mean (map (lambda (x) (expt (- mn x) 2)) lst))))

(define (std lst) 
  (sqrt (variance lst)))


;; tests

(run-test
 num-samples lag dt num-lp-steps runs
 '( (define x (gaussian 5 2))
    x
    true )
 (lambda (x) x)
 5.0
 .5
 "unconditional gaussian")

(run-test
 3000 lag .1 num-lp-steps runs
 '( (define m (uniform -5 5))
    (define gen-data (repeat 3 (lambda () (gaussian m 0.1))))
    m
    (and
     (noisy= (first gen-data) 3.9 0.01)
     (noisy= (second gen-data) 4.0 0.01)
     (noisy= (third gen-data) 4.1 0.01)) )
 (lambda (x) x)
 4.0
 .5
 "inferring the mean of a gaussian")

(run-test
 num-samples lag dt num-lp-steps runs
 '( (define reflectance (gaussian 1 1))
    (define illuminance (gaussian 3 0.5))
    (define lightness (* reflectance illuminance))
    reflectance
    (noisy= lightness 5.0 0.1) )
 (lambda (x) x)
 1.6 ;; estimated from mh samples (seems to be slightly off, should re-estimate with rejection)
 .4
 "reflectance/illuminance test")

(run-test
 num-samples lag dt num-lp-steps runs
 '( (define a (gaussian 0.0 1.0))
    (define b (if (< a 0.0) (uniform-draw '(-2.0 -3.0)) 1.0) )
    (define c (* a b))
    a
    (noisy= c 0.5 0.1) )
 (lambda (x) x)
 0.28 ;; estimated by a whole bunch of mh-query samples.
 .05
 "stratified (mixed real/discrete erps) test") ;;this one seems not to mix with gradient-query, but do ok with hmc-query given appropriate step size. 

;; (run-test
;;  num-samples lag dt num-lp-steps runs
;;  '('(define a (gaussian 0.0 1.0))
;;    '(define b (if (flip) (gaussian 0.0 1.0) 0.5))
;;    '(define c (* a b)) )
;;  'a
;;  '(noisy= c 0.5 0.1)
;;  (lambda (x) x)
;;  0.255 ;; FIXME: need to get independent estimate....
;;  .4
;;  "stratified (mixed real/discrete erps) test 2")


;;FIXME: need test that AD is playing nice with eval short-circuiting in trace-update.

;; failed tests?

(if (null? failed-tests)
    (display "\n\nAll tests passed.\n")
    (begin (display "\n\nSome tests failed.\n")
           (pretty-print (reverse failed-tests))))
