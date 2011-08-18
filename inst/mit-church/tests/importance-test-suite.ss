#!r6rs

(import (church church)
        (church readable-scheme)
        (church utils rnrs)
        (church utils utils)
        (church church-eval logmath)
        (church constraint-propagation constraints)
        (church church-eval church-eval)
        (church church-eval trace-eval)
        (church church-eval syntax)
        (church standard-env))

(define *tolerance* 0.01)
(define *num-samples* 50000)

(define failed-tests '())

(define (mean x)
  (/ (sum x) (length x)))

(define (importance-expectation vals imp-weights fn)
  (let* ((Z (mean imp-weights)))
    (/ (mean (map * imp-weights (map fn vals))) Z)))

(define (log-mean x)
  ;; FIXME: use proper log-sum-exp
  (log (mean (map exp x))))

(define (failed? empirical-val expected-val tolerance)
  (> (abs (- empirical-val expected-val)) tolerance))

(define (lazy-church-eval syntax env constraint-list)
  (parameterize ([*lazy* true]
                 [*adaptation* true])
                (church-eval syntax env 'constraints constraint-list)))

(define (run-test-case expr constraint-list expected-value . verbose)
  (let* ([env (setup-environment)]
         [syntax (sexpr->syntax expr env)])
    (let-values ([(tc fw bw) (lazy-church-eval syntax env constraint-list)])
      (let ([val (trace-container->value tc)]
            [score (trace-container->score tc)]
            [stack (trace-container->erps tc)])
        (when (or (null? verbose) (first verbose))
              (for-each display
                        (list expr " -> " constraint-list
                              "\n -- got " val ", expected " expected-value
                              "\n -- score " score ", importance score " fw
                              ;; "\n stack: " stack
                              "\n\n")))
        (list val score stack fw bw)))))

(define (run-expectation-test-case title fn expr constraint-list expected-val expected-score num-runs tolerance)
  (let* ([test-runner (lambda () (run-test-case expr constraint-list expected-val false))]
         [test-results (repeat num-runs test-runner)]
         [vals (map first test-results)]
         [scores (map second test-results)]
         [num-rejects (length (filter (lambda (x) (eq? x LOG-PROB-0)) scores))]
         [fw-scores (map fourth test-results)]
         [weights (map (lambda (log-p log-q) (exp (- log-p log-q))) scores fw-scores)]
         [empirical-expected-val (importance-expectation vals weights fn)]
         [empirical-expected-score (log (mean weights))]
         [failed (or (failed? empirical-expected-val expected-val tolerance)
                     (failed? empirical-expected-score expected-score tolerance))])
    (when failed (set! failed-tests (cons title failed-tests)))
    (for-each display
              (list title
                    "\n ** " expr " -> " constraint-list ", " num-runs "x"
                    "\n -- got f-avg " empirical-expected-val ", expected " expected-val
                    "\n -- with marginal score " empirical-expected-score ", expected " expected-score
                    "\n -- rejection rate: " (/ num-rejects num-runs)
                    "\n -- " (if failed "potential FAILURE!" "passed.") "\n\n"))))


;; ------------------------------------------------------------
;; Tests

(run-expectation-test-case
 "unconstrained weighted flip"
 (lambda (x) (if (true? x) 1.0 0.0))
 '(flip .3)
 'wildcard
 0.3 ; expected value
 (log 1.0) ; expected marginal score
 *num-samples*
 *tolerance*
 )

(run-expectation-test-case
 "constrained weighted flip"
 (lambda (x) (if (true? x) 1.0 0.0))
 '(flip .3)
 (list true)
 1.0 ; expected value
 (log .3) ; expected marginal score
 *num-samples*
 *tolerance*
 )

(run-expectation-test-case
 "fn that only terminates if we are lazy"
 (lambda (x) (if (true? x) 1.0 0.0))
 '(begin
    (define (f) (f))
    (define (g x) true)
    (g (f)))
 'wildcard
 1.0 ; expected value
 (log 1.0) ; expected score
 *num-samples*
 *tolerance*
 )

(run-expectation-test-case
 "unconstrained primitive procedure"
 (lambda (x) (if (true? x) 1.0 0.0))
 '(begin
    (define (f x) (and x x))
    (f true))
 'wildcard
 1.0 ; expected value
 (log 1.0) ; expected score
 *num-samples*
 *tolerance*
 )

(run-expectation-test-case
 "check that top level forces delayed vals"
 (lambda (x) (if (true? x) 1.0 0.0))
 '(begin
    (define (f x) x)
    (f true))
 'wildcard
 1.0 ; expected value
 (log 1.0) ; expected score
 *num-samples*
 *tolerance*
 )

(run-expectation-test-case
 "constrained gaussian"
 (lambda (x) x)
 '(gaussian 1 1)
 (list 2.0)
 2.0 ; expected value
 (log 0.2420) ; expected score
 *num-samples*
 *tolerance*
 )

(run-expectation-test-case
 "inversion of 'or"
 (lambda (x) (if (true? x) 1.0 0.0))
 '(or (flip 0.02) (flip 0.07))
 (list true)
 1.0 ; expected value
 (log (+ (* .02 .07) (* .02 .93) (* .98 .07))) ; expected score
 (* *num-samples* 2)
 *tolerance*
 )

(run-expectation-test-case
 "primitive for which (currently) no inverse exists"
 (lambda (x) (if (true? x) 1.0 0.0))
 '(last (list (flip .02) (flip .07)))
 (list true)
 1.0 ; expected value
 (log .07) ; expected score
 *num-samples*
 0.1 ; higher tolerance since we are doing rejection sampling when no inverse exists
 )

(run-expectation-test-case
 "compound procedure"
 (lambda (x) (if (true? x) 1.0 0.0))
 '(begin
    (define (f x y) (or x y))
    (f (flip 0.02) (flip 0.07)))
 (list true)
 1.0 ; expected value
 (log (+ (* .02 .07) (* .02 .93) (* .98 .07))) ; expected score
 *num-samples*
 *tolerance*
 )

(run-expectation-test-case
 "equal?-inverse"
 (lambda (x) (if x 1.0 0.0))
 '(equal? (flip 0.02) (flip 0.93))
 (list true)
 1.0
 (log (+ (* .02 .93) (* .98 .07)))
 *num-samples*
 *tolerance*
 )

(run-expectation-test-case
 "dependent primitive procedure operands (high variance without adaptation since it defaults to rejection)"
 (lambda (x) (if x 1.0 0.0))
 '(begin
     (define (f x) (or x x))
     (f (flip 0.03)))
 (list true)
 1.0
 (log .03)
 *num-samples*
 *tolerance*
 )

(run-expectation-test-case
 "Compound constraints (not supported yet)" 
 (lambda (x) (if (equal? x (list true true)) 1.0 -1.0))
 '(list (flip) (flip))
 (list (list true *wildcard-value*))
 0.0
 (log 1.0)
 *num-samples*
 *tolerance*)

(run-expectation-test-case
 "sample-integer with constraint" 
 (lambda (x) x)
 '(sample-integer 1000000)
 (list 5)
 5.0
 (log (/ 1 1000000))
 *num-samples*
 *tolerance*)

(run-expectation-test-case
 "begin with constraint" 
 (lambda (x) (if x 1.0 -1.0))
 '(begin (flip .002) (flip .00000003))
 (list true)
 1.0
 (log .00000003)
 *num-samples*
 *tolerance*)

(run-expectation-test-case
 "quotes (reject all samples)"
 (lambda (x) 1.0)
 '(list 'a)
 (list (list 'b))
 +nan.0
 (log 0.0)
 *num-samples*
 *tolerance*)

(run-expectation-test-case
 "if with constraint (won't work without adaptation)" 
 (lambda (x) (if (true? x) 1.0 -1.0))
 '(if (flip .00000000001) (flip) false)
 (list true)
 1.0
 (log .000000000005)
 *num-samples*
 *tolerance*)

(run-expectation-test-case
 "test for combination of constraint propagation and adaptation"
 (lambda (x) (if (true? x) 1.0 -1.0))
 '(let ([a (flip .0001)]
        [b (flip .999999999)])
    (equal? a b))
 (list true)
 1.0
 (log (+ (* .0001 .999999999)
         (* .9999 .000000001)))
 100
 *tolerance*)


;; ------------------------------------------------------------
;; Show failed tests

(when (not (null? failed-tests))
      (for-each display
                (list "------------------------------------------------------------\n"
                "Potentially FAILED tests:\n\n"))
      (map (lambda (x) (begin (display "- ") (display x) (display "\n")))
           failed-tests))

(exit)