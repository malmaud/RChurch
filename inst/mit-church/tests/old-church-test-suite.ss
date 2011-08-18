#!r6rs

(import (church utils rnrs)
       (rnrs eval)
       (church church)
       (church utils utils)
       (church readable-scheme))

(define query-type 'mh)

(define *display-expr* (make-parameter #t))
(define total-average-time 0)
(define failed-tests (list))
(define eval-env (environment '(rnrs)))

(define (test-mh samples samples-before-swap swaps runs definitions lex-query-triple expectation-fn true-expectation tolerance . message)
 (let ((expr (case query-type
               ((tempered)  `(tempered-repeated-mh-lex-query ',tempering-vars ',tempering-temps ,samples ,samples-before-swap ,swaps 
                                                              ,@lex-query-triple (get-current-environment) ) )
               ((PTquery) `(PTquery ',tempering-vars ',tempering-temps ,samples ,(* samples-before-swap swaps) 
                                                              ,@lex-query-triple (get-current-environment) ) )
               ((mtm) `(mtm-lex-query ,samples ,(* samples-before-swap swaps) 4 ,@lex-query-triple (get-current-environment) ) )
               ((TT) `(simple-TT-lex-query ',tempering-vars ',TT-base-temps ',TT-temps ,samples ,(* samples-before-swap swaps)
                                                              ,@lex-query-triple (get-current-environment) ) )
               ((mh) `(repeated-mh-lex-query ,samples ,(* samples-before-swap swaps) ,@lex-query-triple (get-current-environment) ) ))))
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
   
   (let* ((mh-proc (lambda () (scored-value->value (church-top-level-eval (append definitions (list expr)) )) ) )
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

(define TT-temps '((1 . (1 0.1)) (1 . (1 0.2))))
(define TT-base-temps '(1 0.0001))


;;=== deterministic test cases ===

(display "\nDeterministic Tests...\n\n")

(equal?-test "deterministic test: define"
            (church-top-level-eval '((define a 10)
                                     a))
            10)

(equal?-test "deterministic test: define 2"
            (church-top-level-eval '((define (foo x . y) y)
                                     (foo 1 2 3 4) ))
            '(2 3 4))

(equal?-test "deterministic test: let-desugar" 
            (church-top-level-eval '((let ((a 1)
                                           (b 2))
                                       'x
                                       (let ((b a)
                                             (a b))
                                         'y
                                         (list a b)))))
            '(2 1) )

(equal?-test "deterministic test: named-let-desugar" 
            (church-top-level-eval '((let loop ((n 4))  ;; computes n'th fibonacci number
                                       (if (<= n 1) 
                                           1
                                           (+ (loop (- n 2))
                                              (loop (- n 1)) )))))
            5 )

(equal?-test "deterministic test: let*-desugar"
            (church-top-level-eval '((let ((a 1)
                                           (b 2))
                                       'x
                                       (let* ((b a)
                                              (a b))
                                         'y
                                         (list a b)))))
            '(1 1) )

(equal?-test "deterministic test: case-desugar"
            (church-top-level-eval '((case (+ 7 1)
                                       ((7 9 11) 'x 'odd)
                                       ((6 8) 'y 'even)
                                       (else 'z 'nothing) )))
            'even )

(equal?-test "deterministic test: cond-desugar"
            (church-top-level-eval '((cond ((< 2 1) 'x 'a)
                                           ((< 1 1) 'y 'b)
                                           (else 'z 'c))))
            'c )

(equal?-test "deterministic test: cond-desugar"
            (church-top-level-eval '((cond ((< 2 1) 'x 'a)
                                           ((<= 1 1) 'y 'b)
                                           (else 'z 'c))))
            'b )

(equal?-test "deterministic test: map"
            (church-top-level-eval '((map (lambda (x) (list x x)) (list 1 2 3))))
            '((1 1) (2 2) (3 3)) )


;;=== stochastic test cases ====

(display "\nStochastic Tests...\n\n")

(test-mh samples samples-before-swap swaps runs '()
        '( '((a (/ 1 1000)))
           'a
           '(flip a) )
        (lambda (b) b)
        (/ 1 1000)
        0.0 ;;error-tolerance
        "setting a flip" )

(test-mh samples samples-before-swap swaps runs '()
        '( '((a (if (flip) 0 0.5))
             (b (sample-discrete (list a (- 1 a))))
             (c (if (and (= a 0)
                         (= b 0))
                    error ;; probability zero event
                    1)))
           '(+ b c)
           'true)
        (lambda (b) b)
        1.75
        error-tolerance
        "measure zero erp situation." )

(test-mh samples samples-before-swap swaps runs '()
        '( '((dist (if (flip) (list 1) (list 0.5 0.5))) 
             (b (sample-discrete dist)) )
           '(list-ref dist b)  ;; there is a measure zero overflow here 
           'true)
        (lambda (b) b)
        0.75
        error-tolerance
        "more insidious measure zero erp situation.")

(test-mh samples samples-before-swap swaps runs '()
        '( '()
           '(flip 0.7)
           'true)
        (lambda (x) (if x 1 0)) 
        0.7
        error-tolerance
        "unconditioned flip.")

(test-mh samples samples-before-swap swaps runs '()
        '( '((bit-flip (lambda (fidelity x) 
                         (flip (if x fidelity (- 1 fidelity)))))
             (hyp (flip 0.7)))
           'hyp
           '(bit-flip 0.8 hyp))
        (lambda (x) (if x 1 0)) 
        (/ (* 0.7 0.8) (+ (* 0.7 0.8) (* 0.3 0.2)))
        error-tolerance
        "conditioned flip." )

(test-mh samples samples-before-swap swaps runs '()
        '( '()
           '(if (flip 0.7) (flip 0.2) (flip 0.8))
           'true)
        (lambda (x) (if x 1 0)) 
        (+ (* 0.7 0.2) (* 0.3 0.8))
        error-tolerance
        "random 'if' with random branches, unconditioned." )

(test-mh samples samples-before-swap swaps runs '()
        '( '()
           '(flip (if (flip 0.7) 0.2 0.8))
           'true)
        (lambda (x) (if x 1 0)) 
        (+ (* 0.7 0.2) (* 0.3 0.8))
        error-tolerance
        "flip with random weight, unconditioned." )

(test-mh samples samples-before-swap swaps runs '()
        '( '((proc (if (flip 0.7) (lambda (x) (flip 0.2)) (lambda (x) (flip 0.8)))))
           '(proc 1)
           'true)
        (lambda (x) (if x 1 0)) 
        (+ (* 0.7 0.2) (* 0.3 0.8))
        error-tolerance
        "random procedure application, unconditioned." )

(test-mh samples samples-before-swap swaps runs '()
        '( '((hyp (multinomial (list 'b 'c 'd) (list 0.1 0.6 0.3)))
             (observe (lambda (x) (if (flip 0.8) x 'b)))) ;;this will be high variance since likelihood is not marginalized.
           '(eq? hyp 'b)
           '(eq? (observe hyp) 'b))
        (lambda (x) (if x 1 0)) 
        0.357
        error-tolerance
        "conditioned multinomial." )

(test-mh samples samples-before-swap swaps runs 
        '( (define (power-law prob x) (if (flip prob) x (power-law prob (+ x 1)))) )
        '( '((a (power-law 0.3 1)))
           '(< a 5)
           'true)
        (lambda (x) (if x 1 0)) 
        (apply + ((lambda (prob) (map (lambda (x) (* (expt (- 1 prob) (- x 1)) prob)) (list 1 2 3 4))) 0.3))
        error-tolerance
        "recursive stochastic fn using define, unconditioned." )

(test-mh samples samples-before-swap swaps runs '()
        '( '((proc (mem (lambda (x) (flip 0.8)))))
           '(and (and (and (proc 1) (proc 2)) (proc 1)) (proc 2))
           'true)
        (lambda (x) (if x 1 0)) 
        0.64
        error-tolerance
        "memoized flip, unconditioned." )

(test-mh samples samples-before-swap swaps runs '()
        '( '((a (flip 0.8))
             (proc (mem (lambda (x) a))))
           '(and (proc 1) (proc 1))
           'true)
        (lambda (x) (if x 1 0)) 
        0.8
        error-tolerance
        "bound symbol used inside memoizer, unconditioned." )

(test-mh samples samples-before-swap swaps runs '()
        '( '((proc (mem (lambda (x) (flip 0.8)))))
           '(and (proc (uniform-draw (list 1 2 3))) (proc (uniform-draw (list 1 2 3))))
           'true)
        (lambda (x) (if x 1 0)) 
        (+ (* (/ 1 3) 0.8) (* (/ 2 3) (* 0.8 0.8)))
        error-tolerance
        "memoized flip with random argument, unconditioned." )


(test-mh samples samples-before-swap swaps runs '()
        '( '((proc (if (flip 0.7) (lambda (x) (flip 0.2)) (lambda (x) (flip 0.8))))
             (memproc (mem proc)))
           '(and (memproc 1) (memproc 2))
           'true)
        (lambda (x) (if x 1 0)) 
        (+ (* 0.7 0.2 0.2) (* 0.3 0.8 0.8))
        error-tolerance
        "memoized random procedure, unconditioned." )

(test-mh samples samples-before-swap swaps runs  '()
        '( '((draw-type (DPmem 1.0 gensym))
             (class (mem (lambda (x) (draw-type)))))
           '(eq? (class 'bob) (class 'mary))
           'true)
        (lambda (x) (if x 1 0))
        0.5
        error-tolerance
        "DPmem of gensym, unconditioned." )

(test-mh samples samples-before-swap swaps runs 
        '( (define bit-flip (lambda (fidelity x) (flip (if x fidelity (- 1 fidelity))))) )
        '( '()
           '(rejection-query '(define a (flip 0.7)) 'a '(bit-flip 0.8 a) (get-current-environment))
           'true)
        (lambda (x) (if x 1 0)) 
        (/ (* 0.7 0.8) (+ (* 0.7 0.8) (* 0.3 0.2)))
        error-tolerance
        "mh-query over rejection query for conditioned flip." )

(test-mh samples samples-before-swap swaps runs 
        '( (define noisy-xor (lambda (a b n) (flip (if (xor a b) 1 n))) ) 
           (define noise 0))
        '( '((a (flip))
             (b (flip)))
           'a
           '(noisy-xor a b noise))
        (lambda (x) (if x 1 0))
        0.5
        error-tolerance
        "two flips correlated by conditioner (won't mix without tempering)." )

(test-mh samples samples-before-swap swaps runs '()
        '( '((a (if (flip 0.9) (beta 1 5) 0.7))
             (b (flip a)))
           'a
           'b)
        (lambda (x) x)
        0.417 ;approximated by 10000 rejection samples (in church, but not with mh...).
        error-tolerance
        "trans-dimensional." )

(test-mh samples samples-before-swap swaps runs '()
       '( '((a (if (flip) (mem flip) (mem flip)))
            (b (a)))
          'b
          'true)
       (lambda (x) (if x 1 0)) 
       0.5
       error-tolerance
       "memoized flip in if branch (create/destroy memprocs), unconditioned." )


;;;;;;;;;;;;;;
;; now see which tests failed

(for-each display
         (list "\n\n Total average time for test suite: " total-average-time "\n\n") )

(if (null? failed-tests)
   (display "All tests passed.\n")
   (begin (display "Some tests failed.\n")
          (pretty-print (reverse failed-tests)) ))



(exit)
