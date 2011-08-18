#!r6rs

(library 
 (church mcmc queries temperature-games)
 
 (export make-parallel-initializer 
         make-parallel-kernel
         keep-first-parallel-finish 
         make-parallel-scorer
         parallel-visitor
         make-kernel-name
         make-tempered-kernel
         tempered-repeated-mh-lex-query
         tempered-mh-lex-query
         PTquery)
 
 (import (church utils rnrs)
         (church readable-scheme)
         ;(church church-eval laziness)
         ;(church church-eval traces)
         (church mcmc mcmc-core)
         (church church-eval church-eval)
         (church church-eval environments-lexical)
         (_srfi :1))
 
  
 (define (tempered-repeated-mh-lex-query in-vars ladders N M L lex-exp query-exp cond-exp env)
   (let ((zipped-ladders (if (eq? (first in-vars) 'posterior-temperature) 
                             (apply zip ladders) 
                             (apply zip (cons (make-list (length (first ladders)) 1) ladders))))
         (vars (if (eq? (first in-vars) 'posterior-temperature) in-vars (cons 'posterior-temperature in-vars)))
         (normal-form-query-exp (convert-lex lex-exp query-exp cond-exp)))
     (let ((count -1)
           (samples '()))
       (mcmc-query-core (make-parallel-initializer vars zipped-ladders)
                        (make-tempered-kernel vars zipped-ladders M L)
                        (lambda (trace) 
                          (if (= count -1)
                              (begin (set! count 0) false)
                              (if (< count N)
                                  (begin (set! count (+ count 1))
                                         (set! samples (append samples (list (keep-first-parallel-finish trace))))
                                         false)
                                  true)))
                        (lambda (trace) samples)
                        normal-form-query-exp
                        (extend-environment vars (map (lambda (x) +nan.0) vars) env)) )))
 
 (define tempered-mh-lex-query (lambda (in-vars ladders M L lex-exp query-exp cond-exp env)
                                 (first (tempered-repeated-mh-lex-query in-vars ladders 1 M L lex-exp query-exp cond-exp env))))
 
 (define (parallel-finish traces)
   (map trace-finish traces))
 
 (define (keep-first-parallel-finish traces)
   (trace-finish (first traces)))
 
 (define (make-parallel-initializer vars zipped-ladders)
   (lambda (expr env)
     (map (lambda (temps) (default-initialize expr (extend-environment vars temps (enclosing-environment env)))) zipped-ladders) ))
 
 (define (make-kernel-name id attribs)
   (string-append id " " (write-to-string attribs)))
 
 (define (make-tempered-kernel vars zipped-ladders M L . visitor)
   (if (null? visitor)
       (repeat-kernel L (cycle-kernel (repeat-kernel M (make-parallel-kernel vars zipped-ladders))
                                      (make-swap-kernel vars zipped-ladders)))
       (repeat-kernel L (cycle-kernel (repeat-kernel M (cycle-kernel (parallel-visitor (first visitor)) (make-parallel-kernel vars zipped-ladders)))
                                      (make-swap-kernel vars zipped-ladders)))))
 
 (define (parallel-visitor visitor)
   (lambda (trace) (visitor (first trace)) trace))
 
;;  (define (make-parallel-kernel vars zipped-ladders env)
;;    (let ((kernels (map 
;;                    (lambda (temps)
;;                      (let ((kernel-name (make-kernel-name "parallel-kernel" (zip vars temps))))
;;                        (make-mh-kernel (make-trace-walk-proposal (extend-environment vars temps env)) 
;;                                        (make-score-at-temp (first temps)) kernel-name)))
;;                    zipped-ladders)))
;;      (lambda (states env) (map apply kernels (map list states)))))
 (define (make-parallel-kernel vars zipped-ladders)
   (let ((kernels (map 
                   (lambda (temps)
                     (let ((kernel-name (make-kernel-name "parallel-kernel" (zip vars temps))))
                       (make-mh-kernel (make-trace-walk-proposal) (make-score-at-temp (first temps)) kernel-name)))
                   zipped-ladders)))
     (lambda (states env) (map apply kernels
                               (map (lambda (s temps) (list s (extend-environment vars temps (enclosing-environment env)))) states zipped-ladders) ))))
 
 (define (make-score-at-temp temp)
   (lambda (state)
     (/ (trace-score-proc state) temp) ))
 
 (define (make-swap-kernel vars zipped-ladders)
   (let ((kernel-name (make-kernel-name "swap-kernel" (zip vars (apply zip zipped-ladders)))))
     (make-mh-kernel (make-swap-proposal vars zipped-ladders) (make-parallel-scorer zipped-ladders) kernel-name)))
 
 (define (make-parallel-scorer zipped-ladders)
   (let ((scorers (map make-score-at-temp (map first zipped-ladders))))
     (lambda (states) (apply + (map apply scorers (map list states))))))
 
 (define (make-swap-proposal vars zipped-ladders)
   (lambda (traces env) 
     (when (or (*mh-steps*) (*global-debug*)) (for-each display (list "Attempting swap...\n")))
     (let*-values (( (env) (enclosing-environment env))
                   [ (swap-at) (uniform-draw (range 1 (- (length traces) 1)))]
                   [ (bottom-trace) (list-elt traces swap-at)]
                   [ (top-trace) (list-elt traces (+ 1 swap-at))]
                   [ (bottom-temps) (list-elt zipped-ladders (+ swap-at 1))]
                   [ (top-temps) (list-elt zipped-ladders swap-at)]
                   [ (new-bottom-trace bottom-forw bottom-rev) (church-eval bottom-trace
                                                                            (extend-environment vars bottom-temps env) )]
                   [ (new-top-trace top-forw top-rev) (church-eval top-trace 
                                                                   (extend-environment vars top-temps env) )])
       (make-move (+ bottom-forw top-forw) (+ bottom-rev top-rev) 
                  (append (take traces (- swap-at 1)) 
                          (list new-top-trace new-bottom-trace) 
                          (drop traces (+ swap-at 1)))))))

;;;pure church PT:
 
(define (PTquery in-vars ladders M N lex-exp query-exp cond-exp env)
  (let* ((normal-form-query-expr (convert-lex lex-exp query-exp cond-exp))
         (PT-expr (PT-transform in-vars ladders normal-form-query-expr))
         (count -1)
         (samples '()))
    (mcmc-query-core default-initialize
                     (repeat-kernel N (make-mh-kernel (make-trace-walk-proposal) trace-score-proc "trace-walk-kernel (PTquery)"))
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
                     PT-expr
                     env)))

(define (PT-transform temp-vars temp-ladders normal-form-query-expr)
    (let* ((zipped-temps (apply zip temp-ladders))
           (chain-proc `(lambda ,temp-vars ,normal-form-query-expr)))
      `(let* ((temp-permutation (sample-permutation ,(length zipped-temps)))
              (returns (inverse-permute-list
                        (map (lambda (args) (apply ,chain-proc args))
                             (permute-list ',zipped-temps temp-permutation)) temp-permutation)))
                                        ;returns a list of pairs (val . constraint-val)
         (pair (first (first returns)) (apply and (map rest returns))))))

;; (define (PT-transform temp-vars temp-ladders normal-form-query-expr)
;;     (let* ((zipped-temps (apply zip temp-ladders))
;;            (chain-proc `(lambda ,temp-vars ,normal-form-query-expr)))
;;       `(let* ((temp-permutation (sample-permutation ,(length zipped-temps)))
;;               (permuted-temps (permute-list ',zipped-temps temp-permutation))
;;               (returns (inverse-permute-list
;;                         (list (apply ,chain-proc (first permuted-temps)) (apply ,chain-proc (second permuted-temps)))
;;                         temp-permutation)))
;;                                         ;returns a list of pairs (val . constraint-val)
;;          (pair (first (first returns)) (apply and (map rest returns))))))



 
 
 (when (> (*verbosity*) 12)
   (display "loaded temperature games\n"))
 
 )