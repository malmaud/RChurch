#!r6rs

(library
 (church mcmc queries annealed-importance)

 (export ais trajectory-ais sais)

 (import (except (rnrs) vector-map vector-for-each vector-fill! vector->list list->vector string-hash string-ci-hash)
         (church readable-scheme)
         (church mcmc mcmc-core)
         (church mcmc queries tempered-transitions)
         (church church-eval church-eval)
         (church church-eval environments-lexical)
         (church church-eval logmath)
         (church church-eval syntax)
         (_srfi :1))

;;this is an annealed importance sampler.
;;each coupling parameter should range from 1 (fully uncoupled) to min-temp (0 is fully coupled).
;;returns (sample . importance weight).
;;temp-schedule should have form ((k . (p1 ...)) ...), where k is number of kernel applications, and pi are coupling parameter values.
;;  note: must start with all params = 1.0.
;;this should be registered with register-query!.
(define (ais coupling-params temp-schedule normal-form-query-expr env)
  (let*-values ([ (extended-env)  (extend-environment coupling-params (rest (first temp-schedule)) env)]
                [ (init-tc init-forw-prob init-backw-prob) (church-eval (sexpr->syntax normal-form-query-expr extended-env) extended-env)] ;;initialize from fully-uncoupled temps.
                [ (rejuv-kernel) (make-mh-kernel (make-trace-walk-proposal)  trace-score-proc "in-sais-trace-walk-kernel")]
                [ (final-tc forw-prob backw-prob) (TT-proposal init-tc extended-env coupling-params temp-schedule rejuv-kernel)])
    (pair (trace-finish final-tc) (- backw-prob forw-prob))))

;;same as above, but return a trajectory of samples, in case you care about intermediate states.
;;visitation-schedule is the number of steps from temp-schedule to do before taking a sample.
(define (trajectory-ais coupling-params temp-schedule visitation-schedule normal-form-query-expr env)
  (let*-values ([ (extended-env)  (extend-environment coupling-params (rest (first temp-schedule)) env)]
                [ (init-tc init-forw-prob init-backw-prob) (church-eval (sexpr->syntax normal-form-query-expr extended-env) extended-env)] ;;initialize from fully-uncoupled temps.
                [ (rejuv-kernel) (make-mh-kernel (make-trace-walk-proposal)  trace-score-proc "in-sais-trace-walk-kernel")])
    ;;apply TT-proposal, take a sample, then repeat until visitation schedule is up.
    (let loop ((tc init-tc)
               (fw LOG-PROB-1)
               (bw LOG-PROB-1)
               (visitation-schedule visitation-schedule)
               (temp-schedule temp-schedule)
               (samples '()))
      (if (null? visitation-schedule)
          (pair samples (- bw fw))
          (let*-values ([(next-temp-schedule) (take temp-schedule (first visitation-schedule))]
                        [(next-tc forw-prob backw-prob) (TT-proposal tc extended-env coupling-params next-temp-schedule rejuv-kernel)])
            (loop next-tc (+ fw forw-prob) (+ bw backw-prob)
                  (rest visitation-schedule)
                  (drop temp-schedule (first visitation-schedule))
                  (append samples (list (trace-finish next-tc)))  ))))))


;;this is a sequential annealed importance sampler.
;;each coupling parameter should range from 1 (fully uncoupled) to min-temp (0 is fully coupled).
;;will anneal in coupling parameters one at a time.
;;should be registered with register-query!.
(define (sais coupling-params steps-per-param kernels-per-step min-temp normal-form-query-expr env)
  (ais coupling-params (setup-temps coupling-params steps-per-param kernels-per-step min-temp) normal-form-query-expr env))

;;linear schedule on each param.
(define (setup-temps coupling-params steps-per-param kernels-per-step min-temp)
  (define (temps-for-param init-temps index steps stepsize kernels-per-step)
    (if (= steps 0)
        '()
        (let ((this-temps (append (take init-temps index)
                                  (list (- (list-ref init-temps index) stepsize))
                                  (drop init-temps (+ index 1)))))
          (pair (pair kernels-per-step this-temps)
                (temps-for-param this-temps index (- steps 1) stepsize kernels-per-step)))))
  (let loop ((ind 0)
             (temps (list (pair 0 (make-list (length coupling-params) 1.0))))) ;;0 kernel applications at starting (fully uncoupled) temps.
    (if (>= ind (length coupling-params))
        temps
        (let* ((stepsize (/ (- 1 min-temp) steps-per-param))
               (next-param-temps (temps-for-param (rest (last temps)) ind steps-per-param stepsize kernels-per-step)))
          (loop (+ ind 1) (append temps next-param-temps))))))

;;zeno's temperature schedule -- geometric schedule on each param.
(define (setup-temps-geometric coupling-params steps-per-param kernels-per-step min-temp)
  (define (temps-for-param init-temps index steps min-temp kernels-per-step)
    (if (= steps 1)
        (list (append (list kernels-per-step)
                (take init-temps index)
                (list min-temp)
                (drop init-temps (+ index 1))))
        (let ((this-temps (append (take init-temps index)
                                  (list (+ min-temp (* (- (list-ref init-temps index) min-temp) 0.7)))
                                  (drop init-temps (+ index 1)))))
          (pair (pair kernels-per-step this-temps)
                (temps-for-param this-temps index (- steps 1) min-temp kernels-per-step)))))
  (let loop ((ind 0)
             (temps (list (pair 0 (make-list (length coupling-params) 1.0))))) ;;0 kernel applications at starting (fully uncoupled) temps.
    (if (>= ind (length coupling-params))
        temps
        (loop (+ ind 1)
              (append temps
                      (temps-for-param (rest (last temps)) ind steps-per-param min-temp kernels-per-step))))))



 )