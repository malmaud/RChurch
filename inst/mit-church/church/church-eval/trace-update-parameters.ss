#!r6rs

;;written by NDG on Jun 17, 2008

(library (church church-eval trace-update-parameters)
         
         (export setup-parameters force-complete-eval? erps-to-propose params->constraints)
         
         (import (church utils rnrs)
                 (church constraint-propagation constraints)
                 (_srfi :1) ; lists
                 (church readable-scheme))
         
                  
         ;;parameters is a vector with entries: whether to keep subtraces, whether for force complete eval, erp to propose to (or null if none).
         (define (setup-parameters control-arguments) 
           (let ((params (vector false '() (list *wildcard-value*)))
                 (propose (list-index (lambda (f) (eq? 'propose f)) control-arguments))
                 (force-complete-eval (list-index (lambda (f) (eq? 'force-complete-eval f)) control-arguments))
                 (force-re-eval (list-index (lambda (f) (eq? 'force-re-eval f)) control-arguments))
                 (constraints (list-index (lambda (f) (eq? 'constraints f)) control-arguments)))
             (when propose (vector-set! params 1 (list-ref control-arguments (+ propose 1))))
             (when force-complete-eval (vector-set! params 0 (list-ref control-arguments (+ force-complete-eval 1))))
             (when constraints (vector-set! params 2 (list-ref control-arguments (+ constraints 1))))
             ;(when force-re-eval (vector-set! params 0 (list-ref params (+ force-re-eval 1))))
             params))
         (define (force-complete-eval? params) (vector-ref params 0))
         (define (erps-to-propose params) (vector-ref params 1))
         (define (params->constraints params) (vector-ref params 2))
         
 )