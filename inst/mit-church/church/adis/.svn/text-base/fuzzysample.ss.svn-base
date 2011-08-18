#!r6rs

;; fuzzy sampler

(library

 (church adis fuzzysample)
 
 (export weighted-ints->ints
         fuzzysample)
 
 (import (church utils rnrs)
         (church readable-scheme)
         (church external math-env))

 
 (define int->value first)
 (define int->lower second)
 (define int->upper third)

 ;; fuzzy-sample : (list-of (cons prior-probs fuzzy-int)) -> sample
 ;; p: prior probabilities, can be a subprobability measure 
 ;;    (e.g., if you've only observed a subset).  remaining 
 ;;    mass (1 - sum(p)) is assumed to have an interval [0,1]
 ;; q: fuzzy intervals 

 (define FAILED -6)

 ;; takes a list of values, prior probabilities and intervals and
 ;; returns a list of triples (val pL pU) representing a fuzzy
 ;; distribution (i.e., it also calculates the missing probability 1 -
 ;; sum(prior) and assigns it complete uncertainty
 (define (weighted-ints->ints vals ps qs)
   (map (lambda (val p q) (list val (* p (first q)) (* p (rest q))))
        (cons FAILED vals)
        (cons (- 1 (apply + ps)) ps)
        (cons (cons 0 1) qs)
        ) )

 ;; (val pL pU) -> v or FAILED  w.p. v ~ [pL, pU]
 (define (fuzzysample ints)
  (let loop ((ints ints)
             [ZL (sum (map int->lower ints))]
             [ZU (sum (map int->upper ints))])
    (let* ([u (random-real)]
           [pL (int->lower (first ints))]
           [pU (int->upper (first ints))] )
      (cond [(< u (/ pL (+ pL (- ZU pU)))) ;; less than lower bound
             (int->value (first ints)) ]
            [(< u (/ pU (+ pU (- ZL pL)))) ;; in no-go-zone, return fail
             FAILED ] 
            [else (loop (rest ints) 
                        (- ZL pL)
                        (- ZU pU) )] ) ) ) )

 ;; ;;(define f (lambda () (fuzzysample '((0 .5 .5)
 ;; ;;                                    (1 .5 .5)))))
 ;;                                        ;
 ;; ;;(define f (lambda () (fuzzysample '((0 .25 .75)
 ;; ;;                                    (1 .5 .5))
 ;; ;;                                  )))

 ;; (define f (lambda () (fuzzysample (weighted-ints->ints '(0 1 -nan.0) ;; values
 ;;                                                   '(.1 .1 .1)  ;; priors (i.e. we haven't account for .7 of the mass)
 ;;                                                   '((.25 . .75) ;; bounds
 ;;                                                     (1 . 1)
 ;;                                                     (0 . 0) ) ))))
 ;; ;; we should never see 2
 ;; ;; otherwise, expectation should be ~.74

 ;; (define res (repeat 10000 f))

 ;; (define fres (filter (lambda (x) (not (equal? x FAILED))) res))

 ;; (for-each (lambda (x)
 ;;             (begin
 ;;               (display x)
 ;;               (display "\n")))
 ;;           (list
 ;;            (length fres)
 ;;            (apply + fres)
 ;;            (* 1.0 (/ (apply + fres) (length fres)))))

 )