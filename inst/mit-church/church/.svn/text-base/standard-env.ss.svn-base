#!r6rs

;; created Jun 23, 2008 by NDG
;; authors: noah goodman, daniel roy

(library (church standard-env)

         (export register-primitive-procedure!
                 register-primitive-erp!
                 marg
                 register-primitive-constant!
                 register-query!
                 setup-environment
                 identity)

         (import (church utils rnrs)
                 (_srfi :1)
                 (_srfi :43)
                 (church mcmc mcmc-core)
                 (church mcmc queries base-queries)
                 (church mcmc queries mh-query)
                 (church mcmc queries temperature-games)
                 (church mcmc queries gradient-mh-query)
                 (church mcmc queries tempered-transitions)
                 ;; (church mcmc queries emc-games)
                 ;; (church mcmc queries mtm-query)
                 
                 (church readable-scheme)
                 (church constraint-propagation constraints)
                 (church church-eval environments-lexical)
                 (church church-eval logmath)
                 (church church-eval traces)
                 (church church-eval trace-eval) ;should refactor to remove this dependency
                 (church church-eval trace-update) ;should refactor to remove this dependency
                 (church constraint-propagation primitive-inverses)
                 (church constraint-propagation trace-constraint-propagation)
                 (church utils utils)
                 (church external math-env)
                 (church utils levenshtein)

                 (church xrp-lib beta-binomial)
                 (church xrp-lib CRP)
                 (church xrp-lib dirichlet-discrete)
                 (church xrp-lib ms-dirichlet-discrete)
                 (church xrp-lib normal-normal-gamma)
                 (church xrp-lib gensym-xrp)
                 (church register-primitives)
                 )


         (define (setup-environment)
           (extend-environment (all-primitive-names)
                               (all-primitive-objects)
                               the-empty-environment
                               (make-list (length (all-primitive-names)) false))) ;;by setting diff to false we will effectively treat this top frame as immutable.

         (define identity (lambda (x) x))

         ;;this file contains code to setup the standard environment.
         ;;anything which is 'standard' but not a primitive (call to underlying scheme) should be added instead in standard-preamble

         ;; FLIP ;;;;;;;;;;;;

         (define (flip-score args val)
           (if (null? args)
               (- (log 2.0))
               (let ((p (first args))) ;;need to make innexact?
                 (if val
                     (log p)
                     (log (- 1 p)) ))))

         (define flip-in-trace (marg (list 'primitive flip)
                                     (list 'primitive flip-score) ))

         (define log-flip-in-trace (marg (list 'primitive log-flip)
                                         (list 'primitive (lambda (args val)
                                                            (if (null? args)
                                                                (- (log 2.0))
                                                                (let ((log-p (first args)))
                                                                  (if val
                                                                      (* 1.0 log-p) ;;FIXME: this is to cast to inexact... is that even %$##$@ necesary?
                                                                      (log (- 1.0 (exp log-p))) )))))))

         (define flip-in-trace-w-proposer
           (marg (list 'primitive flip)
                 (list 'primitive flip-score)
                 ;;non-standard proposer (just propose other value of the flip):
                 (list 'primitive
                       (lambda (operator operands old-value)
                         (values (not old-value)
                                 LOG-PROB-1 LOG-PROB-1) )) ;;this proposal is deterministic (given current value) so forw and rev probs are 1.
                 ))

         ;;integer (random integer from 0 to n-1)
         (define sample-integer-in-trace (marg (list 'primitive random-integer)
                                               (list 'primitive (lambda (args val)
                                                                  (let ((n (first args)))
                                                                    (if (and (integer? val)
                                                                             (>= val 0)
                                                                             (< val n))
                                                                        (- (log n))
                                                                        LOG-PROB-0 ))))
                                               ))

         ;;;permutations
         (define sample-permutation-in-trace (marg (list 'primitive (lambda (len)
                                                                      ;; implements fisher-yates-knuth
                                                                      (define vec (list->vector (iota len)))
                                                                      (let loop ((n (- len 1)))
                                                                        (if (= n 0)
                                                                            (vector->list vec)
                                                                            (let* ((k    (random-integer (+ n 1)))
                                                                                   (temp (vector-ref vec k)))
                                                                              (vector-set! vec k (vector-ref vec n))
                                                                              (vector-set! vec n temp)
                                                                              (loop (- n 1)) )))))
                                                   (list 'primitive (lambda (args val)
                                                                      (let ((len (first args)))
                                                                        (if (= len (length val)) ;; only a weak sanity check
                                                                            (- (lnfact len))
                                                                            LOG-PROB-0 ))))))

         (define (permute-list lst index-perm)
           (map (lambda (ind) (list-ref lst ind)) index-perm))
         (define (inverse-permute-list lst index-perm)
           (let ((inverse-perm (map (lambda (place) (list-index (lambda (x) (= x place)) index-perm)) (iota (length index-perm)))))
             (permute-list lst inverse-perm)))
         ;;take a list of values with wildcard cdr, and extend it to the required length by padding with *wildcard-value*s.
         (define (augment-value-list lst lnth)
           (if (eq? lst *wildcard-value*)
               (make-list lnth *wildcard-value*)
               (pair (first lst) (augment-value-list (rest lst) (- lnth 1)))))

         ;; (droy) useful helper! we should use it
         (define (add-proposal erp proposal)
           (marg (erp-sampler erp)
                 (erp-scorer erp)
                 proposal))

         ;; FIXME
         (define sample-permutation-with-swap-proposal
           (add-proposal sample-permutation-in-trace
                         (list 'primitive
                               (lambda (operator operands old-value)
                                 (let* ((vec (list->vector old-value))
                                        (len (second operands)))
                                   (let* ((index< (random-integer len))
                                          (index> (modulo (+ 1 index< (random-integer (- len 1)))
                                                          len ))
                                          (temp (vector-ref vec index<)) )
                                     (vector-set! vec index< (vector-ref vec index>))
                                     (vector-set! vec index> temp)
                                     (let ((newval (vector->list vec)))
                                       (values newval LOG-PROB-1 LOG-PROB-1) ;; symmetric
                                       )))))))
         (define sample-permutation-with-adjacent-swap-proposal
           (add-proposal sample-permutation-in-trace
                         (list 'primitive
                               (lambda (operator operands old-value)
                                 (let* ((vec (list->vector old-value))
                                        (len (first operands)))
                                   (let* ((index< (random-integer (- len 1)))
                                          (index> (+ 1 index<))
                                          (temp (vector-ref vec index<)) )
                                     (vector-set! vec index< (vector-ref vec index>))
                                     (vector-set! vec index> temp)
                                     (let ((newval (vector->list vec)))
                                       (values newval LOG-PROB-1 LOG-PROB-1) ;; symmetric
                                       )))))))

                                        ;(define sample-permutation-erp sample-permutation-with-swap-proposal)
         (define sample-index-permutation-erp sample-permutation-with-adjacent-swap-proposal);sample-permutation-in-trace)

         ;;discrete
         (define sample-discrete-in-trace (marg (list 'primitive sample-discrete)
                                                (list 'primitive (lambda (args val)
                                                                   (if (>= val (length (first args)))
                                                                       LOG-PROB-0
                                                                       (let ((p (discrete-pdf (first args) val)))
                                                                         (if (> p 0) (log p) LOG-PROB-0)))))))

         ;;Poisson Distribution
         (define poisson-erp-with-default-proposal
           (marg (list 'primitive sample-poisson)
                 (list 'primitive (lambda (args val) (log (poisson-pdf val (first args)))))))
         (define poisson-erp poisson-erp-with-default-proposal)


         ;;Binomial Distribution

         (define (binomial-pdf-avoid-underflow val p n)
           (let ((result (scheme-binomial-lnpdf val p n)))
             (if (= result LOG-PROB-0)
                 (log (inexact->exact a-very-small-number))
                 result)))

         (define (scheme-binomial-lnpdf val p n)
           (when (> val n) (error 'scheme-binomial-pdf "You cannot have more successes than trials!"))
           (cond ((= p 0.0) (begin (display "P is zero!\n") LOG-PROB-0))
                 ((= p 1.0) (if (= val n) LOG-PROB-1 LOG-PROB-0))
                 (else (let ([log-p (inexact->exact (log (inexact->exact p)))]
                             [log-not-p (inexact->exact (log (- 1 (inexact->exact p))))])
                         (+ (* (inexact->exact val) log-p) (* (- (inexact->exact n) (inexact->exact val)) log-not-p))))))
         
         
         (define binomial-erp-with-default-proposal
           (marg (list 'primitive sample-binomial)
                 (list 'primitive (lambda (args val) (binomial-pdf-avoid-underflow val (first args) (second args))))))
         (define binomial-erp binomial-erp-with-default-proposal)
         
         ;;;;Continuous distribution ERPs.
         ;;gonna put drift proposals on these guys:

         (define (add-drift-proposal step-var erp)
           (marg (erp-sampler erp) (erp-scorer erp)
                 (list 'primitive
                       (lambda (operator operands old-value)
                         (let* ((step (sample-gaussian 0.0 step-var))
                                (newval (+ old-value step)))
                           (values newval LOG-PROB-1 LOG-PROB-1))) ;;this proposal is symmetric, so don't bother computing forw/backw probs.
                       )))

         ;; a drift proposal where a userspace church program can
         ;; specify the step size parameter
         (define (add-drift-proposal-sv erp)
           (marg (erp-sampler erp) (erp-scorer erp)
                 (list 'primitive
                       (lambda (operator operands old-value)
                         (let* ((step (sample-gaussian 0.0 (third operands)))
                                (newval (+ old-value step)))
                           (values newval LOG-PROB-1 LOG-PROB-1))) ;;this proposal is symmetric, so don't bother computing forw/backw probs.
                       )))

         (define (add-positive-drift-proposal step-var erp)
           (marg (erp-sampler erp) (erp-scorer erp)
                 (list 'primitive
                       (lambda (operator operands old-value)
                         (let* ((step (sample-gaussian 0.0 step-var))
                                (newval (exp (+ (log old-value) step))) )
                           (values newval LOG-PROB-1 LOG-PROB-1))) ;;this proposal is symmetric, so don't bother computing forw/backw probs.
                       )))

         ;;FIXME: finish this. use for uniform.
         (define (add-bounded-drift-proposal step-var lower-bound upper-bound erp)
           (marg (erp-sampler erp) (erp-scorer erp)
                 (list 'primitive
                       (lambda (operator operands old-value)
                         (let* ((step (sample-gaussian 0.0 step-var))
                                (newval (exp (+ (log old-value) step))) )
                           (values newval LOG-PROB-1 LOG-PROB-1))) ;;this proposal is symmetric, so don't bother computing forw/backw probs.
                       )))

         ;; uniform[a,b] distribution
         (define uniform-erp-with-default-proposal
           (marg (list 'primitive (lambda (a b) (+ (* (- b a) (random-real)) a)))
                 (list 'primitive (lambda (args val)
                                    (let ((a (first args))
                                          (b (second args)) )
                                      (if (or (< val a)
                                              (> val b))
                                          LOG-PROB-0
                                          (- (log (- b a))) ))))))
         (define uniform-erp uniform-erp-with-default-proposal)

         ;;Gamma Distribution (score defined in terms of AD friendly lngamma).
         (define gamma-erp-with-default-proposal
           (marg (list 'primitive sample-gamma)
                 (list 'primitive (lambda (args value) (gamma-lnpdf value (first args) (second args))))))
         (define gamma-erp
           (add-positive-drift-proposal 0.5 gamma-erp-with-default-proposal))

         ;;Dirichlet Distribution
         (define a-very-small-number 0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001)
         (define (dirichlet-eps pseudos) (let ((draw (sample-dirichlet pseudos)))
                                           (if (memv 0.0 draw)
                                               (let ((norm (+ (* (length draw) a-very-small-number) (apply + draw))))
                                                 (map (lambda (x) (/ (+ x a-very-small-number) norm)) draw))
                                               draw)))
         (define (dirichlet-exclusive pseudos) (let ((draw (sample-dirichlet pseudos)))
                                                 (if (memv 0.0 draw)
                                                     (dirichlet-exclusive pseudos)
                                                     draw)))
         (define dirichlet-sampler dirichlet-eps)
         (define dirichlet-erp-with-default-proposal
           (marg (list 'primitive dirichlet-sampler)
                 (list 'primitive (lambda (args val)
                                    (dirichlet-lnpdf val (first args))))))
         ;;a nice proposal for dirichlet is dirichlet with weights centered on current draw:
         (define dirichlet-erp
           (marg (erp-sampler dirichlet-erp-with-default-proposal) (erp-scorer dirichlet-erp-with-default-proposal)
                 (list 'primitive
                       (lambda (operator operands old-value)
                         (let* ((params (first operands))
                                (scale (make-list (length old-value) (apply + params)))
                                (proposal-params (map + params (map * scale old-value))) ;;should divide by 2? (ie. do i want the proposal scale same as prior scale?)
                                (new-value (dirichlet-sampler proposal-params))
                                (reverse-proposal-params (map + params (map * scale new-value))))
                           (values new-value
                                   (dirichlet-lnpdf new-value proposal-params)
                                   (dirichlet-lnpdf old-value reverse-proposal-params)
                                   )))
                       )))

                                        ;Gaussian Distribution
         (define gaussian-erp-with-default-proposal
           (marg (list 'primitive (lambda args (sample-gaussian (first args) (second args))) )
                 (list 'primitive (lambda (args val) (gaussian-lnpdf val (first args) (second args))))))
         (define gaussian-erp
           (add-drift-proposal 0.05 gaussian-erp-with-default-proposal))
                                        ;           (add-drift-proposal 1.0 gaussian-erp-with-default-proposal))

         (define gaussian-sv-erp
           (add-drift-proposal-sv gaussian-erp-with-default-proposal))

         ;;Exponential
         (define sample-exponential-erp-with-default-proposal
           (marg (list 'primitive (lambda (inv-mean)
                                    (- (/ (log (random-real)) inv-mean)) ))
                 (list 'primitive (lambda (args val) ;; returns density
                                    (if (< val 0)
                                        -inf.0
                                        (let ((inv-mean (exact->inexact (first args))))
                                          (+ (log inv-mean) (- (* inv-mean val))) ))))))
         (define sample-exponential-erp
           (add-positive-drift-proposal 0.5 sample-exponential-erp-with-default-proposal))

         ;; true multinomial -- sample from a categorical num-trials times
         (define sample-mmultinomial-in-trace
           (marg (list 'primitive sample-mmultinomial) ; pvals num-trials
                 (list 'primitive (lambda (args val)
                                    (mmultinomial-lnpdf val (first args) ) ))
                 (list 'primitive ; this proposer just swaps two samples.  NOTE: can propose probability 0 values!
                       (lambda (operator operands old-value)
                         (let ((vcnts (list->vector old-value))
                               (ind1 (random-integer (length (first operands))))
                               (ind2 (random-integer (length (first operands)))) )
                           (vector-set! vcnts ind1 (- (vector-ref vcnts ind1) 1 ) )
                           (vector-set! vcnts ind2 (+ (vector-ref vcnts ind2) 1 ) )
                           (let ((new_list (vector->list vcnts)))
                             (values new_list LOG-PROB-1 LOG-PROB-1 ) )) )) ) )


         (define (set-debug-mode! flag val)
           (case flag
             ('global (*global-debug* val))
             ('trace-eval (*trace-eval-debug* val))
             ('trace-update (*trace-update-debug* val))
             ('init (*constraint-debug* val))
             ('mh-steps (*mh-steps* val))
             ('mh-statistics (*mh-statistics* val))
             ('verbosity (*verbosity* val))
             ('pretty-pictures (*pretty-pictures* val))
             ('safe-mode (*safe-mode* val))
             ('show-mh-syntax (*show-syntax* val))
             (else (error "unkown debug flag!"))))

         ;; @desc
         ;; Set the debug mode.
         ;; @param flag Which mode to set. Must be one of 'global 'trace-eval 'importance-eval 'trace-update 'init 'mh-steps 'mh-statistics 'verbosity 'pretty-pictures 'safe-mode.
         ;; @param val true or false
         ;; @return void
         (register-primitive-procedure! 'debug-mode set-debug-mode!)

         (register-primitive-constant! 'true true)
         (register-primitive-constant! 'false false)

         ;;list stuff is safe to pass through because trace-eval and the underlying scheme use the same representation for lists.

         ;; @desc
         ;; Get the first element of a list.
         ;; @param Lst The list.
         ;; @return First element of the list.
         (register-primitive-procedure! 'first car first-inverse)
         ;; @desc
         ;; Get the tail of a list (all but the first element).
         ;; @param Lst The list.
         ;; @return Tail of list.
         (register-primitive-procedure! 'rest cdr rest-inverse)
         ;; @desc
         ;; Build a pair.
         ;; @param A The first element.
         ;; @param B The second element.
         ;; @return A pair.
         (register-primitive-procedure! 'pair cons pair-inverse)
         ;; @desc
         ;; Build a list.
         ;; @param A ... Things to make into a list.
         ;; @return A list.
         (register-primitive-procedure! 'list list list-inverse)
         ;; @desc
         ;; Get an element from a list. (This is zero-indexed: 0 is the first element.)
         ;; @param lst The list.
         ;; @param index The index of the element to get (starting with 0).
         ;; @return An element.
         (register-primitive-procedure! 'list-ref list-ref list-ref-inverse)
         ;; @desc
         ;; Get an element from a list. (This is one-indexed: 1 is the first element.)
         ;; @param lst The list.
         ;; @param index The index of the element to get (starting with 1).
         ;; @return An element.
         (register-primitive-procedure! 'list-elt (lambda (l k) (list-ref l (- k 1))) list-elt-inverse) ;this is a one-indexed version of list-ref.
         ;; @desc
         ;; Get the length of a list
         ;; @param lst The list.
         ;; @return The length (and integer).
         (register-primitive-procedure! 'length length)
         ;; @desc
         ;; Append some lists together.
         ;; @param lst ... The lists.
         ;; @return A list.
         (register-primitive-procedure! 'append append append-inverse)
         ;; @desc
         ;; Void.
         ;; @return void
         (register-primitive-procedure! 'void void)
         ;; @desc
         ;; Check if a list is null. Returns true when the list is equal to '().
         ;; @param Lst The list.
         ;; @return boolean
         (register-primitive-procedure! 'null? null? null?-inverse)
         ;; @desc
         ;; Check if something is a list in the underlying Scheme.
         ;; @param Lst The list.
         ;; @return boolean
         (register-primitive-procedure! 'scheme-list? list?)
         ;; @desc
         ;; Build a list with a sequence of integers.
         ;; @param size The length of the resulting list.
         ;; @param start The starting value of the list. (This param is optional.)
         ;; @param step The spacing between elements of the list. (This param is optional.)
         ;; @return A list.
         (register-primitive-procedure! 'iota iota) ;;like range...
         ;; @desc
         ;; Make a list filled with some value.
         ;; @param lngth The length of the list.
         ;; @param fill The fill value.
         ;; @return A list.
         (register-primitive-procedure! 'make-list make-list)
         ;; @desc
         ;; Build a list with a range of integers.
         ;; @param from Starting value.
         ;; @param to Ending value.
         ;; @return A list (includes from and to values).
         (register-primitive-procedure! 'range range)
         ;; @desc
         ;; Get the last element from a list.
         ;; @param lst The list.
         ;; @return An element.
         (register-primitive-procedure! 'last last)

         ;;undocumented because it shouldn't be used.
         (register-primitive-procedure! 'error error)

         ;; @desc
         ;; Check if two things are the exact same object in memory.
         ;; Note that (eq? (list 'a) (list 'a)) is false.
         ;; @param a
         ;; @param b
         ;; @return boolean
         (register-primitive-procedure! 'eq? eq? equal?-inverse)
         ;; @desc
         ;; Check if two things are equivalent. (Recursive on lists, vectors, etc.)
         ;; @param a
         ;; @param b
         ;; @return boolean
         (register-primitive-procedure! 'equal? equal? equal?-inverse)


         ;; @desc
         ;; Build a vector.
         ;; @param val ... The entries of the vector.
         ;; @return vector
         (register-primitive-procedure! 'vector vector)
         ;; @desc
         ;; Turn a list into a vector.
         ;; @param lst The list.
         ;; @return vector
         (register-primitive-procedure! 'list->vector list->vector)
         ;; @desc
         ;; Turn a vector into a list.
         ;; @param vect The vector.
         ;; @return list
         (register-primitive-procedure! 'vector->list vector->list)
         ;; @desc
         ;; Get an element from a vector. (Zero indexed.)
         ;; @param vect The vector.
         ;; @param ind The index.
         ;; @return An element.
         (register-primitive-procedure! 'vector-ref vector-ref)
         
         (register-primitive-procedure! 'display-list (lambda args (for-each display args)))
         ;; @desc
         ;; Pretty prints.
         ;; @param val Something to make pretty then print to screen.
         ;; @return void
         (register-primitive-procedure! 'pretty-print pretty-print)
         ;; @desc
         ;; Get the command line arguments that the host scheme was called with.
         ;; @return arguments
         (register-primitive-procedure! 'command-line-arguments command-line-arguments)

         ;; @desc
         ;; Get a new symbol. (Note that probabilities are only defined up to identity of gensym.)
         ;; @return symbol
         (register-primitive-procedure! 'gensym gensym)
         ;; @desc
         ;; Take a list of arbitrary objects, and return a list of integers, where each object has been assigned an integer in canonical order.
         ;; Useful for converting lists of gensyms into something pleasant to look at.
         ;; @param lst The list.
         ;; @return list
         (register-primitive-procedure! 'canonicalize-symbol-list canonicalize-symbol-list)

         ;; @desc
         ;; And.
         ;; @param val ... Each argument is a boolean.
         ;; @return boolean
         (register-primitive-procedure! 'and (lambda l (every identity l)) and-inverse)
         ;; @desc
         ;; Or.
         ;; @param val ... Each argument is a boolean.
         ;; @return boolean
         (register-primitive-procedure! 'or (lambda l (any identity l)) or-inverse)
         ;; @desc
         ;; Xor.
         ;; @param val ... Each argument is a boolean.
         ;; @return boolean
         (register-primitive-procedure! 'xor (lambda (x y) (not (eq? x y))) xor-inverse)
         ;; @desc
         ;; Not. Returns true if the argument is not false.
         ;; @param val
         ;; @return boolean
         (register-primitive-procedure! 'not not not-inverse)

         ;; @desc
         ;; Check if the argument is a number.
         ;; @param val
         ;; @return boolean
         (register-primitive-procedure! 'number? number?)
         ;; @desc
         ;; Plus
         ;; @param val ... A bunch of numbers.
         ;; @return number
         (register-primitive-procedure! '+ + plus-inverse)
         
         ;; @desc
         ;; Minus
         ;; @param val ... A bunch of numbers.
         ;; @return number
         (register-primitive-procedure! '- -)
         
         ;; @desc
         ;; Equals
         ;; @param val ... A bunch of numbers.
         ;; @return number
         (register-primitive-procedure! '= = equal?-inverse)
         
         ;; @desc
         ;; Multiply.
         ;; @param val ... A bunch of numbers.
         ;; @return number
         (register-primitive-procedure! '* *)
         
         ;; @desc
         ;; Divide
         ;; @param val ... A bunch of numbers.
         ;; @return number
         (register-primitive-procedure! '/ /)
         
         ;; @desc
         ;; Less than
         ;; @param val ... A bunch of numbers.
         ;; @return boolean
         (register-primitive-procedure! '< <)
         
         ;; @desc
         ;; Greater than
         ;; @param val ... A bunch of numbers.
         ;; @return boolean
         (register-primitive-procedure! '> >)
         
         ;; @desc
         ;; Less than or equal to
         ;; @param val ... A bunch of numbers.
         ;; @return boolean
         (register-primitive-procedure! '<= <=)
         
         ;; @desc
         ;; Greater than or equal to
         ;; @param val ... A bunch of numbers.
         ;; @return boolean
         (register-primitive-procedure! '>= >=)
         
         ;; @desc
         ;; Exponential
         ;; @param val A number
         ;; @return number
         (register-primitive-procedure! 'exp exp)
         
         ;; @desc
         ;; Log (base e)
         ;; @param val A number
         ;; @return number
         (register-primitive-procedure! 'log log)
         
         ;; @desc
         ;; Raise a number to a power (val1 ^ val2)
         ;; @param val1 A number
         ;; @param val2 A number
         ;; @return boolean
         (register-primitive-procedure! 'expt expt)

         (register-primitive-procedure! 'sqrt sqrt)
         
         ;; @desc
         ;; Logistic function 
         ;; @param val A number
         ;; @return number
         (register-primitive-procedure! 'logistic logistic)
         
         ;; @desc
         ;; Returns x mod y
         ;; @param x A number
         ;; @param y A number
         ;; @return number
         (register-primitive-procedure! 'modulo modulo)
         
         ;; @desc
         ;; Returns the min of a list of numbers
         ;; @param val A bunch of numbers.
         ;; @return number
         (register-primitive-procedure! 'min min)
         
         ;; @desc
         ;; Returns the max of a list of numbers
         ;; @param val A bunch of numbers.
         ;; @return number
         (register-primitive-procedure! 'max max)
         
         ;; @desc
         ;; Absolute value function 
         ;; @param val A number
         ;; @return number
         (register-primitive-procedure! 'abs abs)
         
         ;; @desc
         ;; Convert exact (rational) numbers to inexact numbers (floating point)
         ;; @param val A number
         ;; @return number
         (register-primitive-procedure! 'exact->inexact exact->inexact)
         
         ;; @desc
         ;; Convert inexact (floating point) numbers to exact numbers (rationals)
         ;; @param val A number
         ;; @return number
         (register-primitive-procedure! 'inexact->exact inexact->exact)

         ;; @desc
         ;; Compute the sine of a number
         ;; @param val A number
         ;; @return number
         (register-primitive-procedure! 'sin sin)

         ;; @desc
         ;; Compute the cosine of a number
         ;; @param val A number
         ;; @return number
         (register-primitive-procedure! 'cos cos)
         
         ;; @desc
         ;; Compute the tangent of a number
         ;; @param val A number
         ;; @return number
         (register-primitive-procedure! 'tan tan)

         
         ;; @desc
         ;; Test if a character is lower case
         ;; @param char A single character
         ;; @return boolean
         (register-primitive-procedure! 'char-lower-case? char-lower-case?)
         
         ;; @desc
         ;; Return the n'th character of string (0 indexed)
         ;; @param string A string
         ;; @param n A number
         ;; @return character
         (register-primitive-procedure! 'string-ref string-ref)
         
         ;; @desc
         ;; Concatenates characters into a string
         ;; @param chars ... A bunch of characters
         ;; @return string
         (register-primitive-procedure! 'string string)
         
         ;; @desc
         ;; Concatenates strings together
         ;; @param strings ... A bunch of strings
         ;; @return string
         (register-primitive-procedure! 'string-append string-append)
         
         ;; @desc
         ;; Converts a symbol to a string.
         ;; @param s A symbol
         ;; @return string
         (register-primitive-procedure! 'symbol->string symbol->string)
         
         ;; @desc
         ;; Converts a string to a number. Radix must be either 2, 8, 10, or 16. If the radix is omitted
         ;; the default is 10.
         ;; @param s A string
         ;; @param radix A number
         ;; @return number
         (register-primitive-procedure! 'string->number string->number)
         
         ;; @desc
         ;; Converts a number to a string. Radix must be either 2, 8, 10, or 16. If the radix is omitted
         ;; it defaults to 10
         ;; @param n A number
         ;; @param radix a number
         ;; @return string
         (register-primitive-procedure! 'number->string number->string)
         
         ;; @desc
         ;; TODO: PLEASE ADD DOCUMENTATION
         ;; @return number
         (register-primitive-procedure! 'format format)
         
         
         
         ;; @desc
         ;; Displays a value (including numbers, strings, lists, etc).
         ;; Note: this is mostly for debugging, not for modeling.
         ;; @param v The value to display
         ;; @return void
         (register-primitive-procedure! 'display display)
         
         ;; @desc
         ;; Returns true if x is a symbol
         ;; @param x 
         ;; @return boolean
         (register-primitive-procedure! 'symbol? symbol?)
         
         ;; @desc
         ;; Returns true if x is a pair (or list)
         ;; @param x 
         ;; @return boolean
         (register-primitive-procedure! 'pair? pair?)
         
         ;; @desc
         ;; TODO: PLEASE ADD DOCUMENTATION
         ;; @return number
         (register-primitive-procedure! 'take take)
         
         ;; @desc
         ;; TODO: PLEASE ADD DOCUMENTATION
         ;; @return number
         (register-primitive-procedure! 'drop drop)
                                        ;(list 'cons cons) ;;these names are rediculous relics.. feel free to define them in your own standard preamble.
                                        ;(list 'car car)
                                        ;(list 'cdr cdr)

         ;;these are inference directives:
         (register-primitive-procedure! 'no-proposals identity identity-inverse)
                                        ;(list 'crossover-here identity identity-inverse)

         ;;these are hooks to church guts (must have loaded trace-eval.ss before getting here):
         (register-primitive-procedure! 'marg marg)
         (register-primitive-procedure! 'get-erp-score-fn erp-scorer)
         (register-primitive-procedure! 'procedure? valid-operator?)

         ;; @desc
         ;; Flip a fair or weighted coin.
         ;; @param weight If given, the coin is biased with this weight.
         ;; @return true or false
         (register-primitive-erp! 'flip flip-in-trace-w-proposer)

         (register-primitive-erp! 'flip-default-proposer flip-in-trace)
         
         ;; @desc
         ;; Flip a fair or weighted coin with weights from log space.
         ;; @param weight If given, the coin is biased with (exp weight).
         ;; @return true or false
         (register-primitive-erp! 'log-flip log-flip-in-trace)

         ;; @desc
         ;; Return a random integer between 0 and n-1.
         ;; @param n the number of values possible
         ;; @return an integer between 0 and n-1
         (register-primitive-erp! 'sample-integer sample-integer-in-trace)

         ;; @desc
         ;; Takes a list of weights and samples an index between
         ;; 0 and (number of weights - 1) with probability proportional
         ;; to the weights.
         ;; @param weights a list of weights
         ;; @return an integer between 0 and (length weights)-1
         (register-primitive-erp! 'sample-discrete sample-discrete-in-trace)
         
         (register-primitive-erp! 'sample-mmultinomial sample-mmultinomial-in-trace)
         
         ;; @desc
         ;; Draw a uniform random number from the range [a, b].
         ;; @param a lower bound
         ;; @param b upper bound
         ;; @return number
         (register-primitive-erp! 'uniform uniform-erp)
         
         ;; @desc
         ;; An ERP for gamma distributions
         ;; @return number
         (register-primitive-erp! 'gamma gamma-erp)
         
         ;; @desc
         ;; A random ERP for dirichlet distributions
         ;; @return number
         (register-primitive-erp! 'dirichlet dirichlet-erp)
         
         ;; @desc
         ;; A random ERP for Gaussian distributions
         ;; @return number
         (register-primitive-erp! 'gaussian gaussian-erp)
         
         (register-primitive-erp! 'gaussian-sv gaussian-sv-erp)
         
         ;; @desc
         ;; A random ERP for Poisson distributions
         ;; @return number
         (register-primitive-erp! 'poisson poisson-erp)

         ;; @desc
         ;; A random ERP for binomial distributions
         ;; @return number
         (register-primitive-erp! 'binomial binomial-erp)
         
         ;; @desc
         ;; A random ERP for exponential distributions
         ;; @return number
         (register-primitive-erp! 'exponential sample-exponential-erp)
         (register-primitive-erp! 'sample-exponential sample-exponential-erp)

         (register-primitive-erp! 'gamma-default-proposal gamma-erp-with-default-proposal)
         (register-primitive-erp! 'gaussian-default-proposal gaussian-erp-with-default-proposal)
         (register-primitive-erp! 'poisson-default-proposal poisson-erp-with-default-proposal)
         (register-primitive-erp! 'exponential-default-proposal sample-exponential-erp-with-default-proposal)

         ;; @desc
         ;; Sample a permutation of indices for a list.
         ;; @param size of list to permute.
         ;; @return number
         (register-primitive-erp! 'sample-index-permutation sample-index-permutation-erp)

         ;; @desc
         ;; Apply an index permutation to a list.
         ;; (Use this to apply the result of sample-index-permutation.)
         ;; @param Lst The list to permute.
         ;; @param I The index permutation.
         ;; @return list
         (register-primitive-procedure! 'permute-list
                                        permute-list
                                        (list 'deterministic-inverse
                                              (lambda (val . init-args)
                                                (let* ((cur-index-perm (second init-args))
                                                       (cur-lst (first init-args)))
                                                  (list (list (inverse-permute-list (augment-value-list val (length cur-index-perm))
                                                                                    cur-index-perm)
                                                              cur-index-perm))))))

         ;; @desc
         ;; Apply the inverse of an index permutation to a list.
         ;; @param Lst The list to permute.
         ;; @param I The index permutation (to invert).
         ;; @return list
         (register-primitive-procedure! 'inverse-permute-list
                                        inverse-permute-list
                                        (list 'deterministic-inverse
                                              (lambda (val . init-args)
                                                (let* ((cur-index-perm (second init-args))
                                                       (cur-lst (first init-args)))
                                                  (list (list (permute-list (augment-value-list val (length cur-index-perm)) cur-index-perm) cur-index-perm))))))



         ;;;queries


         ;; @form (mh-query samples lag defines query-expr cond-expr)
         ;; @desc
         ;; Do inference using the Metropolis-Hastings algorithm (as described in Goodman, et al., 2008).
         ;; @param samples The number of samples to take.
         ;; @param lag The number of iterations in between samples.
         ;; @param defines ... A sequence of definitions (usually setting up the generative model).
         ;; @param query-expr The query expression.
         ;; @param cond-expr The condition expression.
         ;; @return A list of samples from the conditional distribution.
         (register-query! 'mh-query mh-query)

         (register-query! 'visited-mh-query visited-mh-query)
         (register-query! 'pt-query pt-query)
         (register-query! 'tt-query tt-query)
         (register-query! 'gradient-query gradient-query)
         (register-query! 'visited-gradient-query visited-gradient-query)
         (register-query! 'hmc-query hmc-query)
         (register-query! 'visited-hmc-query visited-hmc-query)


           ;;all these queries are deprecated....
                                        ;;(register-primitive-procedure! 'emc-lex-query emc-lex-query)
                                        ;;(register-primitive-procedure! 'mtm-lex-query mtm-lex-query)
                                        ;(register-primitive-procedure! 'simple-TT-lex-query simple-TT-lex-query)
                  (register-primitive-procedure! 'repeated-mh-lex-query repeated-mh-lex-query)
                  (register-primitive-procedure! 'mh-lex-query mh-lex-query)
                  (register-primitive-procedure! 'repeated-mh-def-query repeated-mh-def-query)
                  (register-primitive-procedure! 'tmh-lex-query tempered-repeated-mh-lex-query)
                  (register-primitive-procedure! 'tempered-repeated-mh-lex-query tempered-repeated-mh-lex-query)
                  (register-primitive-procedure! 'tempered-mh-lex-query tempered-mh-lex-query)

         ;;;XRP inits (this forces ikarus to eval the files, which registers the pieces...)
         (init-normal-normal-gamma)
         (init-dirichlet-discrete)
         (init-ms-dirichlet-discrete)
         (init-CRP)
         (init-gensym-xrp)
         
         ;levenshtein distance
         (register-primitive-procedure! 'lev-dist list-levenshtein/equal)
         
         ; seed the random number generator
         (randomize-rng)

         (when (> (*verbosity*) 12)
           (display "loaded standard-env.ss\n"))

         )
