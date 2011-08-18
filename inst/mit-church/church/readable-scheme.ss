#!r6rs

(library (church readable-scheme)
         
         (export pair head tail first second third fourth fifth sixth seventh eighth
                 ninth tenth take take! drop last rest
                 mlist mlist-ref mpair mcar mcdr set-mcar! set-mcdr! mhead mtail set-mhead! set-mtail! 
                 list-elt vector-elt
                 repeat range write-to-string cropped-write-to-string display-justified
                 true false true? false?
                 assert-with-message tagged-list?
                 *verbosity* *safe-mode* *global-debug* *pretty-pictures*
                 display-list
                 gensym
                 make-parameter
                 compose
                 fail? normalize map-until accumulative-fold
                 andmap ormap void pretty-print modulo random exact->inexact inexact->exact
                 random-real random-integer
                 sum uniform-draw flip log-flip
                 assoc/default)
         
         (import (_srfi :1)  ; lists
                 (_srfi :43) ; vectors
                 (_srfi :69) ; hash tables
                 (church utils rnrs)
                 (rnrs io ports)
                 (rnrs mutable-pairs (6))
                 (church utils utils)
                 (church external math-env))
         
         ;; default verbosity level is 11: this prints results and some status messages.
         (define *verbosity* (make-parameter 11))
         
         ;; turn on to make a picture of trace of last expression
         ;; (used in church.ss, but created here so that standard-env.ss sees it).
         (define *pretty-pictures* (make-parameter #f))

         
         ;; definitions
         
         ;;r6rs has mutable pairs (unlike plt):
         (define mlist list)
         (define mlist-ref list-ref)
         (define mcons cons)
         (define mcar car)
         (define mcdr cdr)
         (define set-mcar! set-car!)
         (define set-mcdr! set-cdr!)
         
         
         (define rest cdr)
         
         (define pair cons)
         (define head car)
         (define tail cdr)
         
         (define mpair mcons)
         (define mhead mcar)
         (define mtail mcdr)
         
         (define set-mhead! set-mcar!)
         (define set-mtail! set-mcdr!)
         
         (define (list-elt l n) (list-ref l (- n 1)))
         (define (vector-elt v n) (vector-ref v (- n 1)))

         (define (assoc/default key alist default)
           (let ([lookup-val (assoc key alist)])
             (if (false? lookup-val)
                 default
                 (cdr lookup-val))))
         
         (define true #t)
         (define false #f)
         
         (define (true? x)
           (not (eq? x false)))
         
         (define (false? x)
           (eq? x false))
         
         
         (define (repeat n thunk)
           (if (> n 0)
               (pair (thunk) (repeat (- n 1) thunk))
               (list) ))
         
         (define (range from to)
           (if (> from to)
               '()
               (pair from (range (+ from 1) to))))
         
         (define (display-list lst)
           (for-each display lst) )
         
         (define (display-justified str n)
           (display str)
           (let ((len (string-length str)))
             (when (< len n)
               (repeat (- n len) (lambda () (display " ")))))
           '())
         
         (define (write-to-string val)
           (let-values (((string-port extractor) (open-string-output-port)))
             (write val string-port)
             (extractor)))
         ;(get-output-string string-port)))
         
         (define (cropped-write-to-string val n)
           (let ((string (write-to-string val)))
             (if (<= (string-length string) n)
                 string
                 (string-append (substring (write-to-string val) 0 n) "..."))))
         
         
         ;;some utilities used in many places (and fairly generic):
         (define *safe-mode* (make-parameter #f))
         
         (define *global-debug* (make-parameter #f))
         
         ;; TODO: this should become syntax!
         (define (debugging expr) 
           (when (not (*safe-mode*)) 
             expr))
         
         (define (assert-with-message boolean-promise message-promise . optional-thunk)
           (when (and (*safe-mode*)
                      (not (boolean-promise)) )
             (begin (when (not (null? optional-thunk))
                      ((car optional-thunk)))
                    (error "assert-with-message" (message-promise)) )))
         
         (define (tagged-list? exp tag)
           (if (pair? exp)
               (eq? (car exp) tag)
               false))
         
         ; TODO: arbitrary number of arguments
         (define (compose f g) (lambda (x) (f (g x))))
         
         ; FIXME: Move the following things somewhere else?
         
         (define (sum vec) (apply + vec))
         
         (define (uniform-draw lst) (list-ref lst (random-integer (length lst)) ))
         
         (define (flip . w)
           (if (null? w) 
               (< (random-real) 0.5) 
               (< (random-real) (car w)) ))

         (define (log-flip . w)
           (if (null? w)
               (< (random-real) 0.5)
               (< (log (random-real)) (first w))))
               ;;(< (random-real) (exp (first w)) )))
         
         (define (fail? t)
           (or (eq? t 'fail)
               (and (pair? t)
                    (eq? (first t) 'fail))))

         (define (normalize weights)
           (let ([total (sum weights)])
             (map (lambda (w) (/ w total)) weights)))

         ;; map-until applies proc to each element in l (in order), stops if
         ;; stop-test is true for some value and then returns list of values
         ;; built up so far.
         (define (map-until proc l stop-test)
           (if (null? l)
               '()
               (let ((val (proc (first l))))
                 (if (stop-test val)
                     (pair val '())
                     (pair val (map-until proc (rest l) stop-test))))))

         ;; accumulative-fold is like fold, but 1) keeps around a list
         ;; of its intermediate results and 2) takes an additional function
         ;; that is applied to the results that are passed into proc,
         ;; but not to the intermediate results
         (define (accumulative-fold proc lst results-so-far result-modifier)
           (if (null? lst)
               '()
               (let ((val (proc (first lst) results-so-far)))
                 (pair val (accumulative-fold proc
                                              (rest lst)
                                              (cons (result-modifier val) results-so-far)
                                              result-modifier)))))
         
         (when (> (*verbosity*) 12)
           (display "loaded readable-scheme.ss\n"))
         
         )