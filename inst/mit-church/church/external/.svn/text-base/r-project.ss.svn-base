#!r6rs

;; ============================================================================
;; Run R in a shell and generate a port to send commands
;;
;; this is intended to be useful for plotting from within ikarus,
;; here's an example:
;;
;; (define r-port (open-r-port))
;; (r r-port "plot" (list 1 2 3)
;;                  (map log (list 1 2 3))
;;                  "xlab=" "x"
;;                  "ylab=" "y"
;;                  "main=" "title"
;;                  "type=" "l")
;; (close-r-port r-port)
;;
;; it can happen that a r command line gets too long because a list
;; that you want to pass is too long. A workaround for this is
;; assigning the list to a variable first by using r-assign. This
;; builds the r-vector one element at a time and then using this
;; variable later. e.g. to do "a <- c(1,2,3)" (but incrementally) do
;;
;; (r-assign r-port "a" (list 1 2 3))
;; (r r-port "plot" (list "a"))
;;
;; to avoid passing it as a string you have to put it in a list as done
;; above or simply use put-r-string
;;
;; (put-r-string r-port "plot(a)")
;;
;; if you want to use it from within church you can do the following:
;;
;; (import (church church) (church external r-project))
;; (register-primitive-procedure! 'open-r-port open-r-port)
;; (register-primitive-procedure! 'close-r-port close-r-port)
;; (register-primitive-procedure! 'r r)
;; (church
;;  (define r-port (open-r-port))
;;  (r r-port "plot" (list 1 2 3) (list 1 2 3))
;;  (r r-port "locator" 1)
;;  (close-r-port r-port)
;; )
;;
;; 04-28-09 fj -- adapted from Andreas's r.ss and David's fifo code to
;; communicate with ogre
;;
;; 10-21-09 fj -- in order to circumvent problems with the commandline
;; being too short and therefore long lists cannot be passed over to
;; R, there is now a function r-assign that will assign a list to
;; variable by growing it incrementally
;;

(library (church external r-project)
         (export open-r-port close-r-port r put-r-string
                 parse-args r-assign
                 set-r-params!)
         (import (church utils rnrs)
                 (only (ikarus) system format gensym fprintf inexact->exact))

;; --------------------------------------------------------------------------
;; parameters

(define r-params "--vanilla --quiet --interactive")

(define (set-r-params! param-string)
  (set! r-params param-string))


;; --------------------------------------------------------------------------
;; communication with fifos

(define (make-church-to-r-fn fifo-prefix fifo-suffix)
(format "~a/church_to_r_fifo_~a" fifo-prefix fifo-suffix))

(define (make-fifo fn)
  (let ((cmd (format "rm -f ~a && mkfifo ~a" fn fn )))
    (system cmd)))

(define (delete-fifo fn)
  (let ((cmd (format "rm -f ~a" fn )))
    (system cmd)))

;; ----------------------------------------------------------------------------
;; open and close ports

(define (open-r-port)
  (open-r-port-with-path "/tmp"))

(define (close-r-port p)
  (begin
    (display "closing R...\n")
    (close-output-port p)))

(define (open-r-port-with-path fifo-prefix)
  (let* ((fifo-suffix (gensym))
         (church-to-r-fn (make-church-to-r-fn fifo-prefix fifo-suffix))
         ;; generate fifo file
         (church-to-r-fifo (make-fifo church-to-r-fn))
         ;; start r and connect port to r
         (r (start-r church-to-r-fn))
         (church-to-r-port (open-church-to-r-port church-to-r-fn))
         ;; clean up fifo file immediately
         (d (delete-fifo church-to-r-fn)))
    ;; return port
    church-to-r-port))

(define (open-church-to-r-port fn)
  (open-file-output-port fn
                         (file-options no-create no-truncate)
                         (buffer-mode block) ;; line?
                         (make-transcoder (latin-1-codec))))

;; ----------------------------------------------------------------------------
;; start r and send commands

(define (start-r church-to-r-fn)
  ;; --vanilla makes sure that workspace in not restored and not saved
  ;; --quit suppresses intro
  ;; --interactive allows X11 (alternatively try --gui=X11
  (let ((cmd (format "R ~a < ~a &\n"
                     r-params
                     church-to-r-fn)))
    (display "starting R...\n")
    (display cmd)
    (system cmd)))

(define (put-r-string r-port string)
  (begin
    (put-string r-port string)
    (put-string r-port "\n")
    (flush-output-port r-port)))

(define (r r-port function . args)
  (if (null? args)
      (put-r-string r-port (string-append function "()"))
      (let ((parameters (parse-args args)))
        (put-r-string r-port (string-append function "(" parameters ")")))))

;; ----------------------------------------------------------------------------
;; parse arguments and translate types into appropriate strings

(define (val->r-string arg)
  (cond ((string? arg) (format "'~a'" arg))
        ((list? arg) (list->r-string (map val->r-string arg)))
        ((boolean? arg) (if arg "TRUE" "FALSE"))
        (else (format "~a" arg))))

(define (list->r-string r-strings)
  (let* ((comma-aft-values (apply string-append
                                  (map (lambda (x) (format "~a," x)) r-strings)))
         (comma-sep-values (substring comma-aft-values
                                      0
                                      (- (string-length comma-aft-values) 1))))
    (format "c(~a)" comma-sep-values)))

(define (parse-args args)
  (if (null? (cdr args))
      ;; if there is just one element in the list convert this element to
      ;; the right string
      (val->r-string (car args))
      ;; if there are still several elements in the list we need to see
      ;; whether they need to be comma separated or not, e.g. "xlab=" is not
      ;; followed by a comma
      (if (string? (car args))
          (if (char=? #\=
                      (string-ref (car args)
                                  (- (string-length (car args)) 1)))
              (format "~a~a"
                      (car args)
                      (parse-args (cdr args)))
              (format "~a, ~a"
                      (parse-args (list (car args)))
                      (parse-args (cdr args))))
          (format "~a, ~a"
                  (parse-args (list (car args)))
                  (parse-args (cdr args))))))

(define (r-assign r-port variable-name alist)
  (begin
    (put-r-string r-port (format "~a <- c()" variable-name))
    (r-assign2 r-port variable-name alist)))

(define (r-assign2 r-port variable-name alist)
  (if (null? alist)
      '()
      (let* ((s (format "~a <- c(~a,~a)"
                        variable-name
                        variable-name
                        (car alist))))
        (put-r-string r-port s)
        (r-assign2 r-port variable-name (cdr alist)))))

)