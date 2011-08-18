#!r6rs

;; =============================================================================
;;
;; py-pickle
;;
;; for examples go to:
;; http://scripts.mit.edu/~droy/church/index.php?title=Python
;;
;; Added 05/01/09: fjaekel, inspired by wingated's genviz and ast's r
;;

(library (church external py-pickle)
         (export py-pickle-function
                 py-pickle-script
                 py-pickle
                 py-pickle-string
                 py-unpickle
                 py-unpickle-string
                 open-py-port
                 open-py-ports
                 close-py-port
                 close-py-ports)
         (import (church utils rnrs)
                 (only (ikarus) system format gensym fprintf inexact->exact))

;; =============================================================================
;; functions to communicate with a python script via fifos

;; -----------------------------------------------------------------------------
;; generate a scheme function that calls out to a python script and
;; wait for the response that the python script generates
;; (py-pickle-function) or don't wait for the response
;; (py-pickle-script). The way that this is done is by running the
;; script from the shell and piping the arguments into stdin and
;; waiting for the responses on stdout. It is the responsibility of
;; the python programmer to write the script such that it reads the
;; arguments from stdin and sends the results to stdout. This has to
;; be done using the pickle.load(sys.stdin) function until all
;; arguments are read. Values x are returned by using the
;; pickle.dump(x,sys.stdout) function. Since we are using stdin and
;; stdout for process communication a python script MUST NOT write
;; anything to stdout!  Note also that not the whole pickle protocol
;; is implemented, but lists, strings, floats and ints are fine (see
;; py-pickle and py-unpickle below).

(define (py-pickle-function py-file)
  (lambda (x . rest)
    (py-pickle-function-call py-file (cons x rest))))

(define (py-pickle-function-call py-file args)
  (py-pickle-function-call-with-fifo-path py-file "/tmp" args))

(define (py-pickle-function-call-with-fifo-path py-file
                                                fifo-prefix
                                                args)
  (let* ((ss-to-py-fn (make-ss-to-py-fn fifo-prefix))
         (py-to-ss-fn (make-py-to-ss-fn fifo-prefix))
         ;; generate fifo files
         (ss-to-py-fifo (make-fifo ss-to-py-fn))
         (py-to-ss-fifo (make-fifo py-to-ss-fn))
         ;; start python script and connect to ports
         (dummy1 (py-pickle-function-start py-file ss-to-py-fn py-to-ss-fn))
         (ss-to-py-port (open-ss-to-py-port ss-to-py-fn))
         (py-to-ss-port (open-py-to-ss-port py-to-ss-fn))
         ;; clean up fifo file immediately
         (dummy2 (delete-fifo ss-to-py-fn))
         (dummy3 (delete-fifo py-to-ss-fn)))
    ;; send args to the python script and wait for response
    (py-pickle-args ss-to-py-port args)
    (py-unpickle py-to-ss-port)))

;; .............................................................................
;; start process in the background

(define (py-pickle-function-start py-file ss-to-py-fn py-to-ss-fn)
  (let ((cmd (format "python ~a < ~a > ~a &\n"
                     py-file
                     ss-to-py-fn
                     py-to-ss-fn)))
    (system cmd)))


;; ----------------------------------------------------------------------------
;; same thing for script that does not wait for response

(define (py-pickle-script py-file)
  (lambda (x . rest)
    (py-pickle-script-call py-file (cons x rest))))

(define (py-pickle-script-call py-file args)
  (py-pickle-script-call-with-fifo-path py-file "/tmp" args))

(define (py-pickle-script-call-with-fifo-path py-file
                                              fifo-prefix
                                              args)
  (let* ((ss-to-py-fn (make-ss-to-py-fn fifo-prefix))
         ;; generate fifo files
         (ss-to-py-fifo (make-fifo ss-to-py-fn))
         ;; start python script and connect to port
         (dummy1 (py-pickle-script-start py-file ss-to-py-fn))
         (ss-to-py-port (open-ss-to-py-port ss-to-py-fn))
         ;; clean up fifo file immediately
         (dummy2 (delete-fifo ss-to-py-fn)))
    ;; send args to the python script and don't wait for response
    (py-pickle-args ss-to-py-port args)
    (close-py-port ss-to-py-port)))

;; .............................................................................
;; start process in the background and let it sit there until it
;; decides to quit

(define (py-pickle-script-start py-file ss-to-py-fn)
  (let ((cmd (format "python ~a < ~a &\n"
                     py-file
                     ss-to-py-fn)))
    (system cmd)))


;; -----------------------------------------------------------------------------
;; another way to communicate with a python script is to explicitly
;; use ports and the py-pickle function. This is different from
;; py-pickle-function because it does not return anything from the
;; python script. Here, we are not waiting for the python script to
;; return something by writing someting to stdout. This is also
;; different from py-pickle-script because the port can be kept open
;; to send more stuff to the script. In fact, the python script will
;; be expected to wait for further inputs from scheme and never quit
;; until eof is sent. This is useful for plotting into the same
;; figure, for example.

(define (open-py-port py-file)
  (open-py-port-with-fifo-path py-file "/tmp"))

(define (close-py-port py-port)
  (if (pair? py-port)
      (close-py-ports py-port)
      (close-output-port py-port)))

(define (open-py-port-with-fifo-path py-file fifo-prefix)
  (let* ((ss-to-py-fn (make-ss-to-py-fn fifo-prefix))
         ;; generate fifo file
         (ss-to-py-fifo (make-fifo ss-to-py-fn))
         ;; start python script and connect to port
         (dummy1 (py-pickle-script-start py-file ss-to-py-fn))
         (ss-to-py-port (open-ss-to-py-port ss-to-py-fn))
         ;; clean up fifo file immediately
        (dummy2 (delete-fifo ss-to-py-fn)))
    ;; return the port
    ss-to-py-port))

;; ----------------------------------------------------------------------------
;; the same thing with bidirectional communication

(define (open-py-ports py-file)
  (open-py-ports-with-fifo-path py-file "/tmp"))

(define (close-py-ports ports)
  (close-output-port (car ports))
  (close-input-port (cdr ports)))

(define (open-py-ports-with-fifo-path py-file fifo-prefix)
  (let* ((ss-to-py-fn (make-ss-to-py-fn fifo-prefix))
         (py-to-ss-fn (make-py-to-ss-fn fifo-prefix))
         ;; generate fifo file
         (ss-to-py-fifo (make-fifo ss-to-py-fn))
         (py-to-ss-fifo (make-fifo py-to-ss-fn))
         ;; start python script and connect to port
         (dummy1 (py-pickle-function-start py-file ss-to-py-fn py-to-ss-fn))
         (ss-to-py-port (open-ss-to-py-port ss-to-py-fn))
         (py-to-ss-port (open-py-to-ss-port py-to-ss-fn))
         ;; clean up fifo file immediately
         (dummy2 (delete-fifo ss-to-py-fn))
         (dummy3 (delete-fifo py-to-ss-fn)))
    ;; return the two port
    (cons ss-to-py-port py-to-ss-port)))

;; -----------------------------------------------------------------------------
;; helper functions to deal with I/O

(define (make-ss-to-py-fn fifo-prefix)
  (let ((fifo-suffix (gensym)))
    (format "~a/ss_to_py_fifo_~a" fifo-prefix fifo-suffix)))

(define (make-py-to-ss-fn fifo-prefix)
  (let ((fifo-suffix (gensym)))
    (format "~a/py_to_ss_fifo_~a" fifo-prefix fifo-suffix)))

(define (make-fifo fn)
  (let ((cmd (format "rm -f ~a && mkfifo ~a" fn fn )))
    (system cmd)))

(define (delete-fifo fn)
  (let ((cmd (format "rm -f ~a" fn )))
    (system cmd)))

(define (open-ss-to-py-port fn)
  (open-file-output-port fn
                         (file-options no-create no-truncate)
                         (buffer-mode block)
                         (make-transcoder (latin-1-codec))))

(define (open-py-to-ss-port fn)
  (open-file-input-port fn
                        (file-options no-create no-truncate)
                        (buffer-mode block)
                        (make-transcoder (latin-1-codec))))


;; =============================================================================
;; functions for pickling and unpickling

;; -----------------------------------------------------------------------------
;; pickling means translating (or serialzing) a complex object x that
;; can consists of lists of lists with mixed types to a
;; python-pickle-style string that can then be send around or saved or
;; whatever. Here we only implement a small part of python's protocol
;; 0, i.e. we only have list, string, int, and float. Protocol 0 is
;; just ascii. Details of how protocol 0 works can be found in
;; 'pythontools.py'. The function pythontools.dis() is useful because
;; it shows what the pickle machine does to unpickle a string. From
;; this one can reverse-engineer what is going on. Basically it is
;; just a stack machine (with external memory that we did not
;; implement).
;;
;; 'S' push the string that follows and ends with \n onto the stack
;; 'I' push an integer
;; 'F' push a float
;; 'l' push an empty list onto stack
;; 'a' append top element to list below
;; '.' stop symbol
;;
;; '(' make a mark (not implemented here)
;; 'p' put into memory (not implemented here)
;; 'g' get from memory (not implemented here)
;;
;; no other data types are currently implemented here

;; ............................................................................
;; convenience wrapper for pickling any number of arguments

(define (py-pickle port . args)
  (py-pickle-args port args))

;; ............................................................................
;; write one argument to the port and return the port

(define (py-pickle-arg port x)
    (if (pair? port)
        (begin
          (fprintf (car port) (py-pickle-string x))
          (flush-output-port (car port)))
        (begin
          (fprintf port (py-pickle-string x))
          (flush-output-port port)))
    port)

;; ............................................................................
;; do the same for a list of arguments

(define (py-pickle-args port args)
  (if (null? args)
      port
      (begin
        (py-pickle-arg port (car args))
        (py-pickle-args port (cdr args)))))

;; ............................................................................
;; translate a complex object to a string

(define (py-pickle-string x)
  (let ((s (string-append (py-pickle-object x) ".")))
    ;(display s)
    s))

;; ............................................................................
;; translate the base types into corresponding strings

(define (py-pickle-object x)
  (cond
   ((string? x)  (format "S~s\n" x))
   ((symbol? x)  (format "S~s\n" (symbol->string x)))
   ((integer? x) (format "I~a\n" (inexact->exact x)))
   ((real? x)    (format "F~a\n" x))
   ((null? x)    "(l")
   ((list? x)    (format "(l~aa~a"
                         (py-pickle-object (car x))
                         (py-pickle-list (cdr x))))
   (else (display "church/external/py-pickle: unsupported data type! \n"))))

;; ...........................................................................
;; for lists we have to make sure the 'a' (for append) in the protocol
;; comes at all the right places

(define (py-pickle-list x)
  (if (null? x)
      "" 
      (format "~aa~a"
              (py-pickle-object (car x))
              (py-pickle-list (cdr x)))))


;; ----------------------------------------------------------------------------
;; unpickling is done by implementing an incomplete version of the
;; pickle-machine, in particular put and get do not work

;; ............................................................................
;; translate a string back into a complex object of lists of lists

(define (py-unpickle port)
  (if (pair? port)
      (py-unpickle-stack (cdr port) '())
      (py-unpickle-stack port '())))

;; ............................................................................
;; wrapper for string variant

(define (py-unpickle-string s)
  (py-unpickle (open-string-input-port s)))

;; ............................................................................
;; actual function that does the work

(define (py-unpickle-stack port stack)
  (let ((c (get-char port)))
    ; (display (format "char ~a stack ~a\n" c stack))
    (cond
     ;; this should not happen because the input should end with a
     ;; . before end of file is reached
     ((eof-object? c)
      (begin
        (display "church/external/py-pickle: eof before '.'! \n")
        '()))
     ;; otherwise let's see what character we have
     ((char=? c #\.) (py-unpickle-stop port stack))
     ((char=? c #\S) (py-unpickle-strg port stack))
     ((char=? c #\I) (py-unpickle-nmbr port stack))
     ((char=? c #\F) (py-unpickle-nmbr port stack))
     ((char=? c #\() (py-unpickle-mark port stack))
     ((char=? c #\l) (py-unpickle-list port stack))
     ((char=? c #\a) (py-unpickle-apnd port stack))
     ((char=? c #\p) (py-unpickle-put port stack))
     ((char=? c #\g) (py-unpickle-get port stack))
     (else (begin
             (display "church/external/py-pickle: read unknown symbol! \n")
             '())))))

;; ............................................................................
;; '.' we have reached the stop symbol

(define (py-unpickle-stop port stack)
  ;; the stack should have only one element now
  (if (= (length stack) 1)
      (car stack)
      ;; otherwise something went wrong
      (begin
        (display "church/external/py-pickle: non-empty stack after '.'! \n")
        '())))

;; ............................................................................
;;'(' ignore the opening paren that makes a mark in the pickle
;; machine since it only occurs with '(l' for the simple data
;; types that we want to recover here

(define (py-unpickle-mark port stack)
  (py-unpickle-stack port stack))

;; ............................................................................
;; 'l' starts a new list and puts it on the stack of lists

(define (py-unpickle-list port stack)
  (py-unpickle-stack port (cons '() stack)))

;; ............................................................................
;; 'a' appends the object on top of the stack to the previous list

(define (py-unpickle-apnd port stack)
  (if (> (length stack) 1)
      (py-unpickle-stack port
                         (cons (append (cadr stack)
                                       (list (car stack)))
                               (cddr stack)))
      ;; complain if there are less than two lists on the stack     
      (begin
        (display "church/external/py-pickle: nothing to append to! \n")
        '())))

;; ............................................................................
;; 'S' push a string on the stack

(define (py-unpickle-strg port stack)
  (let*
      ((ss (get-line port))                           ; this has '' around it
       (s (substring                                  ; this hasn't
           ss 1 (- (string-length ss) 1))))           ; (strings start at 0)
    (py-unpickle-stack port
                       (cons s stack))))

;; ............................................................................
;; 'I' or 'F' push a number on the stack

(define (py-unpickle-nmbr port stack)
  (let*
      ((s (get-line port))                            ; this is a string
       (n (string->number s)))                        ; and this is a number
    (py-unpickle-stack port
                       (cons n stack))))

;; ............................................................................
;; 'p' put: store a value so that you can use it later with get. This
;; is not implemented at the moment so we just ignore it. If you do
;; want to implement it you might want to use an additional argument
;; to pass around.

(define (py-unpickle-put port stack)
  (get-line port)
  (py-unpickle-stack port stack))

;; ............................................................................
;; 'g' get: get a value from the memory of the pickle machine. If
;; someone tries to do that return the empty list and warn about the
;; fact that this is not implemented

(define (py-unpickle-get port stack)
  (begin
    (display "church/external/py-pickle: get is not implemented! \n")
    '()))


;; =============================================================================
;; end of library

)

