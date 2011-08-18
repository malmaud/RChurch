#!r6rs

;;
;; MPI bindings for ikarus.
;;
;;
;; D. Wingate 2/6/09
;;

(library (church external mpi-bindings)
	 (export MPI_Init MPI_Comm_rank MPI_Comm_size MPI_Finalize
		 MPI_Probe ;; MPI_IProbe 
		 MPI_Send MPI_Recv)

(import
  (church utils rnrs)
  (ikarus)
  (ikarus foreign)
  (church external binding-utils)
)

(define libmpi (dlopen "/usr/lib/libmpi.so" #t #f))

;;
;; ======================================================================
;;

;; XXX these are, unfortunately, almost certainly
;; implementation-dependent. they're taken from MPI.h, which is
;; inaccessible to ikarus' FFI.

;; communicators
(define MPI_COMM_WORLD 91)
(define MPI_COMM_SELF 92)
(define MPI_GROUP_EMPTY 90)

;; tag types
(define MPI_ANY_TAG -1)

;; source types
(define MPI_ANY_SOURCE -2)

;; mpi_datatype
(define MPI_CHAR 1)

;;
;; ======================================================================
;;

(define _MPI_Init
  ((make-c-callout 'void '(pointer pointer)) (dlsym libmpi "MPI_Init")))

;; the second parameter to MPI_Init is a char ***.  That's so that it
;; can modify the command-line parameters passed in, to erase everything
;; that mpirun added for startup purposes.

(define (MPI_Init)
  (let* (
        (num_cmdline_args (length (command-line-arguments)))
        (argc_ptr (malloc (get-ctype-size 'int)))  ;; to hold the argc*
        (retval 42)

        ;; a list of pointers to individually allocated strings
        (main_array (map string->cblock (command-line-arguments) ))

        ;; argv_ptr is a pointer to a cblock of pointers.
        (argv_ptr (list->cblock main_array 'pointer))

        (argv_ptrptr (malloc (get-ctype-size 'pointer)))
        )

    (pointer-set-c-pointer! argv_ptrptr 0 argv_ptr)

    (pointer-set-c-int! argc_ptr 0 num_cmdline_args)

;;    (map display (list "We have " num_cmdline_args " argument(s)\n" ))

    (_MPI_Init argc_ptr argv_ptrptr )

    (set! retval (pointer-ref-c-unsigned-int argc_ptr 0 ) )
    (free argc_ptr)

    (map free main_array)
    (free argv_ptr)
    (free argv_ptrptr)

;;    (display "MPI successfully initialized!\n")

    #t
  ))

;;
;; ======================================================================
;;

(define _MPI_Comm_rank
  ((make-c-callout 'void '(unsigned-int pointer)) (dlsym libmpi "MPI_Comm_rank")))

(define (MPI_Comm_rank)
  (let ( (result_ptr (malloc (get-ctype-size 'int)))
         (retval 42) )
    (_MPI_Comm_rank MPI_COMM_WORLD result_ptr)
    (set! retval (pointer-ref-c-unsigned-int result_ptr 0 ) )
    (free result_ptr)
    retval
    ))

;;
;; ======================================================================
;;

(define _MPI_Comm_size
  ((make-c-callout 'void '(unsigned-int pointer)) (dlsym libmpi "MPI_Comm_size")))

(define (MPI_Comm_size)
  (let ( (result_ptr (malloc (get-ctype-size 'int)))
         (retval 42) )
    (_MPI_Comm_size MPI_COMM_WORLD result_ptr)
    (set! retval (pointer-ref-c-unsigned-int result_ptr 0 ) )
    (free result_ptr)
    retval
    ))

;;
;; ======================================================================
;;

(define _MPI_Finalize
  ((make-c-callout 'void '()) (dlsym libmpi "MPI_Finalize")))
(define (MPI_Finalize) (_MPI_Finalize))

;;
;; ======================================================================
;;

(define _MPI_Send
  ((make-c-callout 'signed-int '(pointer signed-int signed-int signed-int signed-int signed-int)) (dlsym libmpi "MPI_Send")))

(define (MPI_Send dest msg)
  (let ((data_ptr (bytevector->cblock msg)))
    (_MPI_Send data_ptr (bytevector-length msg) MPI_CHAR dest 42 MPI_COMM_WORLD)
    (free data_ptr)
    ))

;;
;; ======================================================================
;;

;; according to mpidefs.h, an mpi_status object is four integers.
(define (make-status)
  (malloc (* 4 (get-ctype-size 'int))))
(define (free-status s)
  (free s))

;;
;; ======================================================================
;;

(define _MPI_Get_count
  ((make-c-callout 'signed-int '(pointer signed-int pointer)) (dlsym libmpi "MPI_Get_count")))

(define (MPI_Get_count status_ptr)
  (let* (
	 (result_ptr (malloc (get-ctype-size 'int)))
	 (result 42)
	 )
    (_MPI_Get_count status_ptr MPI_CHAR result_ptr)
    (set! result (pointer-ref-c-signed-int result_ptr 0))
    (free result_ptr)
    result
    ))

;;
;; ======================================================================
;;

(define _MPI_Probe
  ((make-c-callout 'signed-int '(signed-int signed-int signed-int pointer)) (dlsym libmpi "MPI_Probe")))

(define (MPI_Probe src)
  (let* (
	 (status_ptr (make-status))
	 (result 42)
	 )
    (_MPI_Probe src MPI_ANY_TAG MPI_COMM_WORLD status_ptr )
    (set! result (MPI_Get_count status_ptr))
    (free-status status_ptr)
    result
    ))

;;
;; ======================================================================
;;

;; (define _MPI_IProbe
;;   ((make-c-callout 'signed-int '(signed-int signed-int signed-int pointer pointer)) (dlsym libmpi "MPI_IProbe")))

;; (define (MPI_IProbe src)
;;   (let* (
;; 	 (status_ptr (make-status))
;; 	 (flag_ptr (malloc (get-ctype-size 'int)))
;; 	 (result 42)
;; 	 )

;;     (_MPI_IProbe src MPI_ANY_TAG MPI_COMM_WORLD flag_ptr status_ptr )

;;     (if (pointer-ref-c-signed-int flag_ptr 0)
;; 	;; there's a message waiting.  return true and length
;; 	(begin
;; 	  (set! result (MPI_Get_count status_ptr))
;; 	  (free-status status_ptr)
;; 	  (#t result)
;; 	  )
;; 	;; no message waiting
;; 	(begin
;; 	  (free-status status_ptr)
;; 	  (#f)
;; 	  ))
;;     ))

;;
;; ======================================================================
;;

(define _MPI_Recv
  ((make-c-callout 'signed-int '(pointer signed-int signed-int signed-int signed-int signed-int pointer)) (dlsym libmpi "MPI_Recv")))

(define (MPI_Recv src)
  (let* (
	 (data_len (MPI_Probe src))
	 (data_ptr (malloc data_len))
	 (status_ptr (make-status))
	 (result 42)
	 )
    (_MPI_Recv data_ptr data_len MPI_CHAR src MPI_ANY_TAG MPI_COMM_WORLD status_ptr)
    ;; assert that status_ptr -> length == data_len (ie, we got what we were expecting)
    (set! result (cblock->bytevector data_ptr data_len))
    (free data_ptr)
    result
    ))

)