#!r6rs

;;
;; A high level parallel interface.  The current implementation uses
;; MPI, but it's designed to be (relatively) straightforward to swap
;; in another implementation.
;;
;; D. Wingate 2/6/09
;;

(library (church external parallel-comm)

  (export parallel-initialize parallel-finalize
	  get-neighbors get-my-name
	  par-msg-send par-msg-recv )

  (import
   (church utils rnrs)
   (ikarus)
   (church external binding-utils)
   (church external mpi-bindings)
   (church utils serializer)
   )

;;
;; ======================================================================
;;

  (define (parallel-initialize) (MPI_Init))
  (define (parallel-finalize) (MPI_Finalize))

;;
;; ======================================================================
;;

  (define (seq T)
    (if (= T 0)
        (list 0)
        (append (seq (- T 1)) (list T))))

  ;; Right now, our neighbors are everyone in the pool of machines
  (define (get-neighbors)
    (seq (- (MPI_Comm_size) 1)))

  (define (get-my-name)
    (MPI_Comm_rank))

  (define (par-msg-send dest msg)
    (MPI_Send dest (serialize msg)))

  (define (par-msg-recv dest)
    (deserialize (MPI_Recv dest)))

)