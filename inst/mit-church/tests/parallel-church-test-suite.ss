#!r6rs

;;
;; This program operates like an ordinary MPI program.  It must be
;; invoked via mpirun.
;;
;; If you've never used mpirun before, try the following:
;;   1) Create a file called "machinefile"
;;   2) It should have a single line: "localhost:8" (replace 8 with the
;;      maximum number of processes to run on your local machine).
;;   3) Ensure that ssh is setup to allow password-free logins to localhost.
;;   4) Run the following commands in a bash shell:
;;
;;      export P4_RSHCOMMAND=ssh
;;      mpirun -v -machinefile machinefile -np 2 ./test_pc.sh
;;
;; NOTE: test_pc.sh HAS SOME HARDCODED PATHS IN IT!  You'll need to edit it.
;;
;; You should see the following output:
;;
;; Hello, world.  My name is 0.  I can see (0 1).  My value for 'result' is (an unchanged result)
;; Hello, world.  My name is 1.  I can see (0 1).  My value for 'result' is (the second message from proc 0)
;;
;; D. Wingate 2/10/09
;; 


(import
 (ikarus)
 (church external parallel-comm)
 )

(define result "(an unchanged result)")

(parallel-initialize)

(if (= (get-my-name) 0)
    (begin
      ;; processor 0 sends to processor 1
      (par-msg-send 1 "(the first message from proc 0)")
      (par-msg-send 1 "(the second message from proc 0)")
      )
    (begin
      ;; processor 1 receives from processor 0
      (set! result (par-msg-recv 0))
      (set! result (par-msg-recv 0))
      )
    )

;; ----------------

(display "Hello, world.  My name is ")
(display (get-my-name))
(display ".  I can see ")
(display (get-neighbors))
(display ".  My value for 'result' is ")
(display result)
(newline)

;; ----------------

(parallel-finalize) ;; you should ALWAYS call this!
(exit)              ;; you should ALMOST ALWAYS call exit, too -- otherwise ikarus hangs in a repl!
