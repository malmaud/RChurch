#!r6rs

;;
;; This library supports the parallel-comm library.  We serialize
;; objects before sending them across a communication channel.
;;
;; D. Wingate 2/10/09
;;

(library (church utils serializer)

  (export serialize deserialize)

  (import
   (church utils rnrs)
   (only (ikarus)
	 open-bytevector-output-port open-bytevector-input-port
	 fasl-write fasl-read ))

  (define (serialize x)
    (let-values ([(p e) (open-bytevector-output-port)])
                (fasl-write x p)
                (e)))

  (define (deserialize x)
    (fasl-read (open-bytevector-input-port x)))

)
