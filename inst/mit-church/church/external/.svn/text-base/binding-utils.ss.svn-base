#!r6rs

;;
;; A bunch of utilities designed to aid in creating FFIs.
;;
;; D. Wingate 2/6/09
;;

(library (church external binding-utils)

	 (export list->cblock cblock->list
		 vector->cblock cblock->vector
		 string->cblock
		 bytevector->cblock cblock->bytevector
		 get-ctype-size ptr-deref)

	 (import
	   (_srfi :43)
	   (except (rnrs) vector-map vector-for-each vector-fill! vector->list list->vector) ;(church utils rnrs)
	   (only (ikarus) add1 sub1)
	   (ikarus foreign))

         ;; =============================================================
         ;;
         ;; Utilities
         ;;

         (define (get-setter type)
           (cond
             ( (equal? type 'char) pointer-set-c-char! )
             ( (equal? type 'signed-char) pointer-set-c-char! )
             ( (equal? type 'unsigned-char) pointer-set-c-char! )
             ( (equal? type 'short) pointer-set-c-short! )
             ( (equal? type 'signed-short) pointer-set-c-short! )
             ( (equal? type 'unsigned-short) pointer-set-c-short! )
             ( (equal? type 'int) pointer-set-c-int! )
             ( (equal? type 'signed-int) pointer-set-c-int! )
             ( (equal? type 'unsigned-int) pointer-set-c-int! )
             ( (equal? type 'long) pointer-set-c-long! )
             ( (equal? type 'signed-long) pointer-set-c-long! )
             ( (equal? type 'unsigned-long) pointer-set-c-long! )
             ( (equal? type 'float) pointer-set-c-float! )
             ( (equal? type 'double) pointer-set-c-double! )
             ( (equal? type 'pointer) pointer-set-c-pointer! )
             ))
         
         (define (get-ref type)
           (cond
             ( (equal? type 'signed-char) pointer-ref-c-signed-char )
             ( (equal? type 'unsigned-char) pointer-ref-c-unsigned-char )
             ( (equal? type 'signed-short) pointer-ref-c-signed-short )
             ( (equal? type 'usigned-short) pointer-ref-c-unsigned-short )
             ( (equal? type 'signed-int) pointer-ref-c-signed-int )
             ( (equal? type 'unsigned-int) pointer-ref-c-unsigned-int )
             ( (equal? type 'signed-long) pointer-ref-c-signed-long )
             ( (equal? type 'unsigned-long) pointer-ref-c-unsigned-long )
             ( (equal? type 'float) pointer-ref-c-float )
             ( (equal? type 'double) pointer-ref-c-double )
             ( (equal? type 'pointer) pointer-ref-c-pointer )
             ))
         
         (define (get-ctype-size type)
           (cond
             ( (equal? type 'char) 1 )
             ( (equal? type 'signed-char) 1 )
             ( (equal? type 'unsigned-char) 1 )
             ( (equal? type 'short) 2 )
             ( (equal? type 'signed-short) 2 )
             ( (equal? type 'unsigned-short) 2 )
             ( (equal? type 'int) 4 )
             ( (equal? type 'signed-int) 4 )
             ( (equal? type 'unsigned-int) 4 )
             ( (equal? type 'long) (pointer-size) )
             ( (equal? type 'signed-long) (pointer-size) )
             ( (equal? type 'unsigned-long) (pointer-size) )
             ( (equal? type 'float) 4 )
             ( (equal? type 'double) 8 )
             ( (equal? type 'pointer) (pointer-size) )
             ))
         
         ;; we have to do manual pointer arithmetic
         (define (get-offset type cnt)
           (* (get-ctype-size type) cnt))
         
         (define (ptr-ref pointer type offset)
           ((get-ref type) pointer (get-offset type offset)))
         
         (define (ptr-set! pointer type offset value)
           ((get-setter type) pointer (get-offset type offset) value ))
           
         ;; derefence a pointer.
         (define (ptr-deref p)
           (pointer-ref-c-pointer p 0))

         
         ; From the PLT Scheme source code:
         
         (define (list->cblock l type)
           (if (null? l)
               #f ; null => NULL
               (let ([cblock (malloc (* (length l) (get-ctype-size type)))])
                 (let loop ([l l] [i 0])
                   (unless (null? l)
                     (ptr-set! cblock type i (car l))
                     (loop (cdr l) (add1 i))))
                 cblock)))
         
         (define (cblock->list cblock type len)
           (cond [(zero? len) '()]
                 [(pointer? cblock)
                  (let loop ([i (sub1 len)] [r '()])
                    (if (< i 0)
                        r
                        (loop (sub1 i) (cons (ptr-ref cblock type i) r))))]
                 [else (error 'cblock->list
                              "expecting a non-void pointer, got ~s" cblock)]))
         
         ;; Converting Scheme vectors to/from C vectors
         (define (vector->cblock v type)
           (let ([len (vector-length v)])
             (if (zero? len)
                 #f ; #() => NULL
                 (let ([cblock (malloc (* len (get-ctype-size type)))])
                   (let loop ([i 0])
                     (when (< i len)
                       (ptr-set! cblock type i (vector-ref v i))
                       (loop (add1 i))))
                   cblock))))
         
         (define (cblock->vector cblock type len)
           (cond [(zero? len) '#()]
                 [(pointer? cblock)
                  (let ([v (make-vector len)])
                    (let loop ([i (sub1 len)])
                      (unless (< i 0)
                        (vector-set! v i (ptr-ref cblock type i))
                        (loop (sub1 i))))
                    v)]
                 [else (error 'cblock->vector
                              "expecting a non-void pointer, got ~s" cblock)]))

         ;; Copy a string into a pointer
         (define (string->cblock str)
           (let ([len (string-length str)])
	     (let ([cblock (malloc (* len 1))])
	       (let loop ([i 0])
		 (when (< i len)
		       (pointer-set-c-char! cblock i (char->integer (string-ref str i)) )
		       (loop (add1 i))))
	       (pointer-set-c-char! cblock len 0) ; null-terminate the string
	     cblock)))


         ;; Copy a bytevector into a pointer
         (define (bytevector->cblock bvec)
           (let ([len (bytevector-length bvec)])
	     (let ([cblock (malloc len)])
	       (let loop ([i 0])
		 (when (< i len)
		       (pointer-set-c-char! cblock i (bytevector-s8-ref bvec i) )
		       (loop (add1 i))))
	     cblock)))

         (define (cblock->bytevector cblock len)
           (cond [(zero? len) '#()]
                 [(pointer? cblock)
                  (let ([v (make-bytevector len)])
                    (let loop ([i (sub1 len)])
                      (unless (< i 0)
                        (bytevector-s8-set! v i (pointer-ref-c-signed-char cblock i))
                        (loop (sub1 i))))
                    v)]
                 [else (error 'cblock->vector
                              "expecting a non-void pointer, got ~s" cblock)]))

)
