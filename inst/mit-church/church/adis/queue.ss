#!r6rs

(library

 (church adis queue)

 (export make-queue
         queue-empty?
         queued?
         enqueue!
         enqueue-all!
         dequeue!)

 (import (church utils rnrs)
         (church readable-scheme))
 
 (define (make-queue . args)
   (let ([queue (cons '() '())])
     (when (not (null? args))
           (enqueue-all! queue (car args)))
     queue))

 (define (queue-empty? queue)
   (not (pair? (car queue))))

 (define (queued? queue item)
   (memq item (car queue)))

 (define (enqueue! queue object)
   (let ((next (cons object '())))
     (if (pair? (cdr queue))
         (set-mcdr! (cdr queue) next)
         (set-mcar! queue next))
     (set-mcdr! queue next)))

 (define (enqueue-all! queue objects)
   (map (lambda (obj) (enqueue! queue obj)) objects))

 (define (dequeue! queue)
   (let ((next (car queue)))
     (if (not (pair? next))
         (error "Attempt to dequeue from empty queue"))
     (if (pair? (cdr next))
         (set-mcar! queue (cdr next))
         (begin
           (set-mcar! queue '())
           (set-mcdr! queue '())))
     (car next)))

 )