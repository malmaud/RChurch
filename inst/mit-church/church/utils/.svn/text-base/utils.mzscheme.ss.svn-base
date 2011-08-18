#!r6rs

(library (church utils utils)
         
         (export andmap
                 exact->inexact
                 format
                 gensym
                 inexact->exact
                 make-parameter
                 parameterize
                 modulo
                 ormap
                 pretty-print
                 random
                 search-paths
                 system
                 timer
                 void
                 (rename (current-command-line-arguments command-line-arguments)))
         
         (import (church utils rnrs)
                 (only (mzscheme)
                       andmap
                       exact->inexact
                       gensym
                       inexact->exact
                       make-parameter
                       parameterize
                       modulo
                       ormap
                       random
                       void)
                 (only (scheme mpair)
                       list->mlist)
                 (only (scheme system)
                       system)
                 (only (scheme)
                       current-command-line-arguments
                       current-gc-milliseconds
                       current-library-collection-paths
                       current-milliseconds
                       current-process-milliseconds
                       format
                       path->string
                       pretty-print))

         (define (timer thunk)
           (define init-cpu-time (current-process-milliseconds))
           (define init-real-time (current-milliseconds))
           (define init-gc-time (current-gc-milliseconds))
           (let ((ret (thunk)))
             (list
                   ret
                   (- (current-process-milliseconds) init-cpu-time) ;; cpu time
                   (- (current-milliseconds) init-real-time) ;; total time
                   (- (current-gc-milliseconds) init-gc-time) ;; garbage collecting time
                   )))
		
		     (define (search-paths)
		       (map path->string 
		            (list->mlist (current-library-collection-paths))))

)