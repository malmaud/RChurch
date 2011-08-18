#!r6rs

(library (church utils utils)
         
         (export andmap
                 command-line-arguments
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
                 void)
         
         (import (church utils rnrs)
                 (only (ikarus) 
                       andmap
                       command-line-arguments
                       exact->inexact
                       format
                       gensym
                       inexact->exact
                       library-path
                       make-parameter
                       parameterize
                       modulo
                       ormap
                       pretty-print
                       random
                       void
                       )
                 (only (ikarus ipc)
                       system))

         ; FIXME: Replace this once ikarus has the
         ; appropriate timer functions.
         
         (define (timer thunk)
           (define val (thunk))
           (list val 0 0 0))
         

         (define (search-paths)
           (library-path))

)