#!r6rs

(library (church external include-paths)
         
         (export open-included-file)
         
         (import (church utils rnrs)
                 (rnrs files)
                 (church readable-scheme)
                 (church utils utils))

         (define include-paths
           (append 
             (list "./" "include/")
             (map
                (lambda (search-path) (string-append search-path "/include/"))
                (search-paths))))

         ; goes through a list of library paths and opens
         ; the first one it finds
         (define (open-included-file filename)
           (define (loop-through-paths path-list)
             (if (null? path-list)
                 (error "open-included-file" (string-append "File " filename " not found on Church include paths."))
                 (if (file-exists? (string-append (first path-list) filename))
                     (open-input-file (string-append (first path-list) filename))
                     (loop-through-paths (rest path-list)))))
           (loop-through-paths include-paths))

)