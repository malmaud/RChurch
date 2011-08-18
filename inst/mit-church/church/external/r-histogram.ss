#!r6rs

(library

 (church external r-histogram)

 (export histogram hist mhist truehist)

 (import (church utils rnrs)
         (church external r-project)
         (church readable-scheme)
         (_srfi :1)
         (_srfi :69)
         (only (ikarus) inexact->exact exact->inexact)
         (only (ikarus) format list-sort))

 (define (canonical-order lst)
   (list-sort (lambda (a b) (string<? (write-to-string a) (write-to-string b)))
              lst))

 (define (histogram values filetype title) ; xlab ylab . args)
   (define next-int 0)
   (define val-to-int (make-hash-table))
   (define labels '())
   (define int-values (map (lambda (val) (hash-table-ref val-to-int
                                                    val
                                                    (lambda () (begin (hash-table-set! val-to-int val next-int)
                                                                 (set! labels (append labels (list val)))
                                                                 (set! next-int (+ 1 next-int))
                                                                 (hash-table-ref val-to-int val)))))
                            values))
   (define r-port (open-r-port))
   (begin
     (r r-port filetype title)
     (r r-port "hist" int-values "col=" "lightblue" "axes=" "False" "ann=" "False")
     ;(r r-port "axis" 1 "at=" (iota (length labels)) "lab=" (list "\"true\"" "\"false\""))
     (r r-port "axis" 1 "at=" (iota (length labels)) "lab=" (map (lambda (x) (format "\"~a\"" x)) labels))
     (r r-port "axis" 2)
     (close-r-port r-port)))

 ;;values is a list of lists. make counts separately for each list, but using the same label sets....
 (define (count-values values . keys)
   (define counts (make-list (length values) '()))
   (define labels '())
   (let loop ((all-values (if (null? keys) (canonical-order (apply append values))
                              (first keys)))
              (values values))
     (if (null? all-values)
         (pair labels counts)
         (let ((key (first all-values)))
           (set! labels (append labels (list key)))
           (set! counts
                 (map (lambda (vals cnts)
                        (append cnts (list (count (lambda (x) (equal? x key)) vals))))
                      values counts))
           (loop (delete key all-values)
                 (map (lambda (vals) (delete key vals)) values))))))
 
;;  (define (count-values values)
;;   (define count-table (make-hash-table))
;;   (define counts '())
;;   (define labels '())
;;   (begin
    
;;     ;build the count table
;;     (for-each 
;;      (lambda (val)
;;        (let* ((last-total (hash-table-ref count-table val (lambda () 0)))
;;               (new-total (+ last-total 1)))
;;          (hash-table-set!  count-table val new-total)))
;;      values)
    
;;     ;build the label and count lists
;;     (for-each 
;;      (lambda (key)
;;        (let ((count (hash-table-ref count-table key (lambda () 0))))
;;          (begin
;;            (set! labels (append labels (list key)))
;;            (set! counts (append counts (list count))))))
;;      (hash-table-keys count-table))
    
 ;;     (cons labels counts)))

 (define sane-chars
   (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_-."))

 (define (contains lst elt)
   (any (lambda (x) (equal? x elt)) lst))
 
 (define (sanitize str)
   (list->string
    (filter (lambda (char) (contains sane-chars char))
            (string->list str))))

 (define (filename->type fn)
   (let ([ext-length (list-index (lambda (x) (equal? x #\.))
                                 (reverse (string->list fn)))])
     (if (number? ext-length)
         (list->string (take-right (string->list fn) ext-length))
         false)))
               
 (define (title->filename title default-ext)
   (let ([sanitized-title (sanitize title)])
     (if (false? (filename->type sanitized-title))
         (string-append sanitized-title "." default-ext)
         sanitized-title)))
 
 ;calculate the percentages and use a barplot instead of a histogram
 (define (mhist values title . keys)
   (let* ([res (if (null? keys)
                   (count-values values)
                   (count-values values (first keys)))]
          [labels (first res)]
          [counts (rest res)]
          [totals (map length values)]
          [normalized-counts (map (lambda (cnts tot)
                                    (map (lambda (c) (exact->inexact (/ c tot))) cnts))
                                  counts totals)]
          [r-port (open-r-port)]
          [filename (title->filename title "png")]
          [file-ext (filename->type filename)]) ;; assumes that specified filetype is in set of types valid for R
     (r r-port file-ext filename)
     (r r-port "par" "mar=" (list 10 4 4 4))
     (r r-port "m <- matrix" (apply append normalized-counts) (length normalized-counts) (length (first normalized-counts)) "byrow=" 'T)
     (r r-port "colors <- c" "lightblue" "lightgreen" "lightcoral");;"mistyrose" "lightcyan" "lavender")
     (r r-port
        "barplot" 'm
        "beside=" 'T
        "main=" title
        "col=" 'colors
        "names.arg=" (map write-to-string labels)
        "ylim=" (list 0 1)
        "las=" 2) ;;this rotates the labels.
     (close-r-port r-port)))

 (define (hist values title) (mhist (list values) title))
 
 (define (set-precision n)
   (let* ((max-decimals 3)
          (k (expt 10.0 max-decimals)))
     (if (number? n)
         (if (integer? n)
             (inexact->exact n)
             (/ (round (* n k))
                k))
         n)))

 
 ;build a true histogram with continuous values
 (define (truehist values title . num-bins)
   (define r-port (open-r-port))
   (define new-values (map set-precision values))
   (begin
     (display new-values)
     (r r-port "library" 'MASS)
     (r r-port "png" (title->filename title "png"))
     (apply r r-port "truehist" new-values "main=" title "col=" "lightblue" "xlab=" "values"
            (if (null? num-bins)
                '()
                (list "nbins=" (first num-bins))
                ;; (list "h=" (/ (- (apply max new-values) (apply min new-values))
                ;;               (first num-bins)))
                ))
     (close-r-port r-port)))
 
 
 )
         ; "pdf('~a.pdf'); hist(~a, main='~a', col='lightblue', xlab='~a', ylab='~a'~a)" title "~a" title xlab ylab parameters) values)))



;; (define r-port (open-r-port))
;; (r r-port "plot" (list 1 2 3)
;;                  (map log (list 1 2 3))
;;                  "xlab=" "x"
;;                  "ylab=" "y"
;;                  "main=" "title" 
;;                  "type=" "l")
;; (close-r-port r-port)

;;axis(1, at=1:5, lab=c("Mon","Tue","Wed","Thu","Fri"))
;
;# Make y axis with horizontal labels that display ticks at 
;# every 4 marks. 4*0:g_range[2] is equivalent to c(0,4,8,12).
;axis(2, las=1, at=4*0:g_range[2])