#!r6rs

(library (church external r)
         (export r histogram plot barplot lineplot scatterplot)
         (import (church readable-scheme)
                 (church utils utils) 
                 (church utils rnrs)
                 (_srfi :69))

  ; TODO: Make it possible to pass tuples to r
  
  (define (values->r-vector values)
    (let* ((comma-aft-values (apply string-append (map (lambda (x) (format "~a," x)) values)))
           (comma-sep-values (substring comma-aft-values 0 (- (string-length comma-aft-values) 1))))
       (format "c(~a)" comma-sep-values)))
  
  (define (r r-cmd values)
    (let ((r-cmd-with-values (format r-cmd (values->r-vector values))))
      (system (format "echo \"~a\" | R --slave --save" r-cmd-with-values))))
 
  (define (histogram values title xlab ylab . args)
    (let ((parameters (if (not (null? args)) (string-append ", " (first args)) "")))
      (r (format "png('~a.png'); hist(~a, main='~a', col='lightblue', xlab='~a', ylab='~a'~a)" title "~a" title xlab ylab parameters) values)))

  (define (barplot values title xlab ylab . args)
    (let ((parameters (if (not (null? args)) (string-append ", " (first args)) "")))
      (r (format "png('~a.png'); barplot(~a, main='~a', col='lightblue', xlab='~a', ylab='~a'~a)" title "~a" title xlab ylab parameters) values)))
 
  (define (plot values title xlab ylab type . args)
    (let ((parameters (if (not (null? args)) (string-append ", " (first args)) "")))
      (r (format "png('~a.png'); plot(~a, main='~a', xlab='~a', ylab='~a', type='~a'~a)" title "~a" title xlab ylab type parameters) values)))
  
  (define (lineplot values title xlab ylab . args)
    (apply plot (append (list values title xlab ylab "l") args)))
  
  (define (scatterplot values title xlab ylab . args)
    (apply plot (append (list values title xlab ylab "p") args)))

)