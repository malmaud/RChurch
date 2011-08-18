;;; Copyright (c) 2008 Derick Eddington
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; Except as contained in this notice, the name(s) of the above copyright
;;; holders shall not be used in advertising or otherwise to promote the sale,
;;; use or other dealings in this Software without prior written authorization.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

#!r6rs
(library (_srfi private implementation-features)
  (export
    OS-features
    implementation-features)
  (import
    (rnrs)
    (only (scheme base) system-type))
  
  (define OS-features
    (let*          ;; TODO? more
        ([alist '(["unix" posix]
                  ["linux" linux posix]
                  ["macosx" mac-os-x darwin posix]
                  ["solaris" solaris posix]
                  ["gnu" gnu]
                  ["windows" windows])]
         [st (string-append (symbol->string (system-type 'os))
                            " " (system-type 'machine))]
         [st-len (string-length st)]
         [contains? 
          (lambda (str)
            (define str-len (string-length str))
            (and (>= st-len str-len)
                 (let loop ([i 0])
                   (and (<= (+ i str-len) st-len)
                        (or (string-ci=? str (substring st i (+ i str-len)))
                            (loop (+ 1 i)))))))]
         [features 
          (apply append
                 (map cdr (filter (lambda (x) (contains? (car x))) 
                                  alist)))])          
      (unless (positive? (length features))
        (error "(library (_srfi private implementation-features))"
               "Unknown system-type. Please report your's to maintainer person."
               st))
      features))
  
  (define implementation-features
    '(mzscheme))
)
