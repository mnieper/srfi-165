;; Copyright (C) Marc Nieper-Wißkirchen (2019).  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-syntax cond
  (syntax-rules (=> else)
    ((cond (else expr1 expr2 ...))
     (begin expr1 expr2 ...))
    ((cond (test => receiver) clause ...)
     (let ((t test))
       (%cond t (receiver t) clause ...)))
    ((cond (generator guard => receiver) clause ...)
     (call-with-values (lambda () generator)
       (lambda t
         (%cond (apply guard t)
		(apply receiver t)
		clause ...))))
    ((cond (test) clause ...)
     (let ((t test))
       (%cond t t clause ...)))
    ((cond (test body1 body2 ...) clause ...)
     (%cond test
            (begin body1 body2 ...)
            clause ...))))

(define-syntax %cond
  (syntax-rules ()
    ((_ test consequent)
     (if test consequent))
    ((_ test consequent clause ...)
     (if test consequent (cond clause ...)))))
