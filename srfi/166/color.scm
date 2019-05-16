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

(define color (make-state-variable 'color #f #f))
(define underline? (make-state-variable 'underline? #f #f))
(define bold? (make-state-variable 'bold? #f #f))

(define ansi-escape
  (fn (color
       underline?
       bold?)
    (each #\x1b #\[
	  (joined displayed
		  (cons "0"
			(append (or color '())
				(if bold? (list "1") '())
				(if underline? (list "4") '())))
		  ";")
	  #\m)))

(define (colored new-color fmt)
  (each (with ((color (list new-color)))
	  (each ansi-escape fmt))
	ansi-escape))

(define (as-red . fmt*) (colored "31" (each-in-list fmt*)))
(define (as-blue . fmt*) (colored "34" (each-in-list fmt*)))
(define (as-green . fmt*) (colored "32" (each-in-list fmt*)))
(define (as-cyan . fmt*) (colored "36" (each-in-list fmt*)))
(define (as-yellow . fmt*) (colored "33" (each-in-list fmt*)))
(define (as-magenta . fmt*) (colored "35" (each-in-list fmt*)))
(define (as-white . fmt*) (colored "37" (each-in-list fmt*)))
(define (as-black . fmt*) (colored "30" (each-in-list fmt*)))

(define (as-bold . fmt*)
  (each (with ((bold? #t))
	  (each-in-list (cons ansi-escape fmt*)))
	ansi-escape))

(define (as-underline . fmt*)
  (each (with ((underline? #t))
	  (each-in-list (cons ansi-escape fmt*)))
	ansi-escape))

(define (as-color r g b . fmt*)
  (each (with ((color (list "38" "5" (number->string (+ 16
							(* 36 r)
							(* 6 g)
							b)))))
	  (each-in-list (cons ansi-escape fmt*)))
	ansi-escape))
