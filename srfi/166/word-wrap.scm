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

;; Takes a positive integer WIDTH and a vector on negative integers
;; LEN-VEC of length N, and returns a list RESULT of strictly
;; increasing integers between 0 and N - 1, which is determined as
;; follow: A text of N words indexed from 0 to N - 1, where the length
;; of the Ith word is the value of the Ith entry of LEN-VEC, is
;; wrapped into lines such that each line has at most length WIDTH and
;; such that the sum of the cubes of trailing spaces at each line is
;; minimized.  RESULT is the list of the indices of the last word for
;; each line.

(define (word-wrap width len-vec)
  (let* ((n (vector-length len-vec))
	 ;; (vector-ref total-cost I) is the total cost of all lines
	 ;; starting with a line starting with word I.
	 (total-cost (make-vector n +inf.0))
	 ;; (vector-ref last-in-line I) is the index of the last word
	 ;; in a line starting with word I.
	 (last-in-line (make-vector n (- n 1))))
    (do ((i (- n 1) (- i 1)))
	((< i 0))
      (do ((j i (+ j 1))
	   (curr-len (vector-ref len-vec i)
		     (+ curr-len (if (= j (- n 1))
				     0
				     (+ (vector-ref len-vec (+ j 1)) 1)))))
	  ((or (>= j n)
	       (> curr-len width)))
	(let ((cost (if (= j (- n 1))
			0
			(+ (expt (- width curr-len) 3)
			   (vector-ref total-cost (+ j 1))))))
	  (when (< cost (vector-ref total-cost i))
	    (vector-set! total-cost i cost)
	    (vector-set! last-in-line i j)))))
    (let loop ((i 0))
      (if (>= i n)
	  '()
	  (let ((j (vector-ref last-in-line i)))
	    (cons j (loop (+ j 1))))))))
