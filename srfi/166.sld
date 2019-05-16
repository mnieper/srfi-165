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

(define-library (srfi 166)
  (export show displayed written written-shared written-simply escaped
	  maybe-escaped numeric numeric/comma numeric/si numeric/fitted nl fl
	  space-to tab-to nothing each each-in-list joined joined/prefix
	  joined/suffix joined/last joined/dot joined/range padded padded/right
	  padded/both trimmed trimmed/right trimmed/both trimmed/lazy
	  fitted fitted/right fitted/both
	  fn with with! forked call-with-output make-state-variable
	  port row col width output writer string-width pad-char ellipsis
	  radix precision decimal-sep decimal-align sign-rule comma-rule
	  comma-sep word-separator? ambiguous-is-wide?)
  (include-library-declarations "166/color.exports.scm")
  (include-library-declarations "166/columnar.exports.scm")
  (import (srfi 166 base))
  (import (srfi 166 columnar))
  (import (srfi 166 color)))
