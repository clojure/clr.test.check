;   Copyright (c) Rich Hickey, Reid Draper, and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test.check.util)

;; .NET Framework 4.x and .Net 6.0 lack Int64.LeadingZeroCount.

;; Straightforward translation of standard binary-search algorithm for counting leading zeros.
;; Refer to Hacker's Delight, Figure 5-8.  Adapted from 32 to 64 bits.
;; We use this version because it does not run into trouble with unsigned longs.
;; NOTE: For use with non-negative values only. (As is the case in our application.)

(defn leading-zero-count ^long [^long x]
  (loop [ x (unchecked-long x)
          n 64
          c (int 32)]
    (if (zero? c)
	   (- n x)
	   (let [y (bit-shift-right x c)]
	      (if (not (zero? y))
		     (recur y (- n c) (int (bit-shift-right c 1)))
			 (recur x n (int (bit-shift-right c 1))))))))
		
;; We use a macro to avoid detection at runtime.

(defmacro long-leading-zero-count [x]
  (if (>= (.Major System.Environment/Version) 7)
    `(System.Int64/LeadingZeroCount ~x)
    `(leading-zero-count ~x)))