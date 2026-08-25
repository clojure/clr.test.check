;   Copyright (c) Rich Hickey, Reid Draper, and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test.check.util)

;; .NET Framework 4.x lacks Int64.Leading


;; Straightforward translation of standard binary-search algorithm for counting leading zeros.
;; Refer to Hacker's Delight, Figure 5-6.  Adapted from 32 to 64 bits.

(defn leading-zero-count ^long [^long x]
  (let [v (unchecked-long x)]
    (if (zero? v)
      64
      (let [n (long 1)
            ;; step 1
            [n v] (if (zero? (bit-shift-right v 32))
                    [(+ n 32) (bit-shift-left v 32)]
                    [n v])
            ;; step 2
            [n v] (if (zero? (bit-shift-right v 48))
                    [(+ n 16) (bit-shift-left v 16)]
                    [n v])
            ;; step 3
            [n v] (if (zero? (bit-shift-right v 56))
                    [(+ n 8) (bit-shift-left v 8)]
                    [n v])
            ;; step 4
            [n v] (if (zero? (bit-shift-right v 60))
                    [(+ n 4) (bit-shift-left v 4)]
                    [n v])
            ;; step 5
            [n v] (if (zero? (bit-shift-right v 62))
                    [(+ n 2) (bit-shift-left v 2)]
                    [n v])]
        (- n (bit-shift-right v 63))))))
		
;; We use a macro to avoid detection at runtime.

(defmacro long-leading-zero-count [x]
  (if (>= (.Major System.Environment/Version) 7)
    `(System.Int64/LeadingZeroCount ~x)
    `(leading-zero-count ~x)))