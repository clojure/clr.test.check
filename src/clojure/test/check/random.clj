;   Copyright (c) Rich Hickey, Reid Draper, and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:author "Gary Fredericks"
      :doc "Purely functional and splittable pseudo-random number generators."}
  clojure.test.check.random
  (:refer-clojure :exclude [unsigned-bit-shift-right]))


(defprotocol IRandom
  (rand-long [rng]
    "Returns a random long based on the given immutable RNG.

  Note: to maintain independence you should not call more than one
  function in the IRandom protocol with the same argument")
 (rand-double [rng]
    "Returns a random double between zero (inclusive) and 1.0 (exclusive) 
  based on the given immutable RNG.

  Note: to maintain independence you should not call split and rand-double
  with the same argument")
  (split [rng]
    "Returns two new RNGs [rng1 rng2], which should generate
  sufficiently independent random data.

  Note: to maintain independence you should not call more than one
  function in the IRandom protocol with the same argument")
  (split-n [rng n]
    "Returns a collection of `n` RNGs, which should generate
  sufficiently independent random data.

  Note: to maintain independence you should not call more than one
  function in the IRandom protocol with the same argument"))


;; Immutable version of Java 8's java.util.SplittableRandom
;;
;; Meant to give the same results as similar uses of
;; java.util.SplittableRandom, in particular:
;;
;; (= (-> (make-java-util-splittable-random 42)
;;        (rand-long))
;;    (.nextLong (SplittableRandom. 42)))
;;
;; (= (-> (make-java-util-splittable-random 42)
;;        (split)
;;        (first)
;;        (rand-long))
;;    (.nextLong (doto (SplittableRandom. 42)
;;                     (.split))))
;;
;; (= (-> (make-java-util-splittable-random 42)
;;        (split)
;;        (second)
;;        (rand-long))
;;    (.nextLong (.split (SplittableRandom. 42))))
;;
;; Also see the spec that checks this equivalency.


;; backwards compatibility for clojure 1.5
(def ^:private old-clojure?
  (not (resolve 'clojure.core/unsigned-bit-shift-right)))
(defmacro ^:private unsigned-bit-shift-right
  [x n]
  {:pre [(<= 1 n 63)]}
  (if old-clojure?
    (let [mask (-> Int64/MinValue                                                 ;;; Long/MIN_VALUE
                   (bit-shift-right (dec n))
                   ((fn [^long x] (bit-and-not x x))))]                                                             ;;;  (bit-not)  -- temporary fix, until I get bit-not working in clojure.core -- sigh
      `(-> ~x
           (bit-shift-right ~n)
           (bit-and ~mask)))
    `(clojure.core/unsigned-bit-shift-right ~x ~n)))

(defmacro ^:private longify
  "Macro for writing arbitrary longs in the java 0x syntax. E.g.
  0x9e3779b97f4a7c15 (which is read as a bigint because it's out
  of range) becomes -7046029254386353131."
  [num]
  (if (> num Int64/MinValue)                                                 ;;; Long/MIN_VALUE
    (-> num
        (- 18446744073709551616N)
        (long)
        (bit-or -9223372036854775808))
    num))

(set! *unchecked-math* :warn-on-boxed)

(defmacro ^:private bxoubsr
  "Performs (-> x (unsigned-bit-shift-right n) (bit-xor x))."
  [x n]
  (vary-meta
   `(let [x# ~x]
      (-> x# (unsigned-bit-shift-right ~n) (bit-xor x#)))
   assoc :tag 'long))

(defmacro ^:private mix-64
  [n]
  `(-> ~n
       (bxoubsr 30)
       (* (longify 0xbf58476d1ce4e5b9))
       (bxoubsr 27)
       (* (longify 0x94d049bb133111eb))
       (bxoubsr 31)))

;;; DM: Added -- if I ever add a bitCount for Int64 in the base Clojure code, we can get rid of this 

(defn long-bit-count ^long [^long i]
   (let [i  (- i ^long (bit-and (unsigned-bit-shift-right i 1) 0x5555555555555555))
		 i	(+ (bit-and i 0x3333333333333333) (bit-and (unsigned-bit-shift-right i 2) 0x3333333333333333))
		 i  (bit-and (+ i (unsigned-bit-shift-right i 4)) 0x0f0f0f0f0f0f0f0f)
		 i  (+ i (unsigned-bit-shift-right i 8))
		 i  (+ i (unsigned-bit-shift-right i 16))
		 i  (+ i (unsigned-bit-shift-right i 32))]
	(long (bit-and (int i) (int 0x7f)))))

;     public static int bitCount(long i) {
;        // HD, Figure 5-14
;        i = i - ((i >>> 1) & 0x5555555555555555L);
;        i = (i & 0x3333333333333333L) + ((i >>> 2) & 0x3333333333333333L);
;        i = (i + (i >>> 4)) & 0x0f0f0f0f0f0f0f0fL;
;        i = i + (i >>> 8);
;        i = i + (i >>> 16);
;        i = i + (i >>> 32);
;        return (int)i & 0x7f;
;     }
;
;;;
	   
(defmacro ^:private mix-gamma
  [n]
  `(-> ~n
       (bxoubsr 33)
       (* (longify 0xff51afd7ed558ccd))
       (bxoubsr 33)
       (* (longify 0xc4ceb9fe1a85ec53))
       (bxoubsr 33)
       (bit-or 1)
       (as-> z#
             (cond-> z#
                     (> 24 (-> z#
                               (bxoubsr 1)
                               long-bit-count))                            ;;; (Long/bitCount)
                     (bit-xor (longify 0xaaaaaaaaaaaaaaaa))))))

(def ^{:private true :const true} double-unit (/ 1.0 (double (bit-set 0 53))))
;; Java: 0x1.0p-53 or (1.0 / (1L << 53))

(deftype JavaUtilSplittableRandom [^long gamma ^long state]
  IRandom
  (rand-long [_]
    (-> state (+ gamma) (mix-64)))
  (rand-double [this]
    (* double-unit (unsigned-bit-shift-right (long (rand-long this)) 11)))	
  (split [this]
    (let [state' (+ gamma state)
          state'' (+ gamma state')
          gamma' (mix-gamma state'')]
      [(JavaUtilSplittableRandom. gamma state'')
       (JavaUtilSplittableRandom. gamma' (mix-64 state'))]))
  (split-n [this n]
    ;; imitates a particular series of 2-way splits, but avoids the
    ;; intermediate allocation. See the `split-n-spec` for a test of
    ;; the equivalence to 2-way splits.
    (let [n (long n)]
      (case n
        0 []
        1 [this]
        (let [n-dec (dec n)]
          (loop [state state
                 ret (transient [])]
            (if (= n-dec (count ret))
              (-> ret
                  (conj! (JavaUtilSplittableRandom. gamma state))
                  (persistent!))
              (let [state' (+ gamma state)
                    state'' (+ gamma state')
                    gamma' (mix-gamma state'')
                    new-rng (JavaUtilSplittableRandom. gamma' (mix-64 state'))]
                (recur state'' (conj! ret new-rng))))))))))

(def ^:private golden-gamma
  (longify 0x9e3779b97f4a7c15))

(defn make-java-util-splittable-random
  [^long seed]
  (JavaUtilSplittableRandom. golden-gamma seed))

;; some global state to make sure that seedless calls to make-random
;; return independent results

;;; DM: Added
;;; Unfortunately, this solution requires ThreadLocal values.
;;; We do not have an internal way to create static fields in a gen-class so that we could use ThreadStatic.
;;; And ThreadStatic in CLR was only introduced in 4.0.
;;; So, we use the equivalent of the old way in 3.5 and use the new way in 4.0 only.

;;; This duplicates compile-if from clojure/core/reducers.

(defmacro ^:private compile-if
  "Evaluate `exp` and if it returns logical true and doesn't error, expand to
  `then`.  Else expand to `else`.

  (compile-if (Class/forName \"java.util.concurrent.ForkJoinTask\")
    (do-cool-stuff-with-fork-join)
    (fall-back-to-executor-services))"
  [exp then else]
  (if (try (eval exp)
           (catch Exception _ false))                      ;;; Throwable
    `(do ~then)
    `(do ~else)))
	
	
;;;(def ^:private next-rng
;;;  "Returns a random-number generator. Successive calls should return
;;;  independent results."
;;;  (let [a (atom (make-java-util-splittable-random (Environment/TickCount)))                    ;;; (System/currentTimeMillis)
;;;
;;;        thread-local
;;;        (proxy [ThreadLocal] []
;;;          (initialValue []
;;;            (first (split (swap! a #(second (split %)))))))]
;;;    (fn []
;;;      (let [rng (.get thread-local)
;;;            [rng1 rng2] (split rng)]
;;;        (.set thread-local rng2)
;;;        rng1))))
  
(compile-if 
  (Type/GetType "System.Threading.ThreadLocal`1")
  (do 		
	(def ^:private next-rng
	  "Returns a random-number generator. Successive calls should return
	  independent results."
	  (let [a (atom (make-java-util-splittable-random (Environment/TickCount)))			   
			init-delegate (sys-func [Object] [] (first (split (swap! a #(second (split %))))))
			thread-local (|System.Threading.ThreadLocal`1[System.Object]|. ^|System.Func`1[System.Object]| init-delegate)]
		(fn []
		  (let [rng (.Value thread-local)
				[rng1 rng2] (split rng)]
			(.set_Value thread-local rng2)
			rng1)))))
  (do 
    (def ^:private next-rng 
		(fn [] (make-java-util-splittable-random (Environment/TickCount))))))  
  
(defn make-random
  "Given an optional Long seed, returns an object that satisfies the
  IRandom protocol."
  ([] (next-rng))
  ([seed] (make-java-util-splittable-random seed)))