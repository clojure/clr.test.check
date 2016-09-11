;   Copyright (c) Rich Hickey, Reid Draper, and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.test.check.test
  #?(:clj (:use clojure.test)  :cljr (:use clojure.test))                                                 ;;; Added :cljr clause
  (:require #?(:cljs
                [cljs.test :as test :refer-macros [deftest testing is]])
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen #?@(:cljs [:include-macros true])]
            [clojure.test.check.properties :as prop #?@(:cljs [:include-macros true])]
            [clojure.test.check.rose-tree :as rose]
            [clojure.test.check.random :as random]
            [clojure.test.check.clojure-test :as ct #?(:default :refer :cljs :refer-macros) (defspec)]    ;;; Changed :clj to :default
            #?(:default  [clojure.edn :as edn]                                                            ;;; Changed :clj to :default
               :cljs [cljs.reader :as edn])))

(def gen-seed
  (let [gen-int (gen/choose 0 0x100000000)]
    (gen/fmap (fn [[s1 s2]]
                (bit-or s1 (bit-shift-left s2 32)))
              (gen/tuple gen-int gen-int))))

(deftest generators-are-generators
  (testing "generator? returns true when called with a generator"
           (is (gen/generator? gen/int))
           (is (gen/generator? (gen/vector gen/int)))
           (is (gen/generator? (gen/return 5)))))

(deftest values-are-not-generators
  (testing "generator? returns false when called with a value"
           (is (not (gen/generator? 5)))
           (is (not (gen/generator? int)))
           (is (not (gen/generator? [1 2 3])))))

;; plus and 0 form a monoid
;; ---------------------------------------------------------------------------

(defn passes-monoid-properties
  [a b c]
  (and (= (+ 0 a) a)
       (= (+ a 0) a)
       (= (+ a (+ b c)) (+ (+ a b) c))))

(deftest plus-and-0-are-a-monoid
  (testing "+ and 0 form a monoid"
           (is (let [p (prop/for-all* [gen/int gen/int gen/int] passes-monoid-properties)]
                 (:result
                   (tc/quick-check 1000 p)))))
  #?(:cljr                                                                                                 ;;; changed :clj to :cljr
    (testing "with ratios as well"
           (is (let [p (prop/for-all* [gen/ratio gen/ratio gen/ratio] passes-monoid-properties)]
                 (:result
                   (tc/quick-check 1000 p)))))

    ;; NOTE: no ratios in ClojureScript - David
    ))

;; reverse
;; ---------------------------------------------------------------------------

(defn reverse-equal?-helper
  [l]
  (let [r (vec (reverse l))]
    (and (= (count l) (count r))
         (= (seq l) (rseq r)))))

(deftest reverse-equal?
  (testing "For all vectors L, reverse(reverse(L)) == L"
           (is (let [p (prop/for-all* [(gen/vector gen/int)] reverse-equal?-helper)]
                 (:result (tc/quick-check 1000 p))))))

;; failing reverse
;; ---------------------------------------------------------------------------

(deftest bad-reverse-test
  (testing "For all vectors L, L == reverse(L). Not true"
           (is (false?
                 (let [p (prop/for-all* [(gen/vector gen/int)] #(= (reverse %) %))]
                   (:result (tc/quick-check 1000 p)))))))

;; failing element remove
;; ---------------------------------------------------------------------------

(defn first-is-gone
  [l]
  (not (some #{(first l)} (vec (rest l)))))

(deftest bad-remove
  (testing "For all vectors L, if we remove the first element E, E should not
           longer be in the list. (This is a false assumption)"
           (is (false?
                 (let [p (prop/for-all* [(gen/vector gen/int)] first-is-gone)]
                   (:result (tc/quick-check 1000 p)))))))

;; exceptions shrink and return as result
;; ---------------------------------------------------------------------------

(def exception (#?(:default Exception. :cljs js/Error.) "I get caught"))                  ;;; Changed :clj to :default

(defn exception-thrower
  [& args]
  (throw exception))

(deftest exceptions-are-caught
  (testing "Exceptions during testing are caught. They're also shrunk as long
           as they continue to throw."
           (is (= [exception [0]]
                  (let [result
                        (tc/quick-check
                          1000 (prop/for-all* [gen/int] exception-thrower))]
                    [(:result result) (get-in result [:shrunk :smallest])])))))

;; Count and concat work as expected
;; ---------------------------------------------------------------------------

(defn concat-counts-correct
  [a b]
  (= (count (concat a b))
     (+ (count a) (count b))))

(deftest count-and-concat
  (testing "For all vectors A and B:
           length(A + B) == length(A) + length(B)"
           (is (:result
                 (let [p (prop/for-all* [(gen/vector gen/int)
                                        (gen/vector gen/int)] concat-counts-correct)]
                   (tc/quick-check 1000 p))))))

;; Interpose (Count)
;; ---------------------------------------------------------------------------

(defn interpose-twice-the-length ;; (or one less)
  [v]
  (let [interpose-count (count (interpose :i v))
        original-count (count v)]
    (or
      (= (* 2 original-count) interpose-count)
      (= (dec (* 2 original-count)) interpose-count))))


(deftest interpose-creates-sequence-twice-the-length
  (testing
    "Interposing a collection with a value makes its count
    twice the original collection, or ones less."
    (is (:result
          (tc/quick-check 1000 (prop/for-all [v (gen/vector gen/int)] (interpose-twice-the-length v)))))))

;; Lists and vectors are equivalent with seq abstraction
;; ---------------------------------------------------------------------------

(defn list-vector-round-trip-equiv
  [a]
  ;; NOTE: can't use `(into '() ...)` here because that
  ;; puts the list in reverse order. clojure.test.check found that bug
  ;; pretty quickly...
  (= a (apply list (vec a))))

(deftest list-and-vector-round-trip
  (testing
    ""
    (is (:result
          (tc/quick-check
            1000 (prop/for-all*
                   [(gen/list gen/int)] list-vector-round-trip-equiv))))))

;; keyword->string->keyword roundtrip
;; ---------------------------------------------------------------------------

(def keyword->string->keyword
  (comp keyword name))

(defn keyword-string-roundtrip-equiv
  [k]
  (= k (keyword->string->keyword k)))

;; NOTE cljs: this is one of the slowest due to how keywords are constructed
;; drop N to 100 - David
(deftest keyword-string-roundtrip
  (testing
    "For all keywords, turning them into a string and back is equivalent
    to the original string (save for the `:` bit)"
    (is (:result
          (let [n #?(:default 1000 :cljs 100)]                                    ;;; Change :clj to :default
            (tc/quick-check n (prop/for-all*
                                [gen/keyword] keyword-string-roundtrip-equiv)
                            :max-size 25))))))

;; Boolean and/or
;; ---------------------------------------------------------------------------

(deftest boolean-or
  (testing
    "`or` with true and anything else should be true"
    (is (:result (tc/quick-check
                   1000 (prop/for-all*
                          [gen/boolean] #(or % true)))))))

(deftest boolean-and
  (testing
    "`and` with false and anything else should be false"
    (is (:result (tc/quick-check
                   1000 (prop/for-all*
                          [gen/boolean] #(not (and % false))))))))

;; Sorting
;; ---------------------------------------------------------------------------

(defn elements-are-in-order-after-sorting
  [v]
  (every? identity (map <= (partition 2 1 (sort v)))))

(deftest sorting
  (testing
    "For all vectors V, sorted(V) should have the elements in order"
    (is (:result
          (tc/quick-check
            1000
            (prop/for-all*
              [(gen/vector gen/int)] elements-are-in-order-after-sorting))))))

;; Constant generators
;; ---------------------------------------------------------------------------

;; A constant generator always returns its created value
(defspec constant-generators 100
  (prop/for-all [a (gen/return 42)]
                (print "")
                (= a 42)))

(deftest constant-generators-dont-shrink
  (testing
    "Generators created with `gen/return` should not shrink"
    (is (= [42]
           (let [result (tc/quick-check 100
                                        (prop/for-all
                                          [a (gen/return 42)]
                                          false))]
             (-> result :shrunk :smallest))))))

;; Tests are deterministic
;; ---------------------------------------------------------------------------

(defn vector-elements-are-unique
  [v]
  (== (count v) (count (distinct v))))

(defn unique-test
  [seed]
  (tc/quick-check 1000
                  (prop/for-all*
                    [(gen/vector gen/int)] vector-elements-are-unique)
                  :seed seed))

(defn equiv-runs
  [seed]
  (= (unique-test seed) (unique-test seed)))

(deftest tests-are-deterministic
  (testing "If two runs are started with the same seed, they should
           return the same results."
           (is (:result
                 (tc/quick-check 1000 (prop/for-all* [gen/int] equiv-runs))))))

;; Generating basic generators
;; --------------------------------------------------------------------------
(deftest generators-test
  (let [t (fn [generator pred]
            (is (:result (tc/quick-check 100
                           (prop/for-all [x generator]
                             (pred x))))))
        is-char-fn #?(:default char? :cljs string?)]                                     ;;; Changed :clj to :default

    (testing "keyword"              (t gen/keyword keyword?))
	
    ;; No ratio in cljs
    #?@(:cljr [                                                                          ;;; Changed :clj to :cljr
    (testing "ratio"                (t gen/ratio   (some-fn ratio? integer?)))
    (testing "byte"                 (t gen/byte    #(instance? Byte %)))
    (testing "bytes"                (t gen/bytes   #(instance? |System.Byte[]| %)))])    ;;; (Class/forName "[B")

	
    (testing "char"                 (t gen/char                 is-char-fn))
    (testing "char-ascii"           (t gen/char-ascii           is-char-fn))
    (testing "char-alphanumeric"    (t gen/char-alphanumeric    is-char-fn))
    (testing "string"               (t gen/string               string?))
    (testing "string-ascii"         (t gen/string-ascii         string?))
    (testing "string-alphanumeric"  (t gen/string-alphanumeric  string?))

    (testing "vector" (t (gen/vector gen/int) vector?))
    (testing "list"   (t (gen/list gen/int)   list?))
    (testing "map"    (t (gen/map gen/int gen/int) map?))
    ))

;; Distinct collections
;; --------------------------------------------------------------------------

(def gen-distinct-generator
  (gen/elements [gen/list-distinct gen/vector-distinct
                 gen/set gen/sorted-set]))

(def gen-size-bounds-and-pred
  "Generates [pred size-opts], where size-opts is a map to pass to
  distinct generators, and pred is a predicate on the size of a
  collection, to check that it matches the options."
  (gen/one-of
   [(gen/return [(constantly true) {}])
    (gen/fmap (fn [num-elements]
                [#{num-elements} {:num-elements num-elements}])
              gen/nat)
    (gen/fmap (fn [min-elements]
                [#(<= min-elements %) {:min-elements min-elements}])
              gen/nat)
    (gen/fmap (fn [max-elements]
                [#(<= % max-elements) {:max-elements max-elements}])
              gen/nat)
    (gen/fmap (fn [bounds]
                (let [[min-elements max-elements] (sort bounds)]
                  [#(<= min-elements % max-elements)
                   {:min-elements min-elements
                    :max-elements max-elements}]))
              (gen/tuple gen/nat gen/nat))]))

(defspec map-honors-size-opts
  (prop/for-all [[the-map size-pred _]
                 (gen/bind gen-size-bounds-and-pred
                           (fn [[pred size-opts]]
                             (gen/fmap #(vector % pred size-opts)
                                       (gen/map gen/string gen/nat size-opts))))]
    (size-pred (count the-map))))

(defspec distinct-collections-honor-size-opts
  (prop/for-all [[the-coll size-pred _]
                 (gen/bind (gen/tuple gen-size-bounds-and-pred
                                      gen-distinct-generator)
                           (fn [[[pred size-opts] coll-gen]]
                             (gen/fmap #(vector % pred size-opts)
                                       (coll-gen gen/string size-opts))))]
    (size-pred (count the-coll))))

(defspec distinct-collections-are-distinct
  (prop/for-all [the-coll
                 (gen/bind (gen/tuple gen-size-bounds-and-pred
                                      gen-distinct-generator)
                           (fn [[[_ size-opts] coll-gen]]
                             (coll-gen gen/string size-opts)))]
    (or (empty? the-coll)
        (apply distinct? the-coll))))

(defspec distinct-by-collections-are-distinct-by 20
  (let [key-fn #(quot % 7)]
    (prop/for-all [the-coll
                   (gen/bind (gen/tuple gen-size-bounds-and-pred
                                        (gen/elements
                                         [(partial gen/vector-distinct-by key-fn)
                                          (partial gen/list-distinct-by key-fn)])
                                        gen-distinct-generator)
                             (fn [[[_ size-opts] coll-gen]]
                               (coll-gen (gen/choose -10000 10000) size-opts)))]
      (or (empty? the-coll)
          (apply distinct? (map key-fn the-coll))))))
		
(deftest distinct-generators-throw-when-necessary
  ;; I tried using `are` here but it breaks in cljs
  (doseq [g [gen/vector-distinct
             gen/list-distinct
             gen/set
             gen/sorted-set]]
    (is (thrown? #?(:default Exception :cljs js/Error)                             ;;; Changed :clj to :default
                      (first (gen/sample
                              (g gen/boolean {:min-elements 5}))))))
  (is (thrown? #?(:default Exception :cljs js/Error)                               ;;; Changed :clj to :default
               (first (gen/sample
                       (gen/map gen/boolean gen/nat {:min-elements 5}))))))

(defspec shrinking-respects-distinctness-and-sizing 20
  (prop/for-all [g gen-distinct-generator
                 seed gen-seed
                 size (gen/choose 1 20)
                 [pred opts] gen-size-bounds-and-pred]
    (let [rose-tree (gen/call-gen (g (gen/choose 0 1000) opts)
                                  (random/make-random seed) size)
          ;; inevitably some of these will be way too long to actually
          ;; test, so this is the easiest thing to do :/
          vals (take 1000 (rose/seq rose-tree))]
      (every? (fn [coll]
                (and (or (empty? coll)
                         (apply distinct? coll))
                     (pred (count coll))))
              vals))))

(defspec distinct-generators-can-shrink-in-size 20
  (prop/for-all [g gen-distinct-generator
                 seed gen-seed
                 size (gen/choose 1 20)]
    (let [rose-tree (gen/call-gen (g (gen/choose 1 1000))
                                  (random/make-random seed) size)
          a-shrink (->> rose-tree
                        (iterate #(first (rose/children %)))
                        (take-while identity)
                        (map rose/root))]
      (and (apply > (map #(reduce + %) a-shrink))
           (empty? (last a-shrink))))))

(defspec distinct-collections-are-not-biased-in-their-ordering 5
  (prop/for-all [g (gen/elements [gen/vector-distinct gen/list-distinct])
                 seed gen-seed]
    (let [rng (random/make-random seed)]
      (every?
       (->> (gen/lazy-random-states rng)
            (take 1000)
            (map #(rose/root (gen/call-gen (g gen/nat {:num-elements 3, :max-tries 100}) % 0)))
            (set))
       [[0 1 2] [0 2 1] [1 0 2] [1 2 0] [2 0 1] [2 1 0]]))))
	
(defspec distinct-collections-with-few-possible-values 20
  (prop/for-all [boolean-sets (gen/vector (gen/resize 5 (gen/set gen/boolean)) 100)]
    (= 4 (count (distinct boolean-sets)))))

(deftest can't-generate-set-of-five-booleans
  (is (thrown-with-msg? #?(:default Exception :cljs js/Error)                                ;;; Changed :clj to :default
                        #"Couldn't generate enough distinct elements"
                        (gen/generate (gen/set gen/boolean {:num-elements 5})))))

;; Generating proper matrices
;; ---------------------------------------------------------------------------

(defn proper-matrix?
  "Check if provided nested vectors form a proper matrix � that is, all nested
   vectors have the same length"
  [mtx]
  (let [first-size (count (first mtx))]
    (every? (partial = first-size) (map count (rest mtx)))))

(deftest proper-matrix-test
  (testing
    "can generate proper matrices"
    (is (:result (tc/quick-check
                  100 (prop/for-all
                       [mtx (gen/vector (gen/vector gen/int 3) 3)]
                       (proper-matrix? mtx)))))))

(def bounds-and-vector
  (gen/bind (gen/tuple gen/s-pos-int gen/s-pos-int)
            (fn [[a b]]
              (let [minimum (min a b)
                    maximum (max a b)]
                (gen/tuple (gen/return [minimum maximum])
                           (gen/vector gen/int minimum maximum))))))

(deftest proper-vector-test
  (testing
    "can generate vectors with sizes in a provided range"
    (is (:result (tc/quick-check
                  100 (prop/for-all
                       [b-and-v bounds-and-vector]
                       (let [[[minimum maximum] v] b-and-v
                             c (count v)]
                         (and (<= c maximum)
                              (>= c minimum)))))))))

;; Tuples and Pairs retain their count during shrinking
;; ---------------------------------------------------------------------------

(defn n-int-generators
  [n]
  (vec (repeat n gen/int)))

(def tuples
  [(apply gen/tuple (n-int-generators 1))
   (apply gen/tuple (n-int-generators 2))
   (apply gen/tuple (n-int-generators 3))
   (apply gen/tuple (n-int-generators 4))
   (apply gen/tuple (n-int-generators 5))
   (apply gen/tuple (n-int-generators 6))])

(defn get-tuple-gen
  [index]
  (nth tuples (dec index)))

(defn inner-tuple-property
  [size]
  (prop/for-all [t (get-tuple-gen size)]
                false))

(defspec tuples-retain-size-during-shrinking 1000
  (prop/for-all [index (gen/choose 1 6)]
                (let [result (tc/quick-check
                               100 (inner-tuple-property index))]
                  (= index (count (-> result
                                    :shrunk :smallest first))))))

;; Bind works
;; ---------------------------------------------------------------------------

(def nat-vec
  (gen/such-that not-empty
                 (gen/vector gen/nat)))

(def vec-and-elem
  (gen/bind nat-vec
            (fn [v]
              (gen/tuple (gen/elements v) (gen/return v)))))

(defspec element-is-in-vec 100
  (prop/for-all [[element coll] vec-and-elem]
                (some #{element} coll)))

;; fmap is respected during shrinking
;; ---------------------------------------------------------------------------

(def plus-fifty
  (gen/fmap (partial + 50) gen/nat))

(deftest f-map-respected-during-shrinking
  (testing
    "Generators created fmap should have that function applied
    during shrinking"
    (is (= [50]
           (let [result (tc/quick-check 100
                                        (prop/for-all
                                          [a plus-fifty]
                                          false))]
             (-> result :shrunk :smallest))))))

;; gen/int returns an integer when size is a double; regression for TCHECK-73
;; ---------------------------------------------------------------------------

(def gen-double
  (gen/fmap (fn [[x y]] (double (+ x (/ y 10))))
            (gen/tuple gen/pos-int (gen/choose 0 9))))

(defspec gen-int-with-double-size 1000
  (prop/for-all [size gen-double]
    (integer? (gen/generate gen/int size))))

;; recursive-gen doesn't change ints to doubles; regression for TCHECK-73
;; ---------------------------------------------------------------------------

(defspec recursive-generator-test 100
  (let [btree* (fn [g] (gen/hash-map
                        :value gen/int
                        :left g
                        :right g))
        btree (gen/recursive-gen btree* (gen/return nil))
        valid? (fn valid? [tree]
                 (and (integer? (:value tree))
                      (or (nil? (:left tree))
                          (valid? (:left tree)))
                      (or (nil? (:right tree))
                          (valid? (:right tree)))))]
    (prop/for-all [t btree] (valid? t))))

;; NOTE cljs: adjust for JS numerics - NB

#?(:cljr                                                                                      ;;; Change :clj to :cljr
(deftest calc-long-increasing
  ;; access internal gen/calc-long function for testing
  (are [low high] (apply < (map #(@#'gen/calc-long % low high) (range 0.0 0.9999 0.111)))
      ;; low and high should not be too close, 100 is a reasonable spread
      (- Int64/MaxValue 100) Int64/MaxValue                                   ;;; Long/MAX_VALUE Long/MAX_VALUE
      Int64/MinValue (+ Int64/MinValue 100)                                   ;;; Long/MIN_VALUE Long/MIN_VALUE
      Int64/MinValue 0                                                        ;;; Long/MIN_VALUE
      0 100
      -100 0
      0 Int64/MaxValue                                                        ;;; Long/MAX_VALUE
      Int64/MinValue Int64/MaxValue)))                                         ;;; Long/MIN_VALUE Long/MAX_VALUE  

;; edn rountrips
;; ---------------------------------------------------------------------------

(def simple-type
  "Like gen/simple-type but excludes Infinity and NaN."
  (gen/one-of [gen/int gen/large-integer #_(gen/double* {:infinite? false, :NaN? false}) gen/char gen/string                  ;;; commented out double* because we cannot get exact round-trip on X64/.Net,  see http://stackoverflow.com/questions/24299692/why-is-a-round-trip-conversion-via-a-string-not-safe-for-a-double
               gen/ratio gen/boolean gen/keyword gen/keyword-ns gen/symbol gen/symbol-ns gen/uuid]))

(def any-edn (gen/recursive-gen gen/container-type simple-type))

(defn edn-roundtrip?
  [value]
  (= value (-> value prn-str edn/read-string)))

(defspec edn-roundtrips 50
  (prop/for-all [a any-edn]
                (edn-roundtrip? a)))

;; not-empty works
;; ---------------------------------------------------------------------------

(defspec not-empty-works 100
  (prop/for-all [v (gen/not-empty (gen/vector gen/boolean))]
                (not-empty v)))

;; no-shrink works
;; ---------------------------------------------------------------------------

(defn run-no-shrink
  [i]
  (tc/quick-check 100
                  (prop/for-all [coll (gen/vector gen/nat)]
                                (some #{i} coll))))

(defspec no-shrink-works 100
  (prop/for-all [i gen/nat]
                (let [result (run-no-shrink i)]
                  (if (:result result)
                    true
                    (= (:fail result)
                       (-> result :shrunk :smallest))))))

;; elements works with a variety of input
;; ---------------------------------------------------------------------------

(deftest elements-with-empty
  (is (thrown? #?(:clj AssertionError :cljs js/Error  :cljr Exception)                ;;; Added :cljr clause
               (gen/elements ()))))

(defspec elements-with-a-set 100
  (prop/for-all [num (gen/elements #{9 10 11 12})]
    (<= 9 num 12)))


;; choose respects bounds during shrinking
;; ---------------------------------------------------------------------------

(def range-gen
  (gen/fmap (fn [[a b]]
              [(min a b) (max a b)])
            (gen/tuple gen/int gen/int)))

(defspec choose-respects-bounds-during-shrinking 100
  (prop/for-all [[mini maxi] range-gen
                 random-seed gen/nat
                 size gen/nat]
                (let [tree (gen/call-gen
                             (gen/choose mini maxi)
                             (random/make-random random-seed)
                             size)]
                  (every?
                    #(and (<= mini %) (>= maxi %))
                    (rose/seq tree)))))


;; rand-range copes with full range of longs as bounds
;; ---------------------------------------------------------------------------

;; NOTE cljs: need to adjust for JS numerics - David

#?(:cljr                                                                                            ;;; Changed :clj to :cljr
(deftest rand-range-copes-with-full-range-of-longs
  (let [[low high] (reduce
                    (fn [[low high :as margins] x]
                      (cond
                       (< x low) [x high]
                       (> x high) [low x]
                       :else margins))
                    [Int64/MaxValue Int64/MinValue]                                                  ;;; Long/MAX_VALUE Long/MIN_VALUE
                    ; choose uses rand-range directly, reasonable proxy for its
                    ; guarantees
                    (take 1e6 (gen/sample-seq (gen/choose Int64/MinValue Int64/MaxValue))))]         ;;; Long/MIN_VALUE Long/MAX_VALUE
    (is (< low high))
    (is (< low Int32/MinValue))                                                                      ;;; Integer/MIN_VALUE
    (is (> high Int32/MaxValue)))))                                                                  ;;; Integer/MAX_VALUE

;; rand-range yields values inclusive of both lower & upper bounds provided to it
;; further, that generators that use rand-range use its full range of values
;; ---------------------------------------------------------------------------

(deftest rand-range-uses-inclusive-bounds
  (let [bounds [5 7]
        rand-range (fn [r] (apply #'gen/rand-range r bounds))]
    (loop [trials 0
           bounds (set bounds)
           r (random/make-random)]
      (cond
       (== trials 10000)
       (is nil (str "rand-range didn't return both of its bounds after 10000 trials; "
                    "it is possible for this to fail without there being a problem, "
                    "but we should be able to rely upon probability to not bother us "
                    "too frequently."))
       (empty? bounds) (is true)
       :else (let [[r1 r2] (random/split r)]
               (recur (inc trials) (disj bounds (rand-range r1)) r2))))))

(deftest elements-generates-all-provided-values
  (let [options [:a 42 'c/d "foo"]]
    (is (->> (reductions
                 disj
                 (set options)
                 (gen/sample-seq (gen/elements options)))
             (take 10000)
             (some empty?))
        (str "elements didn't return all of its candidate values after 10000 trials; "
             "it is possible for this to fail without there being a problem, "
             "but we should be able to rely upon probability to not bother us "
             "too frequently."))))

;; shuffling a vector generates a permutation of that vector
;; ---------------------------------------------------------------------------

(def original-vector-and-permutation
  (gen/bind (gen/vector gen/int)
        #(gen/tuple (gen/return %) (gen/shuffle %))))

(defspec shuffled-vector-is-a-permutation-of-original 100
  (prop/for-all [[coll permutation] original-vector-and-permutation]
    (= (sort coll) (sort permutation))))

;; UUIDs
;; ---------------------------------------------------------------------------

(defspec uuid-generates-uuids
  (prop/for-all [uuid gen/uuid]
    (and (instance? #?(:clj java.util.UUID :cljs cljs.core.UUID :cljr System.Guid) uuid)                  ;;; Added :cljr clause
         ;; check that we got the special fields right
         #_(re-matches #"[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}"             ;;; Commented out this clause-  System.Guid does not have the encoding restriction.
                     (str uuid)))))

(deftest uuid-generates-distinct-values
  (is (apply distinct?
             (gen/sample gen/uuid 1000))))

;; large integers
;; ---------------------------------------------------------------------------

(defn gen-bounds-and-pred
  "Generates a map that may contain :min and :max, and a pred
  that checks a number satisfies those constraints."
  [num-gen]
  (gen/one-of [(gen/return [{} (constantly true)])
               (gen/fmap (fn [x] [{:min x} #(<= x %)]) num-gen)
               (gen/fmap (fn [x] [{:max x} #(<= % x)]) num-gen)
               (gen/fmap (fn [bounds]
                           (let [[lb ub] (sort bounds)]
                             [{:min lb :max ub} #(<= lb % ub)]))
                         (gen/tuple num-gen num-gen))]))

(def MAX_INTEGER #?(:clj Long/MAX_VALUE :cljs (dec (apply * (repeat 53 2))) :cljr Int64/MaxValue))      ;;; Added :cljr clause
(def MIN_INTEGER #?(:clj Long/MIN_VALUE :cljs (- MAX_INTEGER) :cljr Int64/MinValue))                    ;;; Added :cljr clause

(defn native-integer?
  [x]
  #?(:clj (instance? Long x)  :cljr (instance? Int64 x)                                                ;;; Added :cljr clause                                      
     :cljs (and (integer? x) (<= MIN_INTEGER x MAX_INTEGER))))

(defspec large-integer-spec 500
  (prop/for-all [x gen/large-integer]
    (native-integer? x)))

(defspec large-integer-bounds-spec 500
  (prop/for-all [[opts pred x]
                 (gen/bind (gen-bounds-and-pred gen/large-integer)
                           (fn [[opts pred]]
                             (gen/fmap #(vector opts pred %)
                                       (gen/large-integer* opts))))]
    (pred x)))

(defspec large-integer-distribution-spec 5
  (prop/for-all [xs (gen/no-shrink
                     (gen/vector (gen/resize 150 gen/large-integer) 10000))]
    (every? (fn [[lb ub]]
              (some #(<= lb % ub) xs))
            [[0 10]
             [-10 -1]
             [10000 100000]
             [-100000 -10000]
             [MIN_INTEGER (/ MIN_INTEGER 2)]
             [(/ MAX_INTEGER 2) MAX_INTEGER]])))

;; doubles
;; ---------------------------------------------------------------------------

(defn infinite?
  [x]
  #?(:clj (Double/isInfinite x)    :cljr (Double/IsInfinity x)                    ;;; Added :cljr clause
     :cljs (or (= @#'gen/POS_INFINITY x)
               (= @#'gen/NEG_INFINITY x))))

(defn nan?
  [x]
  #?(:clj (Double/isNaN x)  :cljr (Double/IsNaN x)                               ;;; Added :cljr clause
     :cljs (.isNaN js/Number x)))

(defspec double-test 100
  (prop/for-all [x gen/double]
    #?(:default (instance? Double x)                                             ;;; Changed :clj to :default
       :cljs (number? x))))

(defspec double-distribution-test 5
  (prop/for-all [xs (gen/no-shrink
                     (gen/vector (gen/resize 100 gen/double) 10000))]
    (and (some #(= @#'gen/POS_INFINITY %) xs)
         (some #(= @#'gen/NEG_INFINITY %) xs)
         (some nan? xs)
         (every? (fn [[lb ub]]
                   (some #(<= lb % ub) xs))
                 [[-1e303 -1e200]
                  [-1e200 -1e100]
                  [-1e100 -1.0]
                  [0.0 0.0]
                  [1.0 1e100]
                  [1e100 1e200]
                  [1e200 1e303]])
         #_(let [mods (->> xs                        ;;; mod blows in Numbers.remainder when number out of range of Int64.  See CLJCLR-83.
                         (remove infinite?)
                         (remove nan?)
                         (map #(mod % 1.0)))]
           (every? (fn [[lb ub]]
                     (some #(<= lb % ub) mods))
                   [[0.0 0.1]
                    [0.1 0.2]
                    [0.25 0.75]
                    [0.8 0.9]
                    [0.9 1.0]])))))

(defspec double-bounds-spec 500
  (prop/for-all [[opts pred x]
                 (gen/bind (gen-bounds-and-pred (gen/double* {:infinite? false, :NaN? false}))
                           (fn [[opts pred]]
                             (gen/fmap #(vector opts pred %)
                                       (gen/double* (assoc opts
                                                           :infinite? false,
                                                           :NaN? false)))))]
    (pred x)))


;; vector can generate large vectors; regression for TCHECK-49
;; ---------------------------------------------------------------------------

(deftest large-vector-test
  (is (= 100000
         (count (first (gen/sample
                        (gen/vector gen/nat 100000)
                        1))))))

;; scale controls growth rate of generators
;; ---------------------------------------------------------------------------

(deftest scale-test
  (let [g (gen/scale (partial min 10) gen/pos-int) ;; should limit size to 10
        samples (gen/sample g 1000)]
    (is (every? (partial >= 11) samples))
    (is (some (partial = 10) samples))))

;; generator dev helpers
;; ---------------------------------------------------------------------------

(deftest generate-test
  (is (string? (gen/generate gen/string)))
  (is (string? (gen/generate gen/string 42))))

;; defspec macro
;; ---------------------------------------------------------------------------

(defspec run-only-once 1 (prop/for-all* [gen/int] (constantly true)))

(defspec run-default-times (prop/for-all* [gen/int] (constantly true)))

(defspec run-with-map1 {:num-tests 1} (prop/for-all* [gen/int] (constantly true)))

(defspec run-with-map {:num-tests 1
                       :seed 1}
  (prop/for-all [a gen/int]
                (= a 0)))

(def my-defspec-options {:num-tests 1 :seed 1})

(defspec run-with-symbolic-options my-defspec-options
  (prop/for-all [a gen/int]
                (= a 0)))

(defspec run-with-no-options
  (prop/for-all [a gen/int]
                (integer? a)))
				
(defspec run-float-time 1e3
  (prop/for-all [a gen/int]
                (integer? a)))
				
;; let macro
;; ---------------------------------------------------------------------------

(defspec let-as-fmap-spec 20
  (prop/for-all [s (gen/let [n gen/nat]
                     (str n))]
    (re-matches #"\d+" s)))

(defspec let-as-bind-spec 20
  (prop/for-all [[xs x] (gen/let [xs (gen/not-empty (gen/vector gen/nat))]
                          (gen/tuple (gen/return xs) (gen/elements xs)))]
    (some #{x} xs)))

(defspec let-with-multiple-clauses-spec 20
  (prop/for-all [[xs x] (gen/let [xs (gen/not-empty (gen/vector gen/nat))
                                  x (gen/elements xs)]
                          [xs x])]
    (some #{x} xs)))

;; TCHECK-77 Regression
;; ---------------------------------------------------------------------------

;; Note cljs: need to adjust for JS numerics - NB
#?(:cljr                                                                       ;;; changed :clj to :cljr
(deftest choose-distribution-sanity-check
  (testing
      "Should not get the same random value more than 90% of the time"
    ;; This is a probabilistic test; the odds of a false-positive
    ;; failure for the ranges with two elements should be roughly 1 in
    ;; 10^162 (and even rarer for larger ranges), so it will never
    ;; ever happen.
    (are [low high] (let [xs (gen/sample (gen/choose low high) 1000)
                          count-of-most-frequent (apply max (vals (frequencies xs)))]
                      (< count-of-most-frequent 900))
      (dec Int64/MaxValue) Int64/MaxValue                                    ;;; Long/MAX_VALUE  Long/MAX_VALUE
      Int64/MinValue (inc Int64/MinValue)                                    ;;; Long/MIN_VALUE  Long/MIN_VALUE
      Int64/MinValue 0                                                       ;;; Long/MIN_VALUE
      0 1
      -1 0
      0 Int64/MaxValue                                                       ;;; Long/MAX_VALUE
      Int64/MinValue Int64/MaxValue)))) 			                         ;;; Long/MIN_VALUE  Long/MAX_VALUE