(ns clojure.test.check.clojure-test.assertions
  #?(:cljs (:require-macros [clojure.test.check.clojure-test.assertions.cljs]))
  (:require #?(:default [clojure.test :as t]             ;;; changed :clj to :default
               :cljs [cljs.test :as t])
            [clojure.test.check.results :as results]))

#?(:cljr                                                 ;;; changed :clj to :cljr												
   (defn test-context-stacktrace [st]
     (drop-while
       #(let [class-name (.FullName (.GetType ^System.Diagnostics.StackFrame %))]                           ;;; (.getClassName ^StackTraceElement %)
          (or (.StartsWith class-name "System")                                           ;;; .startsWith  "java.lang"  -- I guess "System" as good as I can get
              (.StartsWith  class-name "clojure.test+")                                      ;;; .startsWith "clojure.test$"
              (.StartsWith  class-name "clojure.test.check.clojure_test+")                   ;;; .startsWith "clojure.test.check.clojure_test$"
              (.StartsWith  class-name "clojure.test.check.clojure_test.assertions")))       ;;; .startsWith 
       st)))

#?(:cljr                                                 ;;; changed :clj to :cljr		
   (defn file-and-line*
     [stacktrace]
     (if (seq stacktrace)
       (let [^System.Diagnostics.StackFrame s (first stacktrace)]                        ;;; ^StackTraceElement
         {:file (.GetFileName s) :line (.GetFileLineNumber s)})                              ;;; .getFileName   .getLineNumber
       {:file nil :line nil})))

(defn check-results [{:keys [result] :as m}]
  (if (results/passing? result)
    (t/do-report
      {:type :pass
       :message (dissoc m :result)})
    (t/do-report
      (merge {:type :fail
              :expected {:result true}
              :actual m}
             #?(:clj (file-and-line* (test-context-stacktrace (.getStackTrace (Thread/currentThread))))
			    :cljr (file-and-line* (test-context-stacktrace (.GetFrames (System.Diagnostics.StackTrace.))))   ;;; Added :cljr
                :cljs (t/file-and-line (js/Error.) 4))))))

(defn check?
  [_ form]
  `(let [m# ~(nth form 1)]
     (check-results m#)))


#?(:cljr                                                                                  ;;; changed :clj to :cljr
   (defmethod t/assert-expr 'clojure.test.check.clojure-test/check?
     [_ form]
     (check? _ form)))