(ns clojure.test.check.clojure-test.assertions.cljs)

#?(:default                                                                             ;;; change :clj to :default
   (try
     (require 'cljs.test
       '[clojure.test.check.clojure-test.assertions :as assertions])

     (eval
       '(defmethod cljs.test/assert-expr 'clojure.test.check.clojure-test/check?
          [_ msg form]
          (assertions/check? msg form)))
     (catch java.io.FileNotFoundException e))
   :cljs
   (defmethod cljs.test$macros/assert-expr 'clojure.test.check.clojure-test/check?
     [_ msg form]
     (clojure.test.check.clojure-test.assertions/check? msg form)))