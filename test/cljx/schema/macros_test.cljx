(ns schema.macros-test
  "Tests for schema macros. "
  #+clj (:use clojure.test [schema.test-macros :only [valid! invalid! invalid-call!]])
  #+cljs (:use-macros
          [cljs-test.macros :only [is is= deftest]]
          [schema.test-macros :only [testing valid! invalid! invalid-call! thrown?]])
  #+cljs (:require-macros
          [schema.macros :as sm])
  (:require
   #+clj [schema.macros :as sm]
   #+cljs cljs-test.core))

(deftest assert-iae-test
  (let [even-check (fn [x] (sm/assert-iae (even? x) "%d is not even!" x))]
    ;; the docs say it will raise an IllegalArgumentException but that seems to be a carry-over
    ;; from the plumbing library. The utils/error fn could be extended to have an exception type
    ;; be passed in but wiht the cljs support it may not be worth the effort.
    (is (thrown-with-msg? RuntimeException #"3 is not even!"
                          (even-check 3)))))
