(ns ansuz.test.core
  (:use [ansuz.core])
  (:use [ansuz.language])
  (:use [clojure.test]))

(deftest replace-me ;; FIXME: write
  (is false "No tests have been written."))

(deftest test-fail
  (try (run (fail "this is an error"))
       (catch Error (is true)))
  (is false))

