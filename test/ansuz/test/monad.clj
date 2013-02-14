(ns ansuz.test.monad
  (:use [ansuz.monad])
  (:use [ansuz.language])
  (:use [clojure.test]))

(defn a-value [] (rand 1000))

(deftest test-return
  (let [val (a-value)]
    (is (= (run (ret val) "") val))))

(deftest test-bind
  (let [val (a-value)]
    (is (= (run (bind [v (ret val)] (ret v)) "")
           val))))
