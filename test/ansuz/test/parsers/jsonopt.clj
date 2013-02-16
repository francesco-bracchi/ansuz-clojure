(ns ansuz.test.parsers.jsonopt
  (:use [ansuz.language])
  (:use [ansuz.parsers.json])
  (:use [ansuz.test.parsers.json])
  (:use [clojure.test]))

(deftest test-parse-jsonopt
  (is (.equals (parse astring) astruct)))

