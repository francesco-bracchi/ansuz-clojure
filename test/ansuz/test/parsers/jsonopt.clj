(ns ansuz.test.parsers.jsonopt
  (:use [ansuz.core])
  (:use [ansuz.language])
  (:use [ansuz.parsers.jsonopt])
  (:use [clojure.test]))

(defn times [n f]
  (if (<= n 1) (f)
      (do (f)
          (recur (dec n) f))))

(deftest test-time 
  (let [s "[1,2,3,4,5,6,7,8,9,{\"a\": 100}, true,   false]"]
    (time (times 100000 #(parse s)))))

