(ns ansuz.test
  (:use [ansuz.parsers.json])
  (:gen-class))

(defn times [n fn]
  (let [v (fn)]
    (if (>= n 1)
      (recur (dec n) fn)
      v)))

(def input "{\"1234567890\": [1,2,3,4,5,6,7,8,9,0], \"true\": true, \"false\": false, \"string\" : \"this will be a very long string\"}")

(defn make-json-array [n in]
  (let [inputs (map (fn [_] in) (range 0 n))]
    (str "[" (reduce #(str %2 "," %1) inputs) "]")))
  
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [input1 (make-json-array 10000 input)]
    (time (do (parse input1) 'ok))))
