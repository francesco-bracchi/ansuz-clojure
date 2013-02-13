(ns ansuz.test
  (:use [ansuz.parsers.json])
  (:gen-class))

(defn times [n fn]
  (let [v (fn)]
    (if (>= n 1)
      (recur (dec n) fn)
      v)))

(def input "{\"1234567890\": [1,2,3,4,5,6,7,8,9,0], \"true\": true, \"false\": false, \"string\" : \"this will be a very long string\"}")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (time (times 10000 #(parse input))))


;; (ns ansuz.test
;;   (:use ansuz.parsers.json)
;;   (:genclass))

;; (def input "{\"1234567890\": [1,2,3,4,5,6,7,8,9,0], \"true\": true, \"false\": false \"string\" : \"this will be a very long string\"}")

;; (defn -main [& args]
;;   (print (times 10000 #(parse input))))
