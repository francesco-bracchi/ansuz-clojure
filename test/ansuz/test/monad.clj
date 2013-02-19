(ns ansuz.test.monad
  (:use [ansuz.monad])
  (:use [ansuz.language])
  (:use [clojure.test]))

(defn a-value [] (rand 1000))

(deftest first-law-test
  "(return x) >>= f == f x"
  (let [val (a-value)]
    (is (= (run (bind [v (ret val)] (ret v)) "abc")
           val))))

(deftest second-law-test
  "m >>= return == m"
  (let [a (parser [] \a)]
    (is (= (run (bind [x (a)] (ret x)) "abc") (run (a) "abc")))))

(deftest third-law-test
  "(m >>= f) >>= g == m >>= (\\x -> f x >>= g)"
  (let[a (parser [] \a)
       b (parser [] \b)
       c (parser [] \c)]
    (is (= (run (bind [y (bind [x (a)] (b))] (c)) "abc")
           (run (bind [x (a)] (bind [y (b)] (c))) "abc")))))
      
           
