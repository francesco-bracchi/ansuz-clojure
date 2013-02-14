(ns ansuz.test.language
  (:use [ansuz.core])
  (:use [ansuz.language])
  (:use [clojure.test]))

(deftest test-parser 
  (let [p (parser [] \a)]
    (is (= (run (p) "a") \a)
        "wrong parser generation")))

(deftest test-cat 
  (let [p (parser [] (cat \a \b))]
    (is (= (run (p) "ab") \b)
        "wrong cat operator")))

(deftest test-alt
  (let [p (parser [] (alt \a \b))]
    (is (= (run (p) "ab") \a)
        "wrong alt operator")))

(deftest test-alt*
  (let [p (parser [] (alt* \a \b))]
    (is (= (run (p) "ab") \a)
        "wrong alt* operator")))

(deftest test-let
  (let [p (parser []
                  (let [x 100]
                    (ret x)))]
    (is (= (run (p) "") 100)
        "wrong let operator")))

(deftest test-if
  (let [p (parser [x]
                  (if x 
                    (ret "true") 
                    (ret "false")))]
    (is (= (run (p true) "") "true"))
    (is (= (run (p false) "") "false"))))

(deftest test-cond
  (let [p (parser [x]
                  (cond
                   (= x 10) (ret "ten")
                   (= x 20) (ret "twenty")
                   (= x 30) (ret "thirty")
                   :else (ret "unknown number")))]
    (is (= (run (p 10) "") "ten"))
    (is (= (run (p 20) "") "twenty"))
    (is (= (run (p 30) "") "thirty"))
    (is (= (run (p :none) "") "unknown number"))))

(deftest test-run
  (is (= (run (cat \a \b \c) "abc") \c))
  (is (= (run (cat \a \b \c) "abd" (fn [x] 'fail)) 'fail)))

(deftest test-run-ndet
  (let [p (parser [] 
                  (<- x (alt* (cat \a \b (ret "ab"))
                              (cat \a (ret "a"))))
                  \b
                  (ret (str x "b")))]
    (is (= (count (run-ndet (p) "abb")) 2))
    (is (= ["abb" "ab"] (vec (run-ndet (p) "abb"))))))

(def ^:dynamic pos)

(deftest test-ndet-sequence-is-lazy
  (binding [pos 'none]
    (let [p (parser [] 
                    (<- x (alt* (cat \a \b 
                                     (ret (set! pos 'first))
                                     (ret "ab"))
                                (cat \a 
                                     (ret (set! pos 'second))
                                     (ret "a"))))
                    \b
                    (ret (str x "b")))
          s (run-ndet (p) "abb")]

      (is (= pos 'none))
      (first s)
      (is (= pos 'first))
      (second s)
      (is (= pos 'second)))))
     
