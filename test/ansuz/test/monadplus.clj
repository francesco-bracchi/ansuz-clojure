(ns ansuz.test.monadplus
  (:use [ansuz.language])
  (:use [ansuz.core])
  (:use [clojure.test]))

(deftest test-orelse
  (let [a-or-b (parser [] (alt \a \b))]
    (is (= (run (a-or-b) "ab") 
           \a)
        "orelse doesn't recognize the first branch")
    (is (= (run (a-or-b) "ba") 
           \b)
           "orelse doesn't recognize the second branch")
    (is (= (run (a-or-b) "cc" (fn [x] 'fail)) 
           'fail)
           "orelse doesn't fail recognizing one of the branches")))

(deftest test-orelse*
  (let [a-or-b (parser [] (alt* \a \b))]
    (is (= (run (a-or-b) "ab") 
           \a)
           "orelse* doesn't recognize the first branch")
    (is (= (run (a-or-b) "ba") 
           \b)
           "orelse* doesn't recognize the second branch")
    (is (= (run (a-or-b) "cc" (fn [x] 'fail))
           'fail)
           "orelse doesn't fail recognizing one of the branches")))

(deftest test-different-behavior-between-orelse-and-orelse*
  (let [ab (parser [] \a \b)
        a (parser [] \a)
        b (parser [] \b)
        p (parser [] (alt (ab) (a)) (b))
        p* (parser [] (alt* (ab) (a)) (b))]
    (is (= (run (p) "ab" (fn [x] 'fail))
           'fail)
           "orelse uncorrecly work with (cat (alt ab a) b)")
    (is (= (run (p*) "ab" (fn [x] 'fail))
           \b)
           "orelse* fail with (cat (alt ab a) b)")))
        
           
