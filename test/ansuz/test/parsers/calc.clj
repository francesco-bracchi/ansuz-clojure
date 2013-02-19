(ns ansuz.test.parsers.calc
  (:use [ansuz.language])
  (:use [ansuz.parsers.calc])
  (:use [clojure.test]))

;(fact (parse "2+3") => 5)

(deftest sum-test
  (is (= (parse "2+3") 5)))

(deftest dif-test
  (is (= (parse "2-3") -1)))

(deftest mul-test
  (is (= (parse "2*3") 6)))

(deftest div-test
  (is (= (parse "2/3") 2/3)))

(deftest sqr-test
  (is (= (parse "5^") 25)))

(deftest fact-test
  (is (= (parse "5!") 120)))

(deftest remove-spaces-test
  (is (= (parse "   1    +   1     ") 2)))

(deftest unary-operator-minus-priority-test
  (is (= (parse "-5*2") -10))
  (is (= (parse "-5/2") -5/2))
  (is (= (parse "-5+2") -3))
  (is (= (parse "-5-2") -7))
  (is (= (parse "-5^") 25))
  (is (= (parse "-5!") 1)))

(deftest multiplication-priority-test
  (is (= (parse "3*-2") -6))
  (is (= (parse "5*2+1") 11))
  (is (= (parse "5*2-1") 9))
  (is (= (parse "5*2/3") 10/3))
  (is (= (parse "5^*2") 50))
  (is (= (parse "5!*2") 240)))

(deftest division-priority-test
  (is (= (parse "3/-2") -3/2))
  (is (= (parse "5/2+1") 7/2))
  (is (= (parse "5/2-1") 3/2))
  (is (= (parse "5/2*3") 15/2))
  (is (= (parse "5^/2") 25/2))
  (is (= (parse "5!/2") 60)))

(deftest square-priority-test
  (is (= (parse "-3^") 9))
  (is (= (parse "3^+2") 11))
  (is (= (parse "3^-2") 7))
  (is (= (parse "3^*2") 18))
  (is (= (parse "3^/2") 9/2))
  (is (= (parse "3^!") 362880)))

(deftest factorial-priority-test
  (is (= (parse "-3!") 1))
  (is (= (parse "3!+2") 8))
  (is (= (parse "3!-2") 4))
  (is (= (parse "3!*2") 12))
  (is (= (parse "3!/2") 3))
  (is (= (parse "3!^") 36)))

(deftest bracket-test
  (is (= (parse "(3+4)*5") 35)))

