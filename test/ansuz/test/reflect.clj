(ns ansuz.test.reflect
  (:refer-clojure :exclude [reify])
  (:use [ansuz.reflect])
  (:use [clojure.test]))

(deftest test-with-args
  (let [vect ['a 'b 'c']]
    (is (= (macroexpand `(with-args ~vect (functor a b c)))
           `(functor a b c ~@(seq vect))))))

(deftest test-reflect-atom
  (is (= `(something-with ~'x ~'y ~'z)
         (macroexpand `(with-args [~'x ~'y ~'z] (reflect [a b c] (something-with a b c)))))))

(deftest test-reflect-sequence
  (is (= `(something-with (foo ~'x) ~'y ~'z)
         (macroexpand `(with-args [~'x ~'y ~'z] (reflect [a b c] (something-with (foo a) b c)))))))

(deftest test-reflect-vector
  (is (= `(something-with [1 2 ~'x 4] ~'y ~'z)
         (macroexpand `(with-args [~'x ~'y ~'z] (reflect [a b c] (something-with [1 2 a 4] b c)))))))

(deftest test-reflect-map
  (is (= `(something-with {:x ~'x :y 2} ~'y ~'z)
         (macroexpand `(with-args [~'x ~'y ~'z] (reflect [a b c] (something-with {:x a :y 2} b c)))))))

(deftest test-reify-atom
  (is (= (macroexpand `(reify (~'a (parse ~'w)) (~'a)))
         `(parse ~'w))))

(deftest test-reify-sequence
  (is (= (macroexpand `(reify (~'a (parse ~'w)) (a sequence (~'a))))
         `(a sequence (parse ~'w)))))

(deftest test-reify-vector
  (is (= (macroexpand `(reify (~'a (parse ~'w)) [:a :vector (~'a)]))
         `(:a :vector (parse ~'w)))))
         
