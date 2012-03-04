(ns ansuz.core
  (:refer-clojure :exclude [reify])
  (:use [ansuz.reflect])
  (:use [ansuz.monad])
  (:use [ansuz.monadplus]))

(defmacrop fail [r]
  (let [[str sc fl :as as] (map gensym '(str sc fl))]
    `(reflect ~(vec as)
              (~fl ~r ~str ~sc))))

(defmacrop retv [w]
  (let[[str sc fl :as as] (map gensym '(str sc fl))]
    `(with-args ~(vec as) (ret ~w))))

(defmacrop any []
  (let [[str sc fl :as as] (map gensym '(str sc fl))]
    `(reflect ~(vec as)
              (~sc (first ~str) (next ~str) ~fl))))

(defmacrop end []
  (let [[str sc fl :as as] (map gensym '(str sc fl))
        str1 (gensym 'str)]
    `(reflect ~(vec as)
              (let [~str1 ~str1]
                (if (empty? ~str1)
                  (~sc true ~str1 ~fl)
                  (~fl "not end" ~str1 ~sc))))))

(defmacrop ! [v]
  (let [[str sc fl :as as] (map gensym '(str sc fl))
        v1 (gensym 'v)]
    `(reflect ~(vec as)
              (let [~v1 (first ~str)]
                (if (= ~v ~v1)
                  (~sc ~v1 (rest ~str) ~fl)
                  (~fl "get failed" ~str ~sc))))))

(defmacrop ? [tst]
  (let [[str sc fl :as as] (map gensym '(str sc fl))
        v1 (gensym 'v)]
    `(reflect ~(vec as)
              (if (~tst (first ~str))
                (~sc (first ~str) (rest ~str) ~fl)
                (~fl "get test failed" ~str ~sc)))))

(defmacrop in []
  (let [[str sc fl :as as] (map gensym '(str sc fl))]
    `(reflect ~(vec as)
              (~sc ~str ~str ~fl))))

;; call with current continuation
(defmacrop call/cc [p]
  (let [[str sc fl :as as] (map gensym '(str sc fl))
        pp (gensym 'p)]
    `(reify [~pp ~p]
       (reflect ~(vec as)
         (~sc (with-args ~(vec as) (~pp ~sc)) ~str ~fl)))))

;; call with current failure
(defmacrop call/cf [p]
  (let [[str sc fl :as as] (map gensym '(str sc fl))
        pp (gensym 'p)]
    `(reify [~pp ~p]
       (reflect ~(vec as)
         (~sc (with-args ~(vec as) (~pp ~fl)) ~str ~fl)))))
