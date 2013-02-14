(ns ansuz.core
  (:refer-clojure :exclude [reify])
  (:use [ansuz.reflect])
  (:use [ansuz.monad :exclude [ret]])
  (:use [ansuz.monadplus]))

(defmacrop fail [r]
  (let [[str sc fl :as as] (map gensym '(str sc fl))]
    `(reflect ~(vec as)
              (fn [] (~fl ~r)))))

(defmacrop ret [w]
  `(ansuz.monad/ret ~w))

(defmacrop any []
  (let [[str sc fl :as as] (map gensym '(str sc fl))]
    `(reflect ~(vec as)
              (if (empty? ~str)
                (fn [] (~fl "any failed (end of stream"))
                (fn [] (~sc (first ~str) (next ~str) ~fl))))))

(defmacrop end []
  (let [[str sc fl :as as] (map gensym '(str sc fl))]
    `(reflect ~(vec as)
              (if (empty? ~str)
                (fn [] (~sc true ~str ~fl))
                (fn [] (~fl "not end"))))))

(defmacrop ! [v]
  (let [[str sc fl :as as] (map gensym '(str sc fl))
        v1 (gensym 'v)]
    `(reflect ~(vec as)
              (let [~v1 (first ~str)]
                (if (= ~v ~v1)
                  (fn [] (~sc ~v1 (rest ~str) ~fl))
                  (fn [] (~fl "! failed")))))))

(defmacrop ? [tst]
  (let [[str sc fl :as as] (map gensym '(str sc fl))
        v1 (gensym 'v)]
    `(reflect ~(vec as)
              (if (~tst (first ~str))
                (fn [] (~sc (first ~str) (rest ~str) ~fl))
                (fn [] (~fl "? failed"))))))

(defmacrop in []
  (let [[str sc fl :as as] (map gensym '(str sc fl))]
    `(reflect ~(vec as) (fn [] (~sc ~str ~str ~fl)))))

;; call with current continuation
(defmacrop callcc [p]
  (let [[str sc fl :as as] (map gensym '(str sc fl))
        pp (gensym 'p)]
    `(reify [~pp ~p]
       (reflect ~(vec as)
         (fn [] (~sc (with-args ~(vec as) (~pp ~sc)) ~str ~fl))))))

;; call with current failure
(defmacrop callcf [p]
  (let [[str sc fl :as as] (map gensym '(str sc fl))
        pp (gensym 'p)]
    `(reify [~pp ~p]
       (reflect ~(vec as)
         (fn [] (~sc (with-args ~(vec as) (~pp ~fl)) ~str ~fl))))))

