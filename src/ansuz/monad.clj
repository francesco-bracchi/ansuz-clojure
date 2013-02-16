(ns ansuz.monad
  (:refer-clojure :exclude [reify])
  (:use [ansuz.reflect]))

(defmacro par
  ([b]
     (let [as (map gensym '(str sc fl))]
       `(fn ~(vec as) (with-args ~(vec as) ~b))))
  ([f b]
     (let [as (map gensym '(str sc fl))]
       `(fn ~(vec (concat f as)) (with-args ~(vec as) ~b))))
  ([s f b]
     (let [as (map gensym '(str sc fl))]
       `(fn ~s ~(vec (concat f as)) (with-args ~(vec as) ~b)))))

(defmacro letpar [v b]
  (let [mapfn (fn [[n f b]]
                (let [as (map gensym '(str sc fl))]
                  `(~n ~(vec (concat f as)) (with-args ~(vec as) ~b))))
        as (map gensym '(str sc fl))]
    `(letfn ~(vec (map mapfn v))
       (with-args ~(vec as) ~b))))

(defmacro defpar [n fs b]
  `(def ~n (par ~fs ~b)))
                
(defmacro defmacrop [n fs b]
  (let [as (map gensym '(str sc fl))]
    `(defmacro ~n ~(vec (concat fs as))
       (list 'with-args (vector ~@as) ~b))))

(defmacrop ret [v]
  (let [[str sc fl :as as] (map gensym '(str sc fl))]
    `(reflect ~(vec as) #(~sc ~v ~str ~fl))))

(defmacrop bind [[v m] n]
  (let [[str1 sc1 fl1 :as as1] (map gensym '(str sc fl))
        [str2 sc2 fl2 :as as2] (map gensym '(str sc fl))
        mm (gensym 'm)
        nn (gensym 'n)]
    `(reify [~mm ~m]
       (reify [~nn ~n]
         (reflect ~(vec as1)
           (fn [] (~mm ~str1
                       (fn [~v ~str2 ~fl2] (~nn ~str2 ~sc1 ~fl2))
                       ~fl1)))))))
