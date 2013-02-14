(ns ansuz.reflect
  (:refer-clojure :exclude [reify])
  (:require clojure.core))

(defmacro with-args [x c]
  (seq (concat c x)))

(defmacro reflect [v b & x]
  (letfn[(replace_ [smap coll]
           (map #(cond
                  (seq? %) 
                  (seq (replace_ smap %))
                  (vector? %)
                  (vec (replace_ smap %))
                  (map? %) (zipmap (replace_ smap (keys %))
                                   (replace_ smap (vals %)))
                  :else %)
                (replace smap coll)))]
    (replace_ (zipmap v x) b)))

(defmacro reify [[v m] val & xs]
  (letfn [(walk [val]
            (cond 
             (and (seq? val) (= (first val) v))
             (concat m (walk (rest val)))
             
             (seq? val)
             (seq (map walk val))
             
             (vector? val)
             (vec (map walk val))
             
             :else val))]
    `(with-args
       ~(vec xs)
       ~(walk val))))


