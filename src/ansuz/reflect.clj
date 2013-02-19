;; this is the lower part 
(ns ansuz.reflect
  (:refer-clojure :exclude [reify])
  (:require clojure.core))

(defmacro with-args 
  "this simple macro takes a set of arguments and append it to the subsequent expression"
  [x c]
  (seq (concat c x)))

(defmacro reflect 
  "while in monad world, it exposes monad internals:

    (reflect [token-stream success fail] <body>)

  the resulting expression is a parser, but implementation access directly to monad internals.
  User code should not use this, because traverse monad abstraction bounduaries, 
  if a future release of parsing library will rely on a different monad representation,
  the resulting code needs to be changed accordingly.
  "
  [v b & x]
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

(defmacro reify 
  "that's the complementary function of reflect, because binds a monadic parser to a name
 that can be called with the monad internal values

    (reify [parser (bind [x ...] (ret 'whatever))]
      (parser token-stream success fail)

  As for reflect do not use this in user code, unless you are sure on what you are doing
  because it can break monad abstraction.
  "
  [[v m] val & xs]
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


