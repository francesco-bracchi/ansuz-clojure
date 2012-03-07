(ns ansuz.language
  (:refer-clojure :exclude [reify])
  (:use [ansuz.core :only [!]])
  (:use [ansuz.reflect])
  (:use [ansuz.monad])
  (:use [ansuz.monadplus]))

;; the main problem with bind and orelse
;; is that they cant't work with variadic args
;; ie (orelse a b c) should be written as (orelse a (orelse b c)).
;; in order to avoid it we provide a code walker that provides
;; that kind of rewriting, this walker is the macro evalp

(defn evalp-cat [e & es]
  (cond
   (empty? es)
   `(evalp ~e)
   (and (seq? e) (= (first e) '<-))
   (let[[_ n m] e]`(bind [~n (evalp ~m)] (evalp (~'cat ~@es))))
   :else
   `(bind [ignore# (evalp ~e)] (evalp (~'cat ~@es)))))

(defn evalp-alt [e & es]
  (if (empty? es)
    `(evalp ~e)
    `(orelse (evalp ~e)
             (evalp (~'alt ~@es)))))

(defn evalp-alt* [e & es]
  (if (empty? es)
    `(evalp ~e)
    `(orelse* (evalp ~e)
              (evalp (~'alt* ~@es)))))

(defn evalp-let [v & es]
  (let [[str sc fl :as as] (map gensym '(str sc fl))
        ee (gensym 'es)]
    `(reify [~ee (evalp (~'cat ~@es))]
       (reflect ~as
         (let ~v (with-args ~as (~ee)))))))

(defn evalp-if [t? l r]
  (let [ll (gensym 'l)
        rr (gensym 'r)
        [str sc fl :as as] (map gensym '(str sc fl))]
    `(reify [~ll (evalp ~l)]
       (reify [~rr (evalp ~r)]
         (reflect ~(vec as)
           (if ~t?
             (with-args ~(vec as) (~ll))
             (with-args ~(vec as) (~rr))))))))

(defn evalp-cond [& es]
  (if (empty? es) `(fail "cond failed")
      (let[[t? m & xs] es
           as (map gensym '(str sc fl))
           xx (gensym 'x)]
        `(reify [~xx (evalp ~m)]
           (reflect ~(vec as)
             (if ~t?
               (with-args ~(vec as) (~xx))
               (evalp (cond ~@xs))))))))

(def parser-operators
  {'cat   evalp-cat
   'alt   evalp-alt
   'alt*  evalp-alt*
   'let   evalp-let
   'if    evalp-if
   'cond  evalp-cond
   })

(defmacrop evalp [e]
  (if (not (seq? e))
    `(! ~e)
    (let [op ((first e) parser-operators)]
      (if op
        (apply op (next e))
        e))))

(defmacro parser [f & b]
  (cond
   (vector? f) 
   `(par ~f (evalp (~'cat ~@b)))
   (symbol? f)
   `(par ~f ~(first b) (evalp (~'cat ~@(next b))))))

(defmacro defparser [n f & b]
  `(def ~n (parser ~f ~@b)))

(defmacro letparser [v & b]
  (let[mapfn (fn [[n f & b]] `(~n ~f (evalp (~'cat ~@b))))]
    `(letpar
      ~(vec (map mapfn v))
      (evalp (~'cat ~@b)))))

(defmacro run
  ([p src]
     `(run ~p ~src #(throw (Error. %))))
  
  ([p src fail]
     (let[v (gensym 'v)
          r (gensym 'r)
          [str sc fl] (map gensym '(str sc fl))
          _sc `(fn [~v ~str ~fl] ~v)
          _fl `(fn [~r ~str ~sc] (~fail ~r))]
       `(with-args [ ~src ~_sc ~_fl] (evalp ~p)))))

(defmacro run-ndet [p src]
  (let[v (gensym 'v)
       r (gensym 'r)
       [str sc fl] (map gensym '(str sc fl))
       _sc `(fn [~v ~str ~fl] (cons ~v (~fl 'restart)))
       _fl `(fn [~r ~str ~fl] ())]
    `(with-args [~src ~_sc ~_fl] (evalp ~p))))
