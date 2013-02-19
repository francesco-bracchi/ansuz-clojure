;; #MONAD
;; This file contains monadic abstraction functions
;; 
(ns ansuz.monad
  (:refer-clojure :exclude [reify])
  (:use [ansuz.reflect]))

(defn signature-with-args [as sig]
  (let [[params & body] sig
        params (vec (concat params as))
        conds (when (and (next body) (map? (first body)))
                (first body))
        body (if conds (next body) body)]
    (if conds
      `(~params ~conds (with-args ~as ~@body))
      `(~params (with-args ~as ~@body)))))

(defmacro par
  "that macro creates a parameterize parser, the syntax resemble `fn`
   see ansuz.language/parser or ansuz.language/defparser
  "
  [& sigs]
  (let [name (if (symbol? (first sigs)) (first sigs) nil)
        sigs (if name (rest sigs) sigs)
        sigs (if (vector? (first sigs)) (list sigs) sigs)
        as (vec (map gensym '(str sc fl)))
        sigs (map #(signature-with-args as %) sigs)]
    (if name
      `(fn ~name ~@sigs)
      `(fn ~@sigs))))
        

    ;; (if (vector? (first sigs))
    ;;   (let [[params & body] sigs]
    ;;     (if name 
    ;;       `(fn ~name ~(vec (concat params as)) (with-args ~(vec as) ~@body))
    ;;       `(fn ~(vec (concat params as)) (with-args ~(vec as) ~@body))))
    ;;   (let [sigs (map #(cons (vec (concat (first %) as)) (rest %)) sigs)]
    ;;     (if name
    ;;       `(fn ~name ,@sigs)
    ;;       `(fn ,@sigs))))))
    
;; (defmacro defpar [n & rest] 
;;   "`(defpar foo [x y] (bar) (baz)) == (def foo (par [x y] (bar) (baz)))`"
;;   `(def ~n (par ~@rest)))

(defmacro defpar [name & decl]
  (let [pre (if (string? (first decl)) (list (first decl)) nil)
        decl (if (string? (first decl)) (next decl) decl)
        pre (if (map? (first decl)) (concat pre (first decl)) pre)
        decl (if (map? (first decl)) (next decl) decl)
        decl (if (vector? (first decl)) (list decl) decl)
        post (if (map? (last decl)) (list (last decl)) nil)
        decl (if (map? (last decl)) (butlast decl) decl)
        as (vec (map gensym '(str sc fl)))
        decl (map #(signature-with-args as %) decl)
        ]
    `(defn ~name ~@(concat pre decl post))))
        
  
(defmacro defmacrop 
  "like **defmacro** but the resulting macro can be used in as a parser.
  The body is a single parser expression.
  it is used internally by the library. Do not use it unless you know what you are doing
  " 
  ([name doc-string attr-map params body]
     (let [as (map gensym '(str sc fl))]
       `(defmacro ~name ~doc-string ~attr-map ~(vec (concat params as))
          (list 'with-args (vector ~@as) ~body))))
  ([name doc-string params body]
     (let [as (map gensym '(str sc fl))]
       `(defmacro ~name ~doc-string ~(vec (concat params as))
          (list 'with-args (vector ~@as) ~body))))
  ([name params body]
     (let [as (map gensym '(str sc fl))]
       `(defmacro ~name ~(vec (concat params as))
          (list 'with-args (vector ~@as) ~body)))))

(defmacrop ret 
  "monadic operator return `(run (ret 10) <anything) => 10)`"
  [v]
  (let [[str sc fl :as as] (map gensym '(str sc fl))]
    `(reflect ~(vec as) #(~sc ~v ~str ~fl))))

(defmacrop bind
  "that's the bind operator. It works in a different way from haskell bind (`>>=`).

    (bind [x <monad-expression>] <another monad expression>)
  
  in the context of <another monad expression> x is bound to the result of <monad expression>.
  "
  [[v m] n]
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
