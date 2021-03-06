;; #Core
;; This is a set of some basic parsers.
;; these are the basic building blocks of your parser,
;; while bind and orelse are the way of combining them in a meaningful way.
;; see the ansuz.language as s DSL built on top of 
;; **ansuz.core**, **ansuz.monad**, **ansuz.monadplus**.
;;
(ns ansuz.core
  (:refer-clojure :exclude [reify])
  (:use [ansuz.reflect])
  (:use [ansuz.monad :exclude [ret]])
  (:use [ansuz.monadplus]))

(defmacrop fail 
  "this parser simply fails

    (run (fail \"reason\")) => Fail 
  
  "
  [r] 
  (let [[str sc fl :as as] (map gensym '(str sc fl))]
    `(reflect ~(vec as)
              (fn [] (~fl ~r)))))

(defmacrop ret 
  "that's the same of `ansuz.monad/ret`"
  [w]
  `(ansuz.monad/ret ~w))

(defmacrop ? 
  "Takes in input a function, and is successful if the result of the 
  application of the function to the next token is true.
  It returns the input token.

    (run (? blank?) \" start with blank\") => the 'space' character
    (run (? blank?) \"start with s\") => Fail   
  .
  "
  [tst]
  (let [[str sc fl :as as] (map gensym '(str sc fl))
        resume (gensym 'resume)
        v1 (gensym 'v)]
    `(reflect ~(vec as)
              (letfn[(~resume [~str]
                       (cond
                        (empty? ~str) (fn [] (~fl ~resume))
                        (~tst (first ~str)) (fn [] (~sc (first ~str) (rest ~str) ~fl))
                        :else (fn [] (~fl "? failed"))))]
                (~resume ~str)))))

(defmacrop any 
  "reads any kind of input token, consumes and returns it.
  it fails only in case the stream is at an end.

    (run (any) \"\") => Fail
    (run (any) \"a\") => the 'a' character
  
  It returns the next token.
  "
  []
  (let [f (gensym 'f)]
    `(? (fn [~f] (not (= ~f ,(char 0)))))))

(defmacrop ! 
  "recognize exatly one token, i.e. it is true that 

    (run (! x) (str a b c))

  if and only if `(= x a)` returns true. The result is a.
  "
  [v]
  (let [f (gensym 'f)]
    `(? (fn [~f] (= ~f ~v)))))

(defmacrop end
  "By default run adds a null `(char 0)` at the end of a stream"
  []
  `(! ,(char 0)))
  
(defmacrop in 
  "This is meant to be an inspection tool, this parser always succeed and 
   returns the rest of the input token stream.
  "
  []
  (let [[str sc fl :as as] (map gensym '(str sc fl))]
    `(reflect ~(vec as) (fn [] (~sc ~str ~str ~fl)))))

(defmacrop callcc
  "This also is meant to be an inspection tool, it deals with the current 
   continuation in case of success. It takes a parameter, that is a parser
   that takes one argument. this argument is the success function.
   DO NOT USE IT UNLESS YOU KNOW WHAT YOU ARE DOING
  " 
  [p]
  (let [[str sc fl :as as] (map gensym '(str sc fl))
        pp (gensym 'p)]
    `(reify [~pp ~p]
       (reflect ~(vec as)
         (fn [] (~sc (with-args ~(vec as) (~pp ~sc)) ~str ~fl))))))

(defmacrop callcf
  "This also is meant to be an inspection tool, it deals with the current 
   continuation in case of failure. It takes a parameter, that is a parser
   that takes one argument. this argument is the fail function.
   DO NOT USE IT UNLESS YOU KNOW WHAT YOU ARE DOING
  "  
  [p]
  (let [[str sc fl :as as] (map gensym '(str sc fl))
        pp (gensym 'p)]
    `(reify [~pp ~p]
       (reflect ~(vec as)
         (fn [] (~sc (with-args ~(vec as) (~pp ~fl)) ~str ~fl))))))

