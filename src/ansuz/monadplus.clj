;; #monad sum (orelse)
;; there are 2 orelses because there is one that is fully correct that maintains a
;; reference to the branching point even when the left branch matches, and another
;; that if the orelse left branch matches the whole parser returns that.
;; it's clearer by an example.
;;
;; suppose
;;
;;     (cat (alt ab a) b)
;;
;; where `ab` matches string `"ab"` , `a` the string `"a"` and `b` string `"b"`
;; the above expression can match
;; `"abb"` and `"ab"` strings.
;;
;; But if the alt operator is `orelse`, the one that do not mantain the backtrace reference,
;; happens that it fails when input is 'ab':
;; the parser first recognize ab, then it expects another b so if simply fails.
;; in the case of orelse* when fail looking for b call backtracking and finds
;; a then b.
;; 
;; Why two orelses? Because `orelse` is more efficient in terms of memory than `orelse*`
;; and it is correct in almost any case.
;;
(ns ansuz.monadplus
  (:refer-clojure :exclude [reify])
  (:use [ansuz.reflect])
  (:use [ansuz.monad]))

(defmacrop orelse
  "this is a parser combinator, it takes in input 2 parsers and outputs a new parser
  that parse an alternative between the input. This orelse is more performant than 
  **orelse*** but it can be incorrect."
  [m n]
  (let [[str1 sc1 fl1 :as as1] (map gensym '(str sc fl))
        [str2 sc2 fl2 :as as2] (map gensym '(str sc fl))
        [mm nn rr vv] (map gensym '(mm nn rr vv))]
    `(reify [~mm ~m]
       (reify [~nn ~n]
         (reflect ~as1
           (~mm ~str1
                (fn [~vv ~str2 ~fl2] (~sc1 ~vv ~str2 ~fl1))
                (fn [~rr] (~nn ~str1 ~sc1 ~fl1))))))))

(defmacrop orelse* 
  "this is a parser combinator, it takes in input 2 parsers and outputs a new parser
  that parse an alternative between the input. This orelse* is less performant than 
  ** orelse ** but it is always correct."
  [m n]
  (let [[str1 sc1 fl1 :as as1] (map gensym '(str sc fl))
        [str2 sc2 fl2 :as as2] (map gensym '(str sc fl))
        [mm nn rr vv] (map gensym '(mm nn rr vv))]
    `(reify [~mm ~m]
       (reify [~nn ~n]
         (reflect ~as1
           (~mm ~str1
                ~sc1
                (fn [~rr] (~nn ~str1 ~sc1 ~fl1))))))))
    
