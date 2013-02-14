(ns ansuz.monadplus
  (:refer-clojure :exclude [reify])
  (:use [ansuz.reflect])
  (:use [ansuz.monad]))

;; Orelse
;; there are 2 orelses because there is one that is fully correct that maintains a
;; reference to the branching point even when the left branch matches, and another
;; that if the orelse left branch matches the whole parser returns that.
;; it's clearer by an example.
;; suppose
;; (>> (<> ab a) b)
;; where ab matches string 'ab' , a the string 'a' and b string 'b'
;; the above expression can match
;; 'abb' and 'ab' strings.
;; but if the <> operator is the one that do not mantain the backtrace reference
;; happens that it fails when input is 'ab':
;; the parser first recognize ab, then it expects another b so if simply fails.
;; in the case of orelse* when fail looking for b call backtracking and finds
;; a then b.
;; Why two orelses? The answer is that the not perfectly correct is more efficient
;; because do not create a reference to backtracking closure for each orelse
;; when left branch succeed.
;; In any case incorrect orelse works fine for many cases, and many times when
;; do not work fine we can simply transform it. We can rewrite our example as
;; (<> (>> ab b) (>> a b))
;; that works correctly.
;; For deterministic parsers the incorrect orelse is sufficient to describe any
;; parser.

(defmacrop orelse [m n]
  (let [[str1 sc1 fl1 :as as1] (map gensym '(str sc fl))
        [str2 sc2 fl2 :as as2] (map gensym '(str sc fl))
        [mm nn rr vv] (map gensym '(mm nn rr vv))]
    `(reify [~mm ~m]
       (reify [~nn ~n]
         (reflect ~as1
           (~mm ~str1
                (fn [~vv ~str2 ~fl2] (~sc1 ~vv ~str2 ~fl1))
                (fn [~rr] (~nn ~str1 ~sc1 ~fl1))))))))

(defmacrop orelse* [m n]
  (let [[str1 sc1 fl1 :as as1] (map gensym '(str sc fl))
        [str2 sc2 fl2 :as as2] (map gensym '(str sc fl))
        [mm nn rr vv] (map gensym '(mm nn rr vv))]
    `(reify [~mm ~m]
       (reify [~nn ~n]
         (reflect ~as1
           (~mm ~str1
                ~sc1
                (fn [~rr] (~nn ~str1 ~sc1 ~fl1))))))))
