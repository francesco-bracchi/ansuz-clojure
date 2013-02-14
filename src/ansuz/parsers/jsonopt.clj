(ns ansuz.parsers.jsonopt
  (:use [ansuz.core])
  (:use [ansuz.extra])
  (:use [ansuz.reflect :only [reflect]])
  (:use [ansuz.language]))

;; this file is the same of json.clj, except some parsers are manually optimized,
;; in particular string parser, and spaces.
;; the problem is that it violates abstraction borders, therefore changes in 
;; the underlying implementation can break this code.
;; A better way for increasing performance could be feeding the parser with tokens
;; that come from a lexer.

(declare json-value)

(defparser spaces []
  (reflect [st sc fl]
           (loop [st st]
             (let [c (first st)]
             (cond
              (= c \space) (recur (rest st))
              (= c \newline) (recur (rest st))
              (= c \tab) (recur (rest st))
              :else (sc true st fl))))))

(defparser json-true []
  \t \r \u \e
  (ret true))

(defparser json-false []
  \f \a \l \s \e
  (ret false))

(defparser json-null []
  \n \u \l \l
  (ret nil))

(def idigit 
  (let [i0 (int \0)]
    (parser []
            (<- c (any))
            (let [d (- (int c) i0)]
              (if (and (>= d 0) (<= d 9))
                (ret d)
                (fail nil))))))

(defparser int-more [c]
  (alt (cat (<- c1 (idigit))
            (int-more (+ c1 (* 10 c))))
       (ret c)))

(defparser intp []
  (<- d (idigit))
  (int-more d))

(defparser frac-more [m c0]
  (alt (cat (<- c (idigit))
            (frac-more (/ m 10) (+ (* m c) c0)))
       (ret c0)))

(defparser frac []
  \.
  (<- d (idigit))
  (frac-more (/ 1 100) (/ d 10)))

(defparser sign []
  (alt (cat \+ (ret (fn [x] x)))
       (cat \- (ret (fn [x] (- x))))
       (ret (fn [x] x))))

(defparser pow10 []
  (cat (alt \e \E)
       (<- s (sign))
       (<- i (intp))
       (ret (s i))))

(defparser json-number []
  (<- s (sign))
  (<- ip (intp))
  (<- fp (alt (frac) (ret 0)))
  (<- ex (alt (pow10) (ret 0)))
  (ret  (s (if (= 0 fp)
             ip
             (* (+ ip fp) (Math/pow 10 ex))))))

(defparser json-string []
  (reflect [st sc fl]
           (let [c (first st)]
             (if (= c \")
               (loop [s []
                      st (rest st)]
                 (let[c (first st)]
                   (cond
                    (not c) (fl "json string incomplete")
                    (= c \") (sc (apply str s) (rest st) fl)
                    (= c \\)
                    (let [c (first st) st (rest st)]
                      (cond
                       (not c) (fl "json string incomplete")
                       (= c \") (recur (conj s \") (rest st))
                       (= c \/) (recur (conj s \/) (rest st))
                       (= c \\) (recur (conj s \\) (rest st))
                       (= c \b) (recur (conj s \backspace) (rest st))
                       (= c \f) (recur (conj s \formfeed) (rest st))
                       (= c \n) (recur (conj s \newline) (rest st))
                       (= c \r) (recur (conj s \return) (rest st))
                       (= c \u) (let [x (apply str (take 4 (rest st)))
                                      v (char (.intValue (java.lang.Integer/valueOf x 16)))]
                                  (recur (conj s v) (drop 5 st)))
                       :else (fl "unknow excape character")))
                    :else (recur (conj s c) (rest st)))))
               (fl "not a string")))))

(defparser json-object-pairs [map]
  (<- k (json-string))
  (spaces)
  \:
  (spaces)
  (<- v (json-value))
  (spaces)
  (let [new (assoc map k v)]
    (alt (cat \, (spaces) (json-object-pairs new))
         (ret new))))

(defparser json-object []
  \{
  (spaces)
  (<- as (json-object-pairs {}))
  (spaces)
  \}
  (ret as))

(defparser json-array0 [vec]
  (<- v (json-value))
  (spaces)
  (let [new (conj vec v)]
    (alt (cat \, (spaces) (json-array0 new))
         (ret new))))

(defparser json-array []
  \[
  (spaces)
  (<- as (json-array0 []))
  (spaces)
  \]
  (ret as))

(defparser json-value []
  (alt (json-string)
       (json-object)
       (json-array)
       (json-true)
       (json-false)
       (json-null)
       (json-number)))
        
(defparser json []
  (spaces)
  (<- v (json-value))
  (spaces)
  (end)
  (ret v))

(defn parse [s] (run (json) s))
