(ns ansuz.parsers.json
  (:use [ansuz.core])
  (:use [ansuz.extra])
  (:use [ansuz.reflect :only [reflect]])
  (:use [ansuz.language]))

(declare json-value)

(defparser spaces []
  (many (alt \space
             \newline
             \tab)))

(defparser json-true []
  \t \r \u \e
  (ret true))

(defparser json-false []
  \f \a \l \s \e
  (ret false))

(defparser json-null []
  \n \u \l \l
  (ret nil))

(defparser idigit []
  (alt (cat \0 (ret 0))
       (cat \1 (ret 1))
       (cat \2 (ret 2))
       (cat \3 (ret 3))
       (cat \4 (ret 4))
       (cat \5 (ret 5))
       (cat \6 (ret 6))
       (cat \7 (ret 7))
       (cat \8 (ret 8))
       (cat \9 (ret 9))))

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

(defparser numb []
  (<- s (sign))
  (<- ip (intp))
  (<- fp (alt (frac) (ret 0)))
  (<- ex (alt (pow10) (ret 0)))
  (ret  (s (if (= 0 fp)
             ip
             (* (+ ip fp) (Math/pow 10 ex))))))

(defparser json-number []
  (<- n (numb))
  (ret n))

(defparser hex []
  (alt (idigit)
       (cat (alt \a \A) (ret 10))
       (cat (alt \b \B) (ret 11))
       (cat (alt \c \C) (ret 12))
       (cat (alt \d \D) (ret 13))
       (cat (alt \e \E) (ret 14))
       (cat (alt \f \F) (ret 15))))
       
(defparser unicode-char []
  (<- [n0 n1 n2 n3] (times (hex) 4))
  (ret (char (+ n3 (* n2 16) (* n1 256) (* n0 4096)))))
  
(defparser json-special-char []
  \\
  (alt (cat \" (ret \"))
       (cat \\ (ret \\))
       (cat \/ (ret \/))
       (cat \b (ret \backspace))
       (cat \f (ret \formfeed))
       (cat \n (ret \newline))
       (cat \r (ret \return))
       (cat \t (ret \tab))
       (cat \u (unicode-char))
       ))

(defparser json-normal-char []
  (? (fn [c] (not (= c \")))))

(defparser json-char []
  (alt (json-special-char)
       (json-normal-char)))

(defparser json-string []
  \"
  (<- s (many (json-char)))
  \"
  (ret (apply str s)))

(defparser json-object-pairs [map]
  (<- k (json-string))
  (spaces)
  \:
  (spaces)
  (<- v (json-value))
  (spaces)
  (let [new (assoc map (keyword k) v)]
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
  (alt (json-object)
       (json-array)
       (json-true)
       (json-false)
       (json-null)
       (json-string)
       (json-number)))
        
(defparser json []
  (spaces)
  (<- v (json-value))
  (spaces)
  (end)
  (ret v))

(defn parse [s]
  (run (json) s))


