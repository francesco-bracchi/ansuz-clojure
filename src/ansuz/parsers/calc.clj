(ns ansuz.parsers.calc
  (:use [ansuz.core])
  (:use [ansuz.extra])
  (:use [ansuz.expressions :only [expr]])
  (:use [ansuz.language]))

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
  (ret (s (* (+ ip fp) (Math/pow 10 ex)))))

(defparser number []
  (<- n (numb))
  (ret n))

(defparser sum []
  (many \space)
  \+
  (ret +))

(defparser dif []
  (many \space)
  \-
  (ret -))

(defparser mul []
  (many \space)
  \*
  (ret *))

(defparser div []
  (many \space)
  \/
  (ret /))

(defparser sqr []
  (many \space)
  \^
  (ret #(* % %)))

; priority table
(def table
  {:prefix [[dif 1]]
   :infix [[sum 1 :left]
           [dif 1 :left]
           [mul 2 :left]
           [div 2 :left]]
   :postfix [[sqr 3]]
   })

(declare term)

(defparser pars []
  \(
  (<- e (expr table term))
  (many \space)
  \)
  (ret e))

(defparser term []
  (many \space)
  (alt (number)
       (pars)))

(defparser math-expr []
  (<- r (expr table term))
  (many \space)
  (alt (end) (fail "end expected"))
  (ret r))

(defparser endp []
  (many \space)
  \e \n \d
  (many \space)
  (end)
  (ret :end))

(defparser calcp []
  (alt (math-expr)
       (endp)))

(defn parse [s]
  (run (calcp) s (fn [x] :fail)))

(defn repl []
  (map println
       ["press ctrl-D to exit"
        "avaible commands:"
        "+ - * / (? is the unary operator for quare)"])
  (loop []
    (print "? ")
    (flush)
    (let[val (parse (read-line))]
      (cond
       (= val :fail)
       (do (print "not well formed expression\n")
           (recur))
       (= val :end)
       (print "end")
       :else
       (do (print "=" val "\n")
           (recur))))))

