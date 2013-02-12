(ns ansuz.parsers.calc
  (:use [ansuz.core])
  (:use [ansuz.extra])
  (:use [ansuz.expressions :only [expr]])
  (:use [ansuz.parsers.json :only [json-number]])
  (:use [ansuz.language]))

(def number json-number)

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

(defn fact [n]
  (loop [prod 1
         counter 1]
    (if (> counter n) prod
        (recur (* counter prod)
               (inc counter)))))
               
(defparser fac []
  (many \space)
  \!
  (ret fact))

; priority table
(def table
  {:prefix [[dif 4]]
   :infix [[sum 1 :left]
           [dif 1 :left]
           [mul 2 :left]
           [div 2 :left]]
   :postfix [[sqr 3]
             [fac 3]
             ]
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
