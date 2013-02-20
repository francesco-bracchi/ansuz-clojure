;; # Simple Calculator
;; this is a simple mathematical expression parser.
;; It supports +,-,*,/,^(squared),!(factorial) operators over float numbers.
;; 
(ns ansuz.parsers.calc
  (:use [ansuz.core])
  (:use [ansuz.extra])
  (:use [ansuz.expressions :only [expr]])
  (:use [ansuz.parsers.json :only [json-number]])
  (:use [ansuz.language]))

(def number json-number)

(defparser spaces
  "remove whitespaces"
  []
  (many \space))

(defparser sum 
  "recognize + infix operator, removing trailing whitespaces"
  []
  (spaces)
  \+
  (ret [+]))

(defparser dif 
  "recognize `-` prefix/infix operator, removing trailing whitespaces"
  []
  (spaces)
  \-
  (ret [-]))

(defparser mul
  "recognize `*` infix operator, removing trailing whitespaces"
  []
  (spaces)
  \*
  (ret [*]))

(defparser div
  "recognize `-` infix operator, removing trailing whitespaces"
  []
  (spaces)
  \/
  (ret [/]))

(defparser sqr
  "recognize `^` (squared) postfix operator, removing trailing whitespaces"
  []
  (spaces)
  \^
  (ret [#(* % %)]))

;; factorial of negative number is 1?
(defn factorial [n]
  (loop [prod 1
         counter 1]
    (if (> counter n) prod
        (recur (* counter prod)
               (inc counter)))))
               
(defparser fac 
  "recognize `!` (factorial) postfix operator, removing trailing whitespaces"
  []
  (spaces)
  \!
  (ret [factorial]))

;; ##priority table
;; this is the table that controls expression behavior:
;;
;;     <expression> ::= <term>
;;                   |  '-' <expression>
;;                   |  <expression> '+' <expression>
;;                   |  <expression> '-' <expression>
;;                   |  <expression> '*' <expression>
;;                   |  <expression> '/' <expression>
;;                   |  <expression> '^'
;;                   |  <expression> '!'
;;
;; see **term** parser for <term> grammar
;; the operator priority is provided by the second number.
(def table
  {:prefix [[dif 4]]
   :infix [[sum 1 :left]
           [dif 1 :left]
           [mul 2 :left]
           [div 2 :left]]
   :postfix [[sqr 3]
             [fac 3]]
   })

(declare term)

(defparser pars 
  "parse an expression enclosed in parenthesis"
  []
  \(
  (<- e (expr table term))
  (spaces)
  \)
  (ret e))

;; this is the parser passed to the **expr** parser provided by
;; **ansuz.expression** namespace 
;;
;;     <term> -> <number> | '(' <expression> ')'
;; 
(defparser term 
  "parse a term i.e. a number or a bracket enclosed expression"
  []
  (spaces)
  (alt (number)
       (pars)))

(defparser math-expr 
  "parse a mathematical expression, remove whitespace, then this parser expect
  the end of stream"
  []
  (<- r (expr table term))
  (spaces)
  (alt (end) (fail "end expected"))
  (ret r))

(defparser endp 
  "recognize the `end` string as a single command"
  []
  (spaces)
  \e \n \d
  (spaces)
  (end)
  (ret :end))

(defparser calcp 
  "recognize an expression or the `end` keyword"
  []
  (alt (math-expr)
       (endp)))

(defn parse 
  "run the calc parser on charcter stream s (that can be, btw a string)"
  [s]
  (run (calcp) s (fn [x] :fail)))

(defn repl 
  "start read eval print loop"
  []
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
