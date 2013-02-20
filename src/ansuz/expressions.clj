;; # Expressions parser 
;; Recursive descent parsers are known to be difficult for expressions with infix operators.
;; this library amims simplifying the burden involved in it.
;; There is an `expr` parser, to which can be passed 2 parameters, an operator priority table,
;; and a term parser.
;; The term parser parse atomic terms (or parenthesis surrounded expressions) while the 
;; operator table provides a list of parsers with priority and associativity. This example taken
;; from ansuz.parsers.calc shows priorities for standard arithmetic functions.
;;
;;     (def table
;;       {:prefix [[dif 4]]
;;        :infix [[sum 1 :left]
;;                [dif 1 :left]
;;                [mul 2 :left]
;;                [div 2 :left]]
;;        :postfix [[sqr 3]
;;                  [fac 3]]
;;        })
;; 
;; In this example `sum`, `dif` ... are parsers, with no input that returns a function in the form
;; `(ret [(fn [a b] ...)])`. It is wrapped in a vector because if run by itself, like `(run (dif) "-")`
;; if the function will not be wrapped it will generate an error because of trampoline called in running.
;; for example:
;;
;;     (defparser dif 
;;       []
;;       (manu \space)
;;       \-
;;       (ret [-]))
;;
;; for an example see [ansuz.parsers.calc](#ansuz.parsers.calc).
;; 
(ns ansuz.expressions
  (:use [ansuz.core])
  (:use [ansuz.language]))

(defparser prefix* 
  "try to read a prefix operator amongst ts"
  [ts]
  (if (empty? ts) (fail "prefix failed")
      (let[[parser prec] (first ts)]
        (alt (cat (<- [func] (parser))
                  (ret [prec func]))
             (prefix* (rest ts))))))

(defparser infix* 
  "try to read am infix operator amongst ts"
  [ts]
  (if (empty? ts) (fail "infix failed")
      (let[[parser prec assoc] (first ts)]
        (alt (cat (<- [func] (parser)) (ret [prec func assoc]))
             (infix* (rest ts))))))

(defparser postfix* 
  "try to read am postfix operator amongst ts"
  [ts]
  (if (empty? ts) (fail "postfix failed")
      (let[[parser prec] (first ts)]
        (alt (cat (<- [func] (parser)) (ret [prec func]))
             (postfix* (rest ts))))))

(defparser prefix 
  "try to parse a prefix operator according to op-table provided"
  [op-table]
  (prefix* (:prefix op-table)))

(defparser infix 
  "try to parse an infix operator according to op-table provided"
  [op-table]
  (infix* (:infix op-table)))

(defparser postfix 
  "try to parse a postfix operator according to op-table provided"
  [op-table]
  (postfix* (:postfix op-table)))

(declare term)
(declare expr-more)

(defparser expr* 
  [p op-table termp]
  (<- t (term op-table termp))
  (expr-more p t op-table termp))

(defparser infix-expr-more [p t0 op-table termp]
  (<- [prec func assoc] (infix op-table))
  (if (< prec p) (fail "infix expr more")
      (cat (<- t1 (expr* (if (= assoc :left) (+ 1 prec) prec)
                         op-table
                         termp))
           (expr-more p (func t0 t1) op-table termp))))

(defparser postfix-expr-more [p t0 op-table termp]
  (<- [prec func] (postfix op-table))
  (if (< prec p)
    (fail "postfix expr more")
    (expr-more p (func t0) op-table termp)))

(defparser prefix-expr [op-table termp]
  (<- [prec func] (prefix op-table))
  (<- e (expr* prec op-table termp))
  (ret (func e)))

(defparser term [op-table termp]
  (alt (prefix-expr op-table termp)
       (termp)))

(defparser expr-more [p t0 op-table termp]
  (alt (postfix-expr-more p t0 op-table termp)
       (infix-expr-more p t0 op-table termp)
       (ret t0)))

(defparser expr 
  "parse an expression according to the provided op-table (operator priority table)
  and according to the termp parser (term parser)
  see ansuz.parsers.calc for an usage example.
  "
  [op-table termp] (expr* 0 op-table termp))
