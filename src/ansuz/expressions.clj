(ns ansuz.expressions
  (:use [ansuz.core])
  (:use [ansuz.language]))

(defparser prefix* [ts]
  (if (empty? ts) (fail "prefix failed")
      (let[[parser prec] (first ts)]
        (alt (cat (<- func (parser))
                  (ret [prec func]))
             (prefix* (rest ts))))))

(defparser infix* [ts]
  (if (empty? ts) (fail "infix failed")
      (let[[parser prec assoc] (first ts)]
        (alt (cat (<- func (parser)) (ret [prec func assoc]))
             (infix* (rest ts))))))

(defparser postfix* [ts]
  (if (empty? ts) (fail "postfix failed")
      (let[[parser prec] (first ts)]
        (alt (cat (<- func (parser)) (ret [prec func]))
             (postfix* (rest ts))))))

(defparser prefix [op-table]
  (prefix* (:prefix op-table)))

(defparser infix [op-table]
  (infix* (:infix op-table)))

(defparser postfix [op-table]
  (postfix* (:postfix op-table)))

(declare term)
(declare expr-more)

(defparser expr* [p op-table termp]
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

;; main
(defparser expr [op-table termp] (expr* 0 op-table termp))
