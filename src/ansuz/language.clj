;; # DSL Language describing parsers
;;
;; ## defining a new named parser
;; the macro **defparser** resemble defn syntax, therefore it support
;; documentation and variadic arguments as well.
;;
;; ## a parser 'value'
;; An anonymous parser can be created using the **parser** macro that 
;; resemple fn syntax.
;;
;; ## the definition language
;; these are the rewrite rules used in `(evalp <exp>)
;; 
;;  * `(cat <a> <b> ...)` => `(bind [_ (evalp <a>)] (evalp (cat <b> ...))`
;;
;;  * `(alt <a> <b> ...)` => `(orelse (evalp <a>) (evalp (alt <b> ...))`
;;
;;  * `(alt* <a> <b> ...)` => `(orelse* (evalp <a>) (evalp (alt* <b> ...))`
;;
;;  * `(let [...] <exp> ...)` => (let [...] (evalp (cat <expr> ...)))`
;;
;;  * `(if test? <a> <b>)` => `(if test? (evalp <a>) (evalp <b>))`
;;
;;  * `(cond testa? <a> testb? <b> ...)` => `(cond testa? (evalp <a>) testb? (evalp <b>) ...)`
;;
;;  * `(<p> ...)` => stays the same.
;;
;;  *  `<token>` where p is not a sequence => `(! <token>)`
;; 
;; `cat` `alt` `alt*` are the variadic versions of `bind` `orelse` `orelse*`,
;; see [ansuz.monad](#ansuz.monad) and [ansuz.monadplus](#ansuz.monadplus).
;;
;; `let` `if` `cond` resembles the corresponding clojure keywords.
;; 
;; If a value is provided (i.e. not a sequence) it is considered as a parser that 
;; matches exactly that token from input stream, hence it is transormed in `(! <token>)`.
;; see [ansuz.core](#ansuz.core) for details on `!` operator.
;; 
(ns ansuz.language
  (:refer-clojure :exclude [reify])
  (:use [ansuz.core :only [! fail]])
  (:use [ansuz.reflect])
  (:use [ansuz.monad])
  (:use [ansuz.monadplus]))

(defn evalp-cat 
  "rewriter for `cat` expressions"
  [e & es]
  (cond
   (empty? es)
   `(evalp ~e)
   (and (seq? e) (= (first e) '<-))
   (let[[_ n m] e]`(bind [~n (evalp ~m)] (evalp (~'cat ~@es))))
   :else
   `(bind [ignore# (evalp ~e)] (evalp (~'cat ~@es)))))

(defn evalp-alt 
  "rewriter for `alt` expressions"
  [e & es]
  (if (empty? es)
    `(evalp ~e)
    `(orelse (evalp ~e)
             (evalp (~'alt ~@es)))))

(defn evalp-alt* 
  "rewriter for `alt*` expressions"
  [e & es]
  (if (empty? es)
    `(evalp ~e)
    `(orelse* (evalp ~e)
              (evalp (~'alt* ~@es)))))

(defn evalp-let 
  "rewriter for `let` expressions"
  [v & es]
  (let [[str sc fl :as as] (map gensym '(str sc fl))
        ee (gensym 'es)]
    `(reify [~ee (evalp (~'cat ~@es))]
       (reflect ~as
         (let ~v (with-args ~as (~ee)))))))

(defn evalp-if 
  "rewriter for `if` expressions"
  [t? l r]
  (let [ll (gensym 'l)
        rr (gensym 'r)
        [str sc fl :as as] (map gensym '(str sc fl))]
    `(reify [~ll (evalp ~l)]
       (reify [~rr (evalp ~r)]
         (reflect ~(vec as)
           (if ~t?
             (with-args ~(vec as) (~ll))
             (with-args ~(vec as) (~rr))))))))

(defn evalp-cond 
  "rewriter for `cond` expressions"
  [& es]
  (if (empty? es)
    `(fail "cond failed")
    (let[[t? m & xs] es]
      `(evalp (~'if ~t? ~m (~'cond ~@xs))))))

(def parser-operators
  {'cat   evalp-cat
   'alt   evalp-alt
   'alt*  evalp-alt*
   'let   evalp-let
   'if    evalp-if
   'cond  evalp-cond
   })

(defmacrop evalp 
  "evalp macro. The expression put inside this macro will be 
  treated according to the ansuz language dsl, see [ansuz.language](#ansuz.language) description"
  [e]
  (if (not (seq? e))
    `(! ~e)
    (let [op ((first e) parser-operators)]
      (if op
        (apply op (next e))
        e))))

(defn signature-with-evalp [sig]  
  (let [[params & body] sig
        conds (when (and (next body) (map? (first body)))
                (first body))
        body (if conds (next body) body)]
    (if conds
      `(~params ~conds (evalp (~'cat ~@body)))
      `(~params (evalp (~'cat ~@body))))))

(defmacro parser 
  "this macro creates a new anonymous parser"
  [& sigs]
  (let [name (if (symbol? (first sigs)) (first sigs) nil)
        sigs (if name (rest sigs) sigs)
        sigs (if (vector? (first sigs)) (list sigs) sigs)
        sigs (map signature-with-evalp sigs)]
    (if name
      `(par ~name ~@sigs)
      `(par ~@sigs))))

(defmacro defparser 
  "this macro creates a new global named parser"
  [name & decl]
  (let [pre (if (string? (first decl)) (list (first decl)) '())
        decl (if (string? (first decl)) (next decl) decl)
        pre (if (map? (first decl)) (concat pre (first decl)) pre)
        decl (if (map? (first decl)) (next decl) decl)
        decl (if (vector? (first decl)) (list decl) decl)
        post (if (map? (last decl)) (list (last decl)) '())
        decl (if (map? (last decl)) (butlast decl) decl)
        decl (map signature-with-evalp decl)]
    `(defpar ~name ~@(concat pre decl post))))

(defmacro run
  "Once created your parser you need to run it.
  the input p is a parser expression according to [ansuz.language](#ansuz-language) syntax.
  a third optional parameter provides a failure action, it has to be a lambda expression"
  ([p src]
     `(run ~p ~src #(throw (Error. %))))
  
  ([p src fail]
     (let[v (gensym 'v)
          r (gensym 'r)
          [str sc fl] (map gensym '(str sc fl))
          _sc `(fn [~v ~str ~fl] ~v)
          _fl fail]
       `(trampoline (with-args [ ~src ~_sc ~_fl] (evalp ~p))))))

(defmacro run-ndet
  "that's the same of run but returns a lazy sequence, that contains the 
  list of all results parsing the input. i.e. `(cat (alt (cat a b) a) b)
  against 'abb' returns 2 values ('abb' 'ab') while with run only 'abb'."
  [p src]
  (let[v (gensym 'v)
       r (gensym 'r)
       [str sc fl] (map gensym '(str sc fl))
       _sc `(fn [~v ~str ~fl] (lazy-seq (cons ~v (lazy-seq (trampoline ~fl nil)))))
       _fl `(fn [~r] nil)]
    `(lazy-seq (trampoline (with-args [ ~src ~_sc ~_fl] (evalp ~p))))))
