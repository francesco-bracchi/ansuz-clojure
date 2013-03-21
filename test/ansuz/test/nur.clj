(ns ansuz.test.nur
  (:use [ansuz.core])
  (:use [clojure.pprint])
  (:use [ansuz.language])
  (:use [clojure.test]))

(defn feed [p s] ((p 0) s))

(deftest test-nur
  (let [p (parser [] (cat (ret (println "0"))
                          \a
                          (ret (println "A"))
                          \b
                          (ret (println "B"))
                          \c
                          (ret (println "C"))))
        sc (fn [v str fl] [false])
        fl (fn [v]
             (if (fn? v) 
               [(fn [s] (trampoline (v s)))]
               (throw (Error. "not found"))))
        sa [#(trampoline (p %1 sc fl))]]
    (feed sa "a")
    (feed (feed sa "a") "bc")))
    
    
        
