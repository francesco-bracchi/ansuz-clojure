(ns ansuz.extra
  (:refer-clojure :exclude [reify])
  (:use [ansuz.reflect])
  (:use [ansuz.core :only [!]])
  (:use [ansuz.monad])
  (:use [ansuz.monadplus])
  (:use [ansuz.language]))

(defmacrop maybe [m]
  (let[mm (gensym 'm)]
    `(reify (~mm (evalp ~m))
       (evalp (~'alt (~mm) (ret false))))))

(defmacrop many [m]
  (let[mm (gensym 'mm)
       kl (gensym 'kl)
       es (gensym 'es)
       e  (gensym 'e)]
    `(reify [~mm (evalp ~m)]
       (evalp
         (~'let [~kl (parser ~kl [~es]
                             (~'alt (~'cat (~'<- ~e (~mm))
                                           (~kl (conj ~es ~e)))
                                    (ret ~es)))]
           (~kl []))))))

(defmacrop upto [n m]
  (let[mm (gensym 'mm)
       up (gensym 'up)
       up1 (gensym 'up)
       es (gensym 'es)
       e  (gensym 'e)
       j (gensym 'j)]
    `(reify [~mm (evalp ~m)]
       (evalp
         (~'let [~up (parser ~up [~es ~j]
                             (if (<= ~j 0)
                               (ret ~es)
                               (~'alt (~'cat (~'<- ~e (~mm))
                                             (~up (conj ~es ~e) (- ~j 1)))
                                      (ret ~es))))]
           (~up [] ~n))))))

(defmacrop times [n m]
  (let[mm (gensym 'mm)
       tm (gensym 'times)
       es (gensym 'es)
       e  (gensym 'e)
       j (gensym 'j)]
    `(reify [~mm (evalp ~m)]
       (evalp
         (~'let [~tm (parser ~tm [~es ~j]
                             (if (= ~j 0) (ret ~es)
                                 (~'cat (~'<- ~e (~mm))
                                        (~tm (conj ~es ~e) (- ~j 1)))))]
           (~tm [] ~n))))))

(defmacrop at-least [n m]
  (let[mm (gensym 'mm)
       h  (gensym 'h)
       t  (gensym 't)]
    `(letpar [(~mm [] (evalp (~'cat ~m)))]
             (evalp
              (~'cat (~'<- ~h (times ~n (~mm)))
                     (~'<- ~t (many (~mm)))
                     (ret (concat ~h ~t)))))))

(defmacrop at-least&most [n m p]
  (let[mm (gensym 'mm)
       h  (gensym 'h)
       t  (gensym 't)]
    `(letpar [(~~mm (evalp (~'cat ~p)))]
             (evalp 
              (~'cat (~'<- ~h (times ~n (~mm)))
                     (~'<- ~t (upto (- ~m ~n) (~mm)))
                     (ret (concat ~h ~t)))))))
