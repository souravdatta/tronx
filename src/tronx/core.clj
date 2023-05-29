(ns tronx.core
  (:use tronx.eval)
  (:gen-class))

(defn repl [prompt env]
  (loop []
    (print prompt " ")
    (flush)
    (let [code (read)
          e (tronx-eval code env)]
      (println
        (if (or
              (instance? clojure.lang.Atom e)
              (map? e))
          ""
          e)))
    (recur)))

(defn -main [& args]
  (println "hello tronx")
  (repl "~>" (new-env)))

(comment
  (repl "tronx>> " (new-env))

  )