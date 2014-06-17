(ns emm-ess-pee.core
  (:use emm-ess-pee.binary-utils
        clojure.repl)
  (:require [clojure.pprint :refer [pprint]])
  (:gen-class))


(defn make-computer []
  {:registers (vec (repeat 16 (make-word 0)))
   :memory (vec (repeat 20 (make-byte 0)))})


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
