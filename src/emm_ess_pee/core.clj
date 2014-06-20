(ns emm-ess-pee.core
  (:use emm-ess-pee.binary-utils
        emm-ess-pee.computer
        emm-ess-pee.instruction-set
        emm-ess-pee.pprint
        clojure.repl)
  (:require [clojure.pprint :refer [pprint]])
  (:gen-class))


;; Interactive "debugger" in the repl
(def computer-state
  "Atom that holds the computer state for us"
  (atom (make-computer)))

(def hello-world-code
  "Some hello world codez"
  [0x31 0x40 0x00 0x44 0x15 0x42 0x5C 0x01 0x75
   0xF3 0x35 0xD0 0x08 0x5A 0x3F 0x40 0x00 0x00
   0x0F 0x93 0x07 0x24 0x82 0x45 0x5C 0x01 0x2F
   0x83 0x9F 0x4F 0x4A 0x44 0x00 0x24 0xF9 0x23
   0x3F 0x40 0x00 0x00 0x0F 0x93 0x06 0x24 0x82
   0x45 0x5C 0x01 0x1F 0x83 0xCF 0x43 0x00 0x24
   0xFA 0x23 0x0F 0x43 0x32 0xD0 0xF0 0x00 0xFD
   0x3F 0x30 0x40 0x48 0x44 0x1F 0x43 0x30 0x41
   0x00 0x13])

(defn load-code
  "Puts a vector of bytes at address 0x4400 (which is where execution will begin)"
  [code]
  (swap! computer-state #(set-bytes % 0x4400 code)))

(defn reset
  "Reset the execution state"
  []
  (swap! computer-state (constantly (make-computer)))
  (load-code hello-world-code)
  (print-registers (:registers @computer-state)))
(def r reset)

(defn step
  "Takes a step"
  []
  (println "Executing Instruction:")
  (swap! computer-state #(apply execute-instruction (fetch-instruction %)))
  (print-registers (:registers @computer-state)))
(def s step)

(defn continue
  "Continues execution until exit"
  []
  (loop []
    (step)
    (if (continue-execution? @computer-state)
      (recur))))
(def c continue)

(defn help
  "Interactive help function"
  []
  (println "This is a clojure REPL that contains some commands for interacting with the emulator")
  (println "(step) or (s) - step through the debugger")
  (println "(reset) or (r) reset the processor's state")
  (println "(continue) or (c) - COMING SOON!"))

(defn -main
  "Our debugger is a repl"
   [& args]
   (println "Welcome to the MAP430")
   (println "Try (help)")
   (clojure.main/repl :prompt #(print "MSP430=> ")
                      :init (fn [] (in-ns 'emm-ess-pee.core)))) 
   
