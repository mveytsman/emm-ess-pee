(ns emm-ess-pee.instruction-set
  (:use emm-ess-pee.binary-utils
        emm-ess-pee.core
        [clojure.core.match :only [match]]))

(defn RPC [source])
(defn SWPB [source])
(defn RRA [])
(defn SXT [])
(defn PUSH [])
(defn CALL [])
(defn RETI [])


(defn execute-instruction [computer [op byte? source-mode register]]
  (case source-mode
    "00" (let [val (get-reg computer register)]
           (set-reg computer register (op val)))))

(def opcodes1 {"000" RPC
               "001" SWPB
               "010" RRA
               "011" SXT
               "100" PUSH
               "101" CALL
               "110" RETI})

(defn fetch-instruction [computer]
  (let [wrd (get-indirect computer (named-register :pc))]
    [(little-endian wrd)]))

(defn parse-instruction [wrd]
  (let [wrd (int->binstr (little-endian wrd))]
    (if-let [[_ opcode byte? source-mode registxer] (re-matches #"^000100([01]{3})([01])([01]{2})([01]{4})$" wrd)]
      (let [ opcode (get opcodes1 opcode)
            byte? (= byte? "1")
            register (binstr->int register)]
        [opcode byte? source-mode register])
      "NOPE")))


(defn print-instruction [name byte-addr? address register])
