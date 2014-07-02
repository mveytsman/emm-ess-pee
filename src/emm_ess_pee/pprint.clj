(ns emm-ess-pee.pprint
  (:use emm-ess-pee.binary-utils
        emm-ess-pee.computer))

(defn print-op
  "Prints the name of an OP along with an optional .b"
  [op byte?]
  (str (name op) (if byte? ".b")))

(defn print-registers
  "Display all the registers"
  [registers]
  (println)
  (println "Register State:")
  (println (apply str (interleave  ["PC: " " SP: " " SR: " " R3: " "\nR4: " " R5: " " R6: " " R7: " "\nR8: " " R9: " " R10: " " R11: " "\nR12: " " R13: " " R14: " " R15: "]
                                    (map #(str (int->hexstr %)) registers)))))

(defn print-register-access
  "Returns a string displaying register access mode and register (for disassembles)"
  [computer mode register]
  (let [reg-name (case register
                   0 "PC"
                   1 "SP"
                   2 "SR"
                   (str "R" register))
        reg-display (case mode
                      :direct reg-name
                      :indexed (str
                                (get-word-indirect computer 0)
                                "(" reg-name ")")
                      :indirect (str "@" reg-name)
                      :indirect-increment (str "@" reg-name "+" "("(get-word-indirect computer register) ")"))]
    (str reg-display )))


(defn print-unary-op
  "Prints a operation of one operand along with its arguments"
  [opcode computer byte? source-mode register]
  (let [op (print-op opcode byte?)
        reg (print-register-access computer source-mode register)
        value 1234;(get-value computer :direct  register)
        ]
    (println op reg)));" [" value "]")))




(defn print-bin-op
  "Prints an operation of two arguments along with its arguments"
  [op computer byte? source-mode source-reg dest-mode dest-reg]
  (let [op (print-op op byte?)
        reg1 (print-register-access computer source-mode source-reg)
        value1 (first (get-value computer source-mode byte? source-reg))
        reg2 (print-register-access computer dest-mode dest-reg)
        ]
    (println op reg1 #_(str "[" value1 "]") "," reg2)))



(defn print-jmp
  "Print a JMP instruction along with the offset"
  [cnd offset]
  (println (name cnd) "$" offset))
