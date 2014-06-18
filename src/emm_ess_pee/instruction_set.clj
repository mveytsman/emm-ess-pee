(ns emm-ess-pee.instruction-set
  (:use emm-ess-pee.binary-utils
        emm-ess-pee.core
        emm-ess-pee.computer
        [clojure.core.match :only [match]]))

(defn fetch-instruction
  "Returns [instruction, computer] where instruction is the word at PC, and computer has the PC register incremented"
  [computer]
  (let [instruction (get-word-indirect computer (named-register :pc))]
    [instruction, (inc-pc computer)]))

(def single-op-codes {"000" :RPC
                      "001" :SWPB
                      "010" :RRA
                      "011" :SXT
                      "100" :PUSH
                      "101" :CALL
                      "110" :RETI})

(def condition-codes {"000" :JNE
                      "001" :JEQ
                      "010" :JNC
                      "011" :JC
                      "100" :NC
                      "101" :JGE
                      "110" :JL
                      "111" :JMP})

(def dual-op-codes {"0100" :MOV
                    "0101" :ADD
                    "0110" :ADDC
                    "0111" :SUBC
                    "1000" :SUB
                    "1001" :CMP
                    "1010" :DADD
                    "1100" :BIT
                    "1101" :BIS
                    "1110" :XOR
                    "1111" :AND})


(def conditions {:JNE #(= (Z %) 0)
                 :JEQ #(= (Z %) 1)
                 :JNC #(= (C %) 0)
                 :JC  #(= (C %) 1)
                 :JN  #(= (N %) 1)
                 :JGE #(= (N %1) (V %1))
                 :JL  #(not= (N %1) (V %1))
                 :JMP (constantly true)})

(def source-modes {"00" :direct
                   "01" :indexed
                   "10" :indirect
                   "11" :indirect-increment})

(def dest-modes {"0" :direct
                 "1" :indirect})

(defmulti get-value
  "A multimethod for a getter that dispatches on address mode. Returns [value, computer]"
  (fn [_ source-mode _] source-mode))
(defmethod get-value :direct
  [computer _ register]
  [(get-reg computer register) computer])
(defmethod get-value :indexed
  [computer _ register]
  (let [[offset, computer] (fetch-instruction computer)]
    [(get-word-indexed computer register offset) computer]))
(defmethod get-value :indirect
  [computer _ register]
  [(get-word-indirect computer register) computer])
(defmethod get-value :indirect-increment
  [computer _ register]
  [(get-word-indirect computer register) (inc-reg computer register) ])

;; single-op is a multimethod that performs a single operand OP. OP is parameterized
;; by the first argument (a symbol)
;; These always puts the result in the register directly, which is a bug I believe?
(defmulti single-op (fn [op _ _ _ _] op))
(defmethod single-op :RPC
  [_ computer byte? source-mode register]
  (let [[val computer] (get-value computer source-mode register)
        val (if byte? (high-byte val) val)
        high-bit (if byte? 7 15)
        c (C computer)
        new-c (bit-get val 0)
        new-val (set-bit (bit-shift-right val 1) high-bit c)]
    (-> computer
        (set-C new-c)
        (set-reg register (make-word new-val)))))

(defmethod single-op :SWPB
  [_ computer _ source-mode register]
  (let [[value computer] (get-value computer source-mode register)]
    (set-reg computer register (little-endian value))))

(defmethod single-op :RRA
  [_ computer byte? source-mode register]
  (let [[value computer] (get-value computer source-mode register)
        value (if byte? (high-byte value) value)]
    (set-reg computer register (make-word (bit-shift-right value 1)))))

(defmethod single-op :SXT
  [_ computer _ source-mode register]
  (let [[val computer] (get-value computer source-mode register)]
    (set-reg computer register (make-word (unchecked-byte (high-byte val))))))

(defmethod single-op :PUSH
  [_ computer byte? source-mode register]
  (let [[val computer] (get-value computer source-mode register)
        val (if byte? (high-byte val) val)]
    (stack-push computer (make-word val))))

(defmethod single-op :CALL
  [_ computer _ source-mode register]
  (let [[val computer] (get-value computer source-mode register)]
    (-> computer
        (set-word-indirect (named-register :sp) val)
        (dec-reg (named-register :sp))
        (set-PC computer val))))

(defmethod single-op :RETI
  [_ computer _ _ _]
  (let [[sp computer] (stack-pop computer)
        [pc computer] (stack-pop computer)]
    (-> computer
        (set-PC pc)
        (set-SP sp))))

(defn perform-jmp [cnd computer offset]
  (let [pc (PC computer)]
    (if ((cnd conditions) computer)
      (set-PC computer (+w pc offset))
      computer)))

;; dual-op is a multimethod that performs a single operand OP. OP is parameterized
;; by the first argument (a symbol)
(defmulti dual-op (fn [op _ _ _ _ _ _] op))
(defmethod dual-op :MOV
  [_ computer byte? souce-mode source-reg dest-mode dest-reg])
(defmethod dual-op :ADD
  [_ computer byte? souce-mode source-reg dest-mode dest-reg])
(defmethod dual-op :ADDC
  [_ computer byte? souce-mode source-reg dest-mode dest-reg])
(defmethod dual-op :SUBC
  [_ computer byte? souce-mode source-reg dest-mode dest-reg])
(defmethod dual-op :SUB
  [_ computer byte? souce-mode source-reg dest-mode dest-reg])
(defmethod dual-op :CMP
  [_ computer byte? souce-mode source-reg dest-mode dest-reg])
(defmethod dual-op :DADD
  [_ computer byte? souce-mode source-reg dest-mode dest-reg])
(defmethod dual-op :BIT
  [_ computer byte? souce-mode source-reg dest-mode dest-reg])
(defmethod dual-op :XOR
  [_ computer byte? souce-mode source-reg dest-mode dest-reg])
(defmethod dual-op :AND
  [_ computer byte? souce-mode source-reg dest-mode dest-reg])




(defn execute-instruction [wrd computer]
  (let [wrd (int->binstr wrd)]
    ;; Matches single operand instruction
    (if-let [[_ opcode byte? source-mode register] (re-matches #"^000100([01]{3})([01])([01]{2})([01]{4})$" wrd)]
      (let [ op (get single-op-codes opcode)
            source-mode (source-modes source-mode)
            byte? (= byte? "1")
            register (binstr->int register)]
        (single-op op computer byte? source-mode register))
      ;; Matches jmp instruction
      (if-let [[_ condition offset] (re-matches #"001([01]{3})([01]{10})$" wrd)]
        (let [cnd (get condition-codes condition)
              offset (binstr->int offset)]
          (perform-jmp cnd computer offset))
        ;; Matches dual operand instruction
        (if-let [[_ opcode source-reg dest-mode byte? source-mode dest-reg ] (re-matches #"([01]{4})([01]{4})([01])([01])([01]{2})([01]{4})$" wrd)]
          (let [op (get dual-op-codes opcode)
                source-mode (source-modes source-mode)
                source-reg (binstr->int source-reg)
                byte? (= byte? "1")
                dest-mode (dest-modes dest-mode)
                dest-reg (binstr->int dest-reg)]
            (dual-op op computer byte? source-mode source-reg dest-mode dest-reg))
          "NOPE")))))


(defn do-stuff [computer]
  "Do stuff I guess"
  (apply execute-instruction (fetch-instruction)))


(defn print-instruction [name byte-addr? address register])
