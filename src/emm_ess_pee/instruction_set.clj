(ns emm-ess-pee.instruction-set
  (:use emm-ess-pee.binary-utils
        emm-ess-pee.computer
        emm-ess-pee.pprint))

;; TODO: some instructions set the status register but never overflow, should I set V to 0 then
;; TODO: constant mode for access from R2 and R3
;; TODO: interrupts?
;; TODO: DADC instruction

(def single-op-codes {"000" :RRC
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
                    "1011" :BIT
                    "1100" :BIC
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

;; single-op is a multimethod that performs a single operand OP. OP is parameterized
;; by the first argument (a symbol)
;; These always puts the result in the register directly, which is a bug I believe?
(defmulti single-op (fn [op _ _ _ _] op))
(defmethod single-op :RRC
  [_ computer byte? source-mode register]
  (let [[val computer] (get-value computer source-mode byte? register)
        high-bit (if byte? 7 15)
        c (C computer)
        new-c (bit-get val 0)
        new-val (set-bit (bit-shift-right val 1) high-bit c)]
    (-> computer
        (set-C new-c)
        (set-reg register (make-word new-val)))))

(defmethod single-op :SWPB
  [_ computer _ source-mode register]
  (let [[value computer] (get-value computer source-mode false register)]
    (set-reg computer register (little-endian value))))

(defmethod single-op :RRA
  [_ computer byte? source-mode register]
  (let [[value computer] (get-value computer source-mode byte? register)
        result (make-bw (bit-shift-right value 1) byte?)]
    (-> (set-reg computer register result)
        (set-ZCN result byte?))))

(defmethod single-op :SXT
  [_ computer _ source-mode register]
  (let [[val computer] (get-value computer source-mode false register)
        result (make-word (unchecked-byte (high-byte val)))]
    (-> (set-reg computer register result)
        (set-ZCN result false))))

(defmethod single-op :PUSH
  [_ computer byte? source-mode register]
  (let [[val computer] (get-value computer source-mode byte? register)]
    (stack-push computer (make-word val))))

(defmethod single-op :CALL
  [_ computer _ source-mode register]
  (let [[val computer] (get-value computer source-mode false register)]
    (-> computer
        (set-word-indirect (named-register :sp) val)
        (dec-reg (named-register :sp))
        (set-PC computer val))))

(defmethod single-op :RETI
  [_ computer _ _ _]
  (let [[sp computer] (stack-pop computer)
        [pc computer] (stack-pop computer)]
    (-> computer
        (set-SP sp)
        (set-PC pc)
        ;;RETI sets status register, no idea what this is about
        (set-ZCN pc false))))

(defn perform-jmp [cnd computer offset]
  (let [pc (PC computer)]
    (if ((cnd conditions) computer)
      (set-PC computer (+w pc offset))
      computer)))

;; dual-op is a multimethod that performs a single operand OP. OP is parameterized
;; by the first argument (a symbol)
(defmulti dual-op (fn [op _ _ _ _ _ _] op))
(defmethod dual-op :MOV
  [_ computer byte? source-mode source-reg dest-mode dest-reg]
  (let [[val computer] (get-value computer source-mode byte? source-reg)]
    (set-value computer dest-mode byte? dest-reg (make-word val))))

(defmethod dual-op :ADD
  [_ computer byte? source-mode source-reg dest-mode dest-reg]
  (let [[src computer] (get-value computer source-mode byte? source-reg)
        [dst computer] (get-value computer dest-mode byte? dest-reg)
        result (+ src dst)
        computer (-> (set-ZCN computer result byte?)
                     (set-V-add dst src result byte?))
        result (make-bw result byte?)]
    (set-value computer dest-mode byte? dest-reg result)))

(defmethod dual-op :ADDC
  [_ computer byte? source-mode source-reg dest-mode dest-reg]
  (let [[src computer] (get-value computer source-mode byte? source-reg)
        [dst computer] (get-value computer dest-mode byte? dest-reg)
        c (C computer)
        result (+ src dst c)
        computer (-> (set-ZCN computer result byte?)
                     (set-V-add dst src result byte?))
        result (make-bw result byte?)]
    (set-value computer dest-mode byte? dest-reg result)))

(defmethod dual-op :SUBC
  [_ computer byte? source-mode source-reg dest-mode dest-reg]
  (let [[src computer] (get-value computer source-mode byte? source-reg)
        src (bit-not src)
        [dst computer] (get-value computer dest-mode byte? dest-reg)
        c (C computer)
        result (+ src dst c)
        computer (-> (set-ZCN computer result byte?)
                     ;; I don't know if I have the correct order here
                     (set-V-sub dst src result byte?))
        result (make-bw result byte?)]
    (set-value computer dest-mode byte? dest-reg result)))

(defmethod dual-op :SUB
  [_ computer byte? source-mode source-reg dest-mode dest-reg]
  (let [[src computer] (get-value computer source-mode byte? source-reg)
        src (bit-not src)
        [dst computer] (get-value computer dest-mode byte? dest-reg)
        result (+ dst src 1)
        computer (-> (set-ZCN computer result byte?)
                     ;; I don't know if I have the correct order here
                     (set-V-sub dst src result byte?))
        result (make-bw result byte?)]
    (set-value computer dest-mode byte? dest-reg result)))

(defmethod dual-op :CMP
  [_ computer byte? source-mode source-reg dest-mode dest-reg]
  (let [[src computer] (get-value computer source-mode byte? source-reg)
        [dst computer] (get-value computer dest-mode byte? dest-reg)
        result (- dst src)]
    (-> (set-ZCN computer result byte?)
        (set-V-sub dst src result byte?))))
(defmethod dual-op :DADD
  [_ computer byte? source-mode source-reg dest-mode dest-reg])

(defmethod dual-op :BIT
  [_ computer byte? source-mode source-reg dest-mode dest-reg]
  (let [[src computer] (get-value computer source-mode byte? source-reg)
        [dst computer] (get-value computer dest-mode byte? dest-reg)
        result (bit-and dst src)]
    (set-ZCN computer result byte?)))

(defmethod dual-op :BIC
  [_ computer byte? source-mode source-reg dest-mode dest-reg]
  (let [[src computer] (get-value computer source-mode byte? source-reg)
        [dst computer] (get-value computer dest-mode byte? dest-reg)
        result (bit-and dst (bit-not src))]
    (set-value computer dest-mode byte? dest-reg result)))

(defmethod dual-op :BIS
  [_ computer byte? source-mode source-reg dest-mode dest-reg]
  (let [[src computer] (get-value computer source-mode byte? source-reg)
        [dst computer] (get-value computer dest-mode byte? dest-reg)
        result (bit-or dst src)]
    (set-value computer dest-mode byte? dest-reg result)))

(defmethod dual-op :XOR
  [_ computer byte? source-mode source-reg dest-mode dest-reg]
  (let [[src computer] (get-value computer source-mode byte? source-reg)
        [dst computer] (get-value computer dest-mode byte? dest-reg)
        result (bit-xor dst src)]
    (-> (set-value computer dest-mode byte? dest-reg result)
        (set-ZCN result byte?))))

(defmethod dual-op :AND
  [_ computer byte? source-mode source-reg dest-mode dest-reg]
  (let [[src computer] (get-value computer source-mode byte? source-reg)
        [dst computer] (get-value computer dest-mode byte? dest-reg)
        result (bit-and dst src)]
    (-> (set-value computer dest-mode byte? dest-reg result)
        (set-ZCN result byte?))))

(defn execute-instruction [wrd computer]
  (let [wrd (int->binstr wrd)]
    ;; Matches single operand instruction
    (if-let [[_ opcode byte? source-mode register] (re-matches #"^000100([01]{3})([01])([01]{2})([01]{4})$" wrd)]
      (let [ op (get single-op-codes opcode)
            source-mode (source-modes source-mode)
            byte? (= byte? "1")
            register (binstr->int register)]
        (print-single-op op computer byte? source-mode register)
        (single-op       op computer byte? source-mode register))
      ;; Matches jmp instruction
      (if-let [[_ condition offset] (re-matches #"001([01]{3})([01]{10})$" wrd)]
        (let [cnd (get condition-codes condition)
              offset (* 2 (binstr->int offset))]
          
          (print-jmp cnd offset)
          (perform-jmp cnd computer offset))
        ;; Matches dual operand instruction
        (if-let [[_ opcode source-reg dest-mode byte? source-mode dest-reg ] (re-matches #"([01]{4})([01]{4})([01])([01])([01]{2})([01]{4})$" wrd)]
          (let [op (get dual-op-codes opcode)
                source-mode (source-modes source-mode)
                source-reg (binstr->int source-reg)
                byte? (= byte? "1")
                dest-mode (dest-modes dest-mode)
                dest-reg (binstr->int dest-reg)]
            (print-dual-op op computer byte? source-mode source-reg dest-mode dest-reg)
            (dual-op op computer byte? source-mode source-reg dest-mode dest-reg))
          "NOPE")))))


(defn do-stuff [computer]
  "Do stuff I guess"
  (apply execute-instruction (fetch-instruction computer)))
