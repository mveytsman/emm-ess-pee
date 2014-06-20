(ns emm-ess-pee.computer
  (:use emm-ess-pee.binary-utils))

(defn get-reg
  "Returns value register reg from a computer"
  [computer i]
  (get-in computer [:registers i]))

(defn set-reg
  "Sets register i to value in computer"
  [computer reg value]
  (assoc-in computer [:registers reg] value))

(defn inc-reg
  "Increments register by delta (default 2)"
  ([computer reg] (inc-reg computer reg 2))
  ([computer reg delta] (let [value (get-reg computer reg)]
                          (set-reg computer reg (+w value delta)))))

(defn dec-reg
  "Decrements register by delta (default 2)"
  ([computer reg] (dec-reg computer reg 2))
  ([computer reg delta] (let [value (get-reg computer reg)]
                          (set-reg computer reg (-w value delta)))))

;; Getters and setters for named registers
(defn PC [computer]
  (get-reg computer 0))

(defn SP [computer]
  (get-reg computer 1))

(defn SR [computer]
  (get-reg computer 2))

(defn set-PC [computer value]
  (set-reg computer 0 value))

(defn set-SP [computer value]
  (set-reg computer 1 value))

(defn set-SR [computer value]
  (set-reg computer 2 value))

(defn inc-PC
  "Increments PC by delta (default 2)"
  ([computer] (inc-reg computer 0 2))
  ([computer delta] (inc-reg computer 0 delta)))

(defn dec-SP
  "Decrements SP by delta (default 2)"
   ([computer] (dec-reg computer 1 2))
   ([computer delta] (dec-reg computer 1 delta)))

;; getters and setters for status bits (SR register)

(defn C [computer]
  (bit-get (SR computer) 0))

(defn Z [computer]
  (bit-get (SR computer) 1))

(defn N [computer]
  (bit-get (SR computer) 2))

(defn GIE [computer]
  (bit-get (SR computer) 3))

(defn CPUOFF [computer]
  (bit-get (SR computer) 4))

(defn OSCOFF [computer]
  (bit-get (SR computer) 5))

(defn SCG0 [computer]
  (bit-get (SR computer) 6))

(defn SCG1 [computer]
  (bit-get (SR computer) 7))

(defn V [computer]
  (bit-get (SR computer) 8))

(defn set-C [computer value]
  (set-SR computer (set-bit (SR computer) 0 value)))

(defn set-Z [computer value]
  (set-SR computer (set-bit (SR computer) 1 value)))

(defn set-N [computer value]
  (set-SR computer (set-bit (SR computer) 2 value)))

(defn set-GIE [computer value]
  (set-SR computer (set-bit (SR computer) 3 value)))

(defn set-CPUOFF [computer value]
  (set-SR computer (set-bit (SR computer) 4 value)))

(defn set-OSCOFF [computer value]
  (set-SR computer (set-bit (SR computer) 5 value)))

(defn set-SCG0 [computer value]
  (set-SR computer (set-bit (SR computer) 6 value)))

(defn set-SCG1 [computer value]
  (set-SR computer (set-bit (SR computer) 7 value)))

(defn set-V [computer value]
  (set-SR computer (set-bit (SR computer) 8 value)))

(defn set-ZCN
  "Sets the Z,C,N flags"
  [computer result byte?]
  (-> (set-Z computer (if (zero? result) 1 0))
      (set-C (overflow-bit result byte?))
      (set-N (high-bit result byte?))))

;; TODO: I'm not really testing these :(
(defn set-V-add
  "Sets the V (overflow) status bit after addition"
  [computer left right result byte?]
  ;;http://teaching.idallen.com/dat2343/10f/notes/040_overflow.txt
  (let [v (if (or (and (= (high-bit left byte?) 1)
                       (= (high-bit right byte?) 1)
                       (= (high-bit result byte?) 0))
                  (and (= (high-bit left byte?) 0)
                       (= (high-bit right byte?) 0)
                       (= (high-bit result byte?) 1)))
            1
            0)]
    (set-V computer v)))


(defn set-V-sub
  "Sets the V (overflow) status bit after subtracton"
  [computer left right result byte?]
  ;;http://teaching.idallen.com/dat2343/10f/notes/040_overflow.txt
  (let [v (if (or (and (= (high-bit left byte?) 0)
                       (= (high-bit right byte?) 1)
                       (= (high-bit result byte?) 1))
                  (and (= (high-bit left byte?) 1)
                       (= (high-bit right byte?) 0)
                       (= (high-bit result byte?) 0)))
            1
            0)]
    (set-V computer v)))

(defn get-word
  "Returns a (little-endian) word from memory at index i"
  [computer i]
  (apply make-word (subvec (:memory computer) i (+ i 2))))

(defn set-word
  "Sets a (little-endian) word in memory to value"
  [computer i value]
  (let [value (little-endian value) ; not sure if I should be flipping the bytes twice here
        hb (high-byte value)
        lb (low-byte value)]
    (-> computer
        (assoc-in [:memory i] lb)
        (assoc-in [:memory (inc i)] hb))))

(defn set-words
  ;; TODO NOTE THIS WRITES BACKWARDS
  ;; I used this in a test to write to the stack, FIX or RENAME
  "Writes a series of words to memory starting at index"
  [computer i words]
  (if (<=  (count words) 0)
    computer
    (set-words (set-word computer i (first words))
               (- i 2)
               (rest words))))

(defn set-bytes
  "Writes a series of bytes to memory starting at index"
  [computer i bytes]
  (if (<=  (count bytes) 0)
    computer
    (set-bytes (assoc-in computer [:memory  i] (first bytes))
               (inc i)
               (rest bytes))))


(defn get-word-indirect
  "Returns a word at the address contained in a register (i.e. @Rn)"
  [computer reg]
  (let [i (int (get-reg computer reg))]
    (get-word computer i)))

(defn set-word-indirect
  "Sets a word at the address contained in a register to val (i.e. @Rn)"
  [computer reg value]
  (let [i (int (get-reg computer reg))]
    (set-word computer i value)))

(defn get-word-indexed
  "Returns a word at the address contained in a register at offset (i.e. offset(Rn))"
  [computer reg offset]
  (let [r (int (get-reg computer reg))
        i (int (+w r offset))]
    (get-word computer i)))

(defn set-word-indexed
  "Sets a word at the address contained in a register at offset (i.e. offset(Rn))"
  [computer reg offset value]
  (let [r (int (get-reg computer reg))
        i (int (+w r offset))]
    (set-word computer i value)))

(defn get-word-indirect-increment
  "Returns a word at the address contained in a register and increments the register (i.e. @Rn+)"
  [computer reg]
  (let [i (int (get-reg computer reg))]
    (-> (get-word computer i)
        (assoc-in [:registers reg] (+w i 2)))))

(defn stack-push
  "Pushes a value onto the stack"
  ;; TODO: handle byte/word mode
  [computer value]
  (-> (set-word computer (SP computer) value)
      (dec-SP)))

(defn stack-pop
  "Pops a value off the stack. Returns [value, computer]"
  ;; TODO: handle byte/word mode
  [computer]
  (let [value (get-word computer (SP computer))]
    [value (dec-SP computer)]))

(defn fetch-instruction
  "Returns [instruction, computer] where instruction is the word at PC, and computer has the PC register incremented"
  [computer]
  (let [instruction (get-word computer (PC computer))]
    [instruction, (inc-PC computer)]))

;; In the future we will hook this for hardware debugging
(defn continue-execution?
  "Stop executing when the CPUOFF flag is set"
  [computer]
  (= (CPUOFF computer) 0))

(defn calculate-jmp-offset
  "JMP offsets are sign extended 10-bit integers, which are doubled to get the offset in bytes"
  [offset]
  (* 2 (sign-extend (binstr->int offset) 10))
  )

(defmulti get-value
  "A multimethod for a getter that dispatches on address mode. Returns [value, computer]"
  (fn [_ source-mode _ _] source-mode))
(defmethod get-value :direct
  [computer _ byte? register]
  (cond
   (= register 3) [0x0 computer]
   :else (let [value (get-reg computer register)
               value (if byte?
                       (high-byte value)
                       value)]
           [value computer])))
(defmethod get-value :indexed
  [computer _ byte? register]
  (cond
   (= register 2) (let [[addr computer] (fetch-instruction computer)
                        value (get-word computer addr)
                        value (if byte?
                                (high-byte value)
                                value)]
                    [value computer])
   (= register 3) [0x1 computer]
   :else (let [[offset, computer] (fetch-instruction computer)
               value (get-word-indexed computer register offset)
               value (if byte?
                       (high-byte value)
                       value)]
           [value computer])))
(defmethod get-value :indirect
  [computer _ byte? register]
  (cond
   (= register 2) [0x4 computer]
   (= register 3) [0x2 computer]
   :else (let [value (get-word-indirect computer register)
               value (if byte? (high-byte value) value)]
           [value  computer])))
(defmethod get-value :indirect-increment
  [computer _ byte? register]
  (cond
   (= register 2) [0x8 computer]
   (= register 3) [(make-bw -1 byte?) computer]
   :else (let [value (get-word-indirect computer register)
               value (if byte? (high-byte value) value)]
           [value (inc-reg computer register)])))

(defmulti set-value
  "A multimethod for a setter that dispatches on address mode."
  (fn [_ dest-mode _ _ _] dest-mode))
(defmethod set-value :direct
  [computer _ byte? register value]
  (set-reg computer register (if byte? (high-byte value) value)))
(defmethod set-value :indirect
  [computer _ byte? register value]
  (set-word computer (get-reg computer register) (if byte? (high-byte value) value)))

(defn make-computer
  "Make a bag of state we call a computer, set PC to 0x4400"
  []  
  (let [computer {:registers (vec (repeat 16 (make-word 0)))
                  :memory (vec (repeat 64000 (make-byte 0)))}]
    (set-PC computer 0x4400)))

