(ns emm-ess-pee.computer
  (:use emm-ess-pee.binary-utils))

(defn make-computer []
  {:registers (vec (repeat 16 (make-word 0)))
   :memory (vec (repeat 64000 (make-byte 0)))})

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

(defn inc-pc
  "Increments PC by delta (default 2)"
  ([computer] (inc-reg computer 0 2))
  ([computer delta] (inc-reg computer 0 delta)))

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
  (set-SR computer (set-bit (SR computer 3) value)))

(defn set-CPUOFF [computer value]
  (set-SR computer (set-bit (SR computer 4) value)))

(defn set-OSCOFF [computer value]
  (set-SR computer (set-bit (SR computer 5) value)))

(defn set-SCG0 [computer value]
  (set-SR computer (set-bit (SR computer 6) value)))

(defn set-SCG1 [computer value]
  (set-SR computer (set-bit (SR computer 7) value)))

(defn set-V [computer value]
  (set-bit computer (SR computer 8) value))

(def register-names [:pc :sp :sr :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15])

(def named-register (zipmap register-names (range)))

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
  "Writes a series of words to memory starting at index"
  [computer i words]
  (if (<=  (count words) 0)
    computer
    (set-words (set-word computer i (first words))
               (- i 2)
               (rest words))))


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
  (-> computer
      (set-word-indirect (named-register :sp) value)
      (dec-reg (named-register :sp))))

(defn stack-pop
  "Pops a value off the stack. Returns [value, computer]"
  ;; TODO: handle byte/word mode
  [computer]
  (let [value (get-word-indirect computer (named-register :sp))]
    [value (dec-reg computer (named-register :sp))]))

;;; maybe I don't need these

