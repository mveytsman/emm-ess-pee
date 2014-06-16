(ns emm-ess-pee.core
  (:use emm-ess-pee.binary-utils)
  (:gen-class))


(def register-names [:pc :sp :sr :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15])

(def named-register (zipmap register-names (range)))

(def status-bit-names [:gc :z :n :gie :cpuoff :oscoff :scg0 :scg1 :v])

(def status-bit-index (zipmap status-bit-names (range)))


(defn pprint-registers [registers]
  (pprint (apply sorted-map (interleave register-names registers))))

(defn make-computer []
  {:registers (vec (repeat 16 (word 0)))
   :memory (vec (repeat 20 (word 0)))})

(defn get-reg [computer reg]
  (get-in computer [:registers reg]))

(defn set-reg [computer reg]
  (assoc-in computer [:registers reg]))

(defn inc-pc
  ([computer] (inc-pc computer 2))
  ([computer delta] (let [pc (get-reg computer (named-register :pc))]
                      (set-reg computer (named-register :pc) (+w pc delta)))))

(defn get-word [vect i]
  (make-word (subvec vect i (+ i 1))))

(defn set-word [vect i val]
  ())

(defn get-indirect [computer reg]
  (let [i (int (get-in computer [:registers reg])]
        (get-word (:memory computer) i))))

(defn set-indirect [computer reg value]
  (let [i (int (get-in computer [:registers reg])]
        (assoc-in computer [: ( (]))] (little-endian value)))


(defn get-indexed [computer reg index]
  (get-in computer [:memory (+w (get-in computer [:registers reg]) index)]))

(defn set-indexed [computer reg value index]
  (assoc-in computer [:memory (+w  (get-in computer [:registers reg]) index)] value))


(defn status? [computer bit]
  (let [bit (if (keyword? bit)
              (bit register-index)
              bit)]
    (bit-test (get-reg computer :sr))) bit)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

