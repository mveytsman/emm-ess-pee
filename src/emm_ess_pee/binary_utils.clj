(ns emm-ess-pee.binary-utils
  (:require [clojure.pprint :refer [cl-format]]))

;; This file contains useful binary operations

(defn make-byte
  "Clojure byte are *signed* so we're going to cheat by anding with 0xff (thereby enxting to an int)"
  [byt]
  (bit-and (unchecked-byte byt) 0xff))

(defn make-word
  "Makes a word out of a value or two bytes (little-endian)"
  ([wrd] (bit-and (unchecked-short wrd) 0xffff))
  ([byt1 byt2] (+ (bit-shift-left (make-byte byt2) 8) (make-byte byt1))))

(defn make-bw
  "Makes a byte or word depending on byte? value"
  [value byte?]
  (if byte?
    (make-byte value)
    (make-word value)))

(defn low-byte
  "Returns low byte of a little-endian word"
  [wrd]
  (make-byte (bit-shift-right wrd 8)))
(defn high-byte
  "Returns high byte of a litle-endian word"
  [wrd]
  (make-byte (bit-and wrd 0x00ff)))

;; Arithmetic operations that operate on words & bytes
(def +w (comp make-word +))
(def -w (comp make-word -))
(def *w (comp make-word *))
(def +b (comp make-byte +))
(def -b (comp make-byte -))
(def *b (comp make-byte *))

(defn little-endian
  "Flips the two bytes of wrd, i.e. abcd becomes cdab"
  [wrd]
  (+w (bit-shift-left wrd 8) (bit-shift-right wrd 8)))

(defn int->hexstr [wrd]
  (cl-format nil "~4,'0x" wrd))
(defn int->binstr [wrd]
  (cl-format nil "~16,'0b" wrd))

(defn binstr->int [str]
  (Integer/parseInt str 2))

(defn hexstr->int [str]
  (Integer/parseInt str 16))


(defn bit-get [x n]
  (if (bit-test x n) 1 0))


(defn set-bit [x n val]
  (if (= val 0)
    (bit-clear x n)
    (bit-set x n)))

(defn high-bit
  "Returns the most significant bit (determined by whether it's a byte or word operatin)"
  [value byte?]
  (if byte?
    (bit-get value 7)
    (bit-get value 15)))

(defn overflow-bit
  "Returns the overflow  bit (determined by whether it's a byte or word operatin)"
  [value byte?]
  (if byte?
    (bit-get value 8)
    (bit-get value 16)))


(defn sign-extend
  "Sign extends a binary number of `bits` bits to a word. Default is 8 (byte)"
  ([num]
     (sign-extend num 8))
  ([num bits]
     (let [delta (- 16 bits)]
       ;; Kind of a hacky way to do this, shift the number left, cast to a word, then shift right
       (-> (bit-shift-left num delta)
           (unchecked-short)
           (bit-shift-right delta)))))
