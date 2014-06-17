(ns emm-ess-pee.binary-utils
  (:require [clojure.pprint :refer [cl-format]]))
;; define some operations on bytes
(def word unchecked-short)
(defn make-byte
  "Clojure byte are *signed* so we're going to cheat by anding with 0xff (thereby enxting to an int)"
  [byt]
  (bit-and (unchecked-byte byt) 0xff))

(defn make-word
  "Makes a word out of a value or two bytes (little-endian)"
  ([wrd] (bit-and (unchecked-short wrd) 0xffff))
  ([byt1 byt2] (+ (bit-shift-left (make-byte byt2) 8) (make-byte byt1))))
(defn low-byte [wrd]
  (make-byte (bit-shift-right wrd 8)))
(defn high-byte [wrd]
  (make-byte (bit-and wrd 0x00ff)))
(def +w (comp make-word +))
(def -w (comp make-word -))
(def *w (comp make-word *))
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
