(ns emm-ess-pee.binary-utils
  (:require [clojure.pprint :refer [cl-format]]))
;; define some operations on bytes
(def word unchecked-short)
(defn low-byte [wrd]
  (unchecked-byte (bit-shift-right wrd 8)))
(defn high-byte [wrd]
  (unchecked-byte wrd))
(def +w (comp word unchecked-add))
(def *w (comp word unchecked-multiply))
(defn little-endian
  "Flips the two bytes of wrd, i.e. abcd becomes cdab"
  [wrd]
  (+w (bit-shift-left wrd 8) (bit-shift-right wrd 8)))

(defn make-word
  "Makes a word out of a vector of two bytes (little endian)"
  [[a b]]
  (+w (bit-shift-left b 8) a))

(defn int->hexstr [wrd]
  (cl-format nil "~4,'0x" wrd))
(defn int->binstr [wrd]
  (cl-format nil "~16,'0b" wrd))

(defn binstr->int [str]
  (Integer/parseInt str 2))

(defn hexstr->int [str]
  (Integer/parseInt str 16))

