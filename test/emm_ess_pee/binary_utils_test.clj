(ns emm-ess-pee.binary-utils-test
  (:require [clojure.test :refer :all]
            [emm-ess-pee.binary-utils :refer :all]))


(deftest make-byte-test
  (testing "make-byte"
    (is (= (make-byte 0xab) 0xab))
    (is (= (make-byte 0xabcd) 0xcd))))

(deftest make-word-test
  (testing "make-word"
    (testing "one arguument"
      (is (= (make-word 0x11ffff) 0xffff)))
    (testing "two arguments"
      (is (= (make-word 0xab 0xcd) 0xcdab)))))

(deftest low-byte-test
  (testing "low-byte"
    (is (= (low-byte 0xabcd) 0xab))))

(deftest high-byte-test
  (testing "high-byte"
    (is (= (high-byte 0xabcd) 0xcd))))

(deftest arith-test
  (testing "+w"
    (is (= (+w 0xab00 0x00cd) 0xabcd)))
  (testing "-w"
    (is (= (-w 0xffff 0x1) 0xfffe))))


(deftest little-endian-test
  (testing "little-endian"
    (is (= (little-endian 0xabcd) 0xcdab))))

(deftest int->hexstr-test
  (testing "int->hexstr"
    (is (= (int->hexstr 0xabcd) "abcd"))))

(deftest int->binstr-test
  (testing "int->binstr"
    (is (= (int->binstr 0xabcd) "1010101111001101"))))


(deftest binstr->int-test
  (testing "binstr->int"
    (is (= (binstr->int "1010101111001101") 0xabcd))))

(deftest hexstr->int-test
  (testing "hexstr->int"
    (is (= (hexstr->int "abcd") 0xabcd))))


(deftest bit-get-test
  (testing "bit-get"
    (is (= (bit-get 0xa2 1) 1))
    (is (= (bit-get 0xa2 0) 0))))

(deftest set-bit-test
  (testing "set-bit"
    (is (= (set-bit 0xa2 0 1) 0xa3))
    (is (= (set-bit 0xa2 1 0) 0xa0))))

