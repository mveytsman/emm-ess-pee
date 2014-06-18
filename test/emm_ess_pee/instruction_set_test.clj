(ns emm-ess-pee.instruction-set-test
  (:require [clojure.test :refer :all]
            [emm-ess-pee.instruction-set :refer :all]
            [emm-ess-pee.binary-utils :refer :all]
            [emm-ess-pee.computer :refer :all]))


;; Do I even need this helper?
(defn load-instructions
  "Loads instructions vector and sets the PC to their start"
  [instructions]
  (-> (make-computer)
      (set-words 0x4400 instructions)
      (set-PC 0x4400)))

(deftest single-op-parsing-test
  (testing "op parsing"
    (with-redefs [single-op (fn [op _ _ _ _] op)]
      (testing "RPC"
        (is (= (execute-instruction (binstr->int (str "000100" "000" "0000000")) nil)
               :RPC)))
      (testing "SWPB"
        (is (= (execute-instruction (binstr->int (str "000100" "001" "0000000")) nil)
               :SWPB)))
      (testing "RRA"
        (is (= (execute-instruction (binstr->int (str "000100" "010" "0000000")) nil)
               :RRA)))
      (testing "SXT"
        (is (= (execute-instruction (binstr->int (str "000100" "011" "0000000")) nil)
               :SXT)))
      (testing "PUSH"
        (is (= (execute-instruction (binstr->int (str "000100" "100" "0000000")) nil)
               :PUSH)))
      (testing "CALL"
        (is (= (execute-instruction (binstr->int (str "000100" "101" "0000000")) nil)
               :CALL)))
      (testing "RETI"
        (is (= (execute-instruction (binstr->int (str "000100" "110" "0000000")) nil)
               :RETI)))))
  (testing "byte/word mode parsing"
    (with-redefs [single-op (fn [_ _ byte? _ _] byte?)]
      (testing "word mode"
        (is (false? (execute-instruction (binstr->int (str "000100" "000" "0" "000000")) nil))))
      (testing "byte mode"
        (is (true?  (execute-instruction (binstr->int (str "000100" "000" "1" "000000")) nil))))))

  (testing "source-mode parsing"
    (with-redefs [single-op (fn [_ _ _ source-mode  _] source-mode)]
      (testing "register-direct mode"
        (is (= (execute-instruction (binstr->int (str "000100" "000" "0" "00" "0000")) nil)
               "00")))
      (testing "register-indexed mode"
        (is (= (execute-instruction (binstr->int (str "000100" "000" "0" "00" "0000")) nil)
               "00")))
      (testing "register-indirect mode"
        (is (= (execute-instruction (binstr->int (str "000100" "000" "0" "00" "0000")) nil)
               "00")))
      (testing "register-indirect-with-post-increment mode"
        (is (= (execute-instruction (binstr->int (str "000100" "000" "0" "00" "0000")) nil)
               "00")))))
  (testing "register parsing"
    (with-redefs [single-op (fn [_ _ _ _ register] register)]
      (is (= (execute-instruction (binstr->int (str "000100" "000" "0" "00" "0000")) nil)
             0))
      (is (= (execute-instruction (binstr->int (str "000100" "000" "0" "00" "0001")) nil)
             1))
      (is (= (execute-instruction (binstr->int (str "000100" "000" "0" "00" "1111")) nil)
             15)))))

;; TODO test edge cases like constant generation here
(deftest get-value-test
  (testing "get-value"
    (let [register 13
          computer (-> (make-computer)
                       (set-PC 0x4402)
                       (set-word 0x4402 0x02)
                       (set-reg register 0xabcd)
                       (set-word 0xabcd 0x1234)
                       (set-word 0xabcf 0x5678))]
      (testing "register-direct mode"
        (let [[value computer] (get-value computer "00" register)]
          (is (= value 0xabcd))))
      (testing "register-indexed mode"
        (let [[value computer] (get-value computer "01" register)]
          (is (= value 0x5678))
          (is (= (PC computer) 0x4404))))
      (testing "register-indirect mode"
        (let [[value computer] (get-value computer "10" register)]
          (is (= value 0x1234))))
      (testing "register-indirect-with-post-increment mode"
        (let [[value computer] (get-value computer "11" register)]
          (is (= value 0x1234))
          (is (= (get-reg computer register) 0xabcf)))))))

(deftest signle-op-test
  (testing "single-op"
    (let [register 13
          computer (-> (make-computer)
                       (set-PC 0x4402)
                       (set-SP 0x2000)
                       (set-words 0x2000 [0xabcd 0x1234])
                       (set-reg register 0x00ff)
                       (set-word 0xabcd 0x1234))
          ;; use this function to wrap single-op for testing
          ;; Note we are only testing direct register access here
          single-op' (fn [op] (single-op op computer false "00" register))]
      (testing "RPC"
        (let [computer (single-op' :RPC)]
          ;; Rotate right through carry
          (is (= (get-reg computer register) 0x007f))
          (is (= (C computer) 1))))
      (testing "SWPB"
        ;; Swap bytes
        (let [computer (single-op' :SWPB)]
          (is (= (get-reg computer register) 0xff00))))
      (testing "RRA"
        (let [computer (single-op' :RRA)]
          (is (= (get-reg computer register) 0x007f))))
      (testing "SXT"
        (let [computer (single-op' :SXT)]
          ;; Sign extend
          (is (= (get-reg computer register) 0xffff))))
      (testing "PUSH"
        (let [computer (single-op' :PUSH)]
          (is (= (get-word computer 0x2000) 0x00ff))
          (is (= (SP computer) 0x1ffe))))
      (testing "RETI"
        (let [computer (single-op' :RETI)]
          (is (= (SP computer) 0xabcd))
          (is (= (PC computer) 0x1234)))))))




