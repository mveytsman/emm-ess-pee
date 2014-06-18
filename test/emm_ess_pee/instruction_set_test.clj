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

(defn parse-test
  "helper function to wrap execute-instruction"
  [instruction] (execute-instruction (binstr->int instruction) nil))

(deftest single-op-parsing-test
  
  (testing "op parsing"
    ;; This redef just gives us back the op symbol (instead of executing the op)
    (with-redefs [single-op (fn [op _ _ _ _] op)]
      (testing "RPC"
        (is (= (parse-test (str "000100" "000" "0000000"))
               :RPC)))
      (testing "SWPB"
        (is (= (parse-test (str "000100" "001" "0000000"))
               :SWPB)))
      (testing "RRA"
        (is (= (parse-test (str "000100" "010" "0000000"))
               :RRA)))
      (testing "SXT"
        (is (= (parse-test (str "000100" "011" "0000000"))
               :SXT)))
      (testing "PUSH"
        (is (= (parse-test (str "000100" "100" "0000000"))
               :PUSH)))
      (testing "CALL"
        (is (= (parse-test (str "000100" "101" "0000000"))
               :CALL)))
      (testing "RETI"
        (is (= (parse-test (str "000100" "110" "0000000"))
               :RETI)))))
  (testing "byte/word mode parsing"
    (with-redefs [single-op (fn [_ _ byte? _ _] byte?)]
      (testing "word mode"
        (is (false? (parse-test (str "000100" "000" "0" "000000")))))
      (testing "byte mode"
        (is (true?  (parse-test (str "000100" "000" "1" "000000")))))))

  (testing "source-mode parsing"
    (with-redefs [single-op (fn [_ _ _ source-mode  _] source-mode)]
      (testing "register-direct mode"
        (is (= (parse-test (str "000100" "000" "0" "00" "0000"))
               "00")))
      (testing "register-indexed mode"
        (is (= (parse-test (str "000100" "000" "0" "00" "0000"))
               "00")))
      (testing "register-indirect mode"
        (is (= (parse-test (str "000100" "000" "0" "00" "0000"))
               "00")))
      (testing "register-indirect-with-post-increment mode"
        (is (= (parse-test (str "000100" "000" "0" "00" "0000"))
               "00")))))
  (testing "register parsing"
    (with-redefs [single-op (fn [_ _ _ _ register] register)]
      (is (= (parse-test (str "000100" "000" "0" "00" "0000"))
             0))
      (is (= (parse-test (str "000100" "000" "0" "00" "0001"))
             1))
      (is (= (parse-test (str "000100" "000" "0" "00" "1111"))
             15)))))


(deftest jmp-parsing-test
  (testing "jmp parsing"
    ;; Redef to return condition symbol
    (with-redefs [perform-jmp (fn [cnd _ _] cnd)]
      (testing "JNE"
        (is (= (parse-test (str "001" "000" "0000000000")) :JNE)))
      (testing "JEQ"
        (is (= (parse-test (str "001" "001" "0000000000")) :JEQ)))
      (testing "JNC"
        (is (= (parse-test (str "001" "010" "0000000000")) :JNC)))
      (testing "JC"
        (is (= (parse-test (str "001" "011" "0000000000")) :JC)))
      (testing "NC"
        (is (= (parse-test (str "001" "100" "0000000000")) :NC)))
      (testing "JGE"
        (is (= (parse-test (str "001" "101" "0000000000")) :JGE)))
      (testing "JL"
        (is (= (parse-test (str "001" "110" "0000000000")) :JL)))
      (testing "JMP"
        (is (= (parse-test (str "001" "111" "0000000000")) :JMP))))))


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
                       (set-reg register 0xaaff)
                       (set-word 0xabcd 0x1234))
          ;; use these function to wrap single-op for testing
          ;; Note we are only testing direct register access here
          single-op-word (fn [op] (single-op op computer false "00" register))
          single-op-byte (fn [op] (single-op op computer  true "00" register))]
      (testing "word forms of OPs"
        (testing "RPC"
          (let [computer (single-op-word :RPC)]
            ;; Rotate right through carry
            (is (= (get-reg computer register) 0x557f))
            (is (= (C computer) 1))))
        (testing "SWPB"
          ;; Swap bytes
          (let [computer (single-op-word :SWPB)]
            (is (= (get-reg computer register) 0xffaa))))
        (testing "RRA"
          (let [computer (single-op-word :RRA)]
            (is (= (get-reg computer register) 0x557f))))
        (testing "SXT"
          (let [computer (single-op-word :SXT)]
            ;; Sign extend
            (is (= (get-reg computer register) 0xffff))))
        (testing "PUSH"
          (let [computer (single-op-word :PUSH)]
            (is (= (get-word computer 0x2000) 0xaaff))
            (is (= (SP computer) 0x1ffe))))
        (testing "RETI"
          (let [computer (single-op-word :RETI)]
            (is (= (SP computer) 0xabcd))
            (is (= (PC computer) 0x1234)))))
      (testing "byte forms of OPs"
        (testing "RPC"
          (let [computer (single-op-byte :RPC)]
            (is (= (get-reg computer register) 0x007f))
            (is (= (C computer) 1))))
        (testing "RRA"
          (let [computer (single-op-byte :RRA)]
            (is (= (get-reg computer register) 0x007f))))
        (testing "PUSH"
          (let [computer (single-op-byte :PUSH)]
            (is (= (get-word computer 0x2000) 0x00ff))
            (is (= (SP computer) 0x1ffe))))))))


(deftest perform-jmo
  (testing "perform-jmp"
    (let [old-pc 0x4402
          new-pc 0x4502
          computer (-> (make-computer)
                       (set-PC 0x4402))
          ;; Wrapper for testing
          perform-jmp' (fn [op computer] (perform-jmp op computer 0x100))]
      (testing "JNE"
        (is (= (PC (perform-jmp' :JNE (set-Z computer 0)))
               new-pc))
        (is (= (PC (perform-jmp' :JNE (set-Z computer 1)))
               old-pc)))
      (testing "JEQ"
        (is (= (PC (perform-jmp' :JEQ (set-Z computer 0)))
               old-pc))
        (is (= (PC (perform-jmp' :JEQ (set-Z computer 1)))
               new-pc)))
      (testing "JNC"
        (is (= (PC (perform-jmp' :JNC (set-C computer 0)))
               new-pc))
        (is (= (PC (perform-jmp' :JNC (set-C computer 1)))
               old-pc)))
      (testing "JC"
        (is (= (PC (perform-jmp' :JC (set-C computer 0)))
               old-pc))
        (is (= (PC (perform-jmp' :JC (set-C computer 1)))
               new-pc)))
      (testing "JN"
        (is (= (PC (perform-jmp' :JN (set-N computer 0)))
               old-pc))
        (is (= (PC (perform-jmp' :JN (set-N computer 1)))
               new-pc)))
      (testing "JGE"
        (is (= (PC (perform-jmp' :JGE  (-> (set-N computer 1) (set-V 1))))
               new-pc))
        (is (= (PC (perform-jmp' :JGE (-> (set-N computer 1) (set-V 0))))
               old-pc)))
      (testing "JL"
        (is (= (PC (perform-jmp' :JL (-> (set-N computer 0) (set-V 0))))
               old-pc))
        (is (= (PC (perform-jmp' :JL (-> (set-N computer 1) (set-V 0))))
               new-pc)))
      (testing "JMP"
        (is (= (PC (perform-jmp' :JMP computer))
               new-pc))))))



