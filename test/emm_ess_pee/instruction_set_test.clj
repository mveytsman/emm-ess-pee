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
               :direct)))
      (testing "register-indexed mode"
        (is (= (parse-test (str "000100" "000" "0" "01" "0000"))
               :indexed)))
      (testing "register-indirect mode"
        (is (= (parse-test (str "000100" "000" "0" "10" "0000"))
               :indirect)))
      (testing "register-indirect-with-post-increment mode"
        (is (= (parse-test (str "000100" "000" "0" "11" "0000"))
               :indirect-increment)))))

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


(deftest dual-op-parsing-test
  (testing "dual op parsing"
    ;; Redef to return condition symbol
    (with-redefs [dual-op (fn [op _ _ _ _ _ _] op)]
      (testing "MOV"
        (is (= (parse-test (str "0100" "000000000000")) :MOV)))
      (testing "ADD"
        (is (= (parse-test (str "0101" "000000000000")) :ADD)))
      (testing "ADDC"
        (is (= (parse-test (str "0110" "000000000000")) :ADDC)))
      (testing "SUBC"
        (is (= (parse-test (str "0111" "000000000000")) :SUBC)))
      (testing "SUB"
        (is (= (parse-test (str "1000" "000000000000")) :SUB)))
      (testing "CMP"
        (is (= (parse-test (str "1001" "000000000000")) :CMP)))
      (testing "DADD"
        (is (= (parse-test (str "1010" "000000000000")) :DADD)))
      (testing "BIT"
        (is (= (parse-test (str "1100" "000000000000")) :BIT)))
      (testing "BIS"
        (is (= (parse-test (str "1101" "000000000000")) :BIS)))
      (testing "XOR"
        (is (= (parse-test (str "1110" "000000000000")) :XOR)))
      (testing "AND"
        (is (= (parse-test (str "1111" "000000000000")) :AND)))))
  (testing "byte/word parsing"
    (with-redefs [dual-op (fn [_ _ byte? _ _ _ _] byte?)]
      (testing "word-mode"
        (is (= (parse-test (str "0100" "00000" "0" "000000")) false)))
      (testing "byte-mode"
        (is (= (parse-test (str "0100" "00000" "1" "000000")) true)))))
  (testing "source-mode parsing"
    (with-redefs [dual-op (fn [_ _ _ source-mode _ _ _] source-mode)]
      (testing "word-mode"
        (is (= (parse-test (str "0100000000" "00" "0000")) :direct)))
      (testing "byte-mode"
        (is (= (parse-test (str "0100000000" "01" "0000")) :indexed)))
      (testing "word-mode"
        (is (= (parse-test (str "0100000000" "10" "0000")) :indirect)))
      (testing "byte-mode"
        (is (= (parse-test (str "0100000000" "11" "0000")) :indirect-increment)))))
  (testing "source register parsing"
    (with-redefs [dual-op (fn [_ _ _ _ source-reg _ _] source-reg)]
      (is (= (parse-test (str "0100" "0000" "00000000")) 0))
      (is (= (parse-test (str "0100" "0001" "00000000")) 1))
      (is (= (parse-test (str "0100" "1111" "00000000")) 15))))
  (testing "dest-mode parsing"
    (with-redefs [dual-op (fn [_ _ _ _ _ dest-mode _] dest-mode)]
      (is (= (parse-test (str "01000000" "0" "0000000")) :direct))
      (is (= (parse-test (str "01000000" "1" "0000000")) :indirect))))
  (testing "dest register parsing"
    (with-redefs [dual-op (fn [_ _ _ _ _ _ dest-reg] dest-reg)]
      (is (= (parse-test (str "010000000000" "0000")) 0))
      (is (= (parse-test (str "010000000000" "0001")) 1))
      (is (= (parse-test (str "010000000000" "1111")) 15)))))


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
      (testing "word mode"
        (testing "register-direct mode"
          (let [[value computer] (get-value computer :direct false register)]
            (is (= value 0xabcd))))
        (testing "register-indexed mode"
          (let [[value computer] (get-value computer :indexed false register)]
            (is (= value 0x5678))
            (is (= (PC computer) 0x4404))))
        (testing "register-indirect mode"
          (let [[value computer] (get-value computer :indirect false register)]
            (is (= value 0x1234))))
        (testing "register-indirect-with-post-increment mode"
          (let [[value computer] (get-value computer :indirect-increment false register)]
            (is (= value 0x1234))
            (is (= (get-reg computer register) 0xabcf)))))
      
      (testing "byte mode"
        (testing "register-direct mode"
          (let [[value computer] (get-value computer :direct true register)]
            (is (= value 0xcd))))
        (testing "register-indexed mode"
          (let [[value computer] (get-value computer :indexed true register)]
            (is (= value 0x78))
            (is (= (PC computer) 0x4404))))
        (testing "register-indirect mode"
          (let [[value computer] (get-value computer :indirect true register)]
            (is (= value 0x34))))
        (testing "register-indirect-with-post-increment mode"
          (let [[value computer] (get-value computer :indirect-increment true register)]
            (is (= value 0x34))
            (is (= (get-reg computer register) 0xabcf))))))))

(deftest set-value-test
  (testing "set-value"
    (let [register 13
          computer (-> (make-computer)
                       (set-reg register 0x4402)
                       (set-word 0x4402 0x02))]
      (testing "register-direct mode"
        (let [computer (set-value computer :direct false register 0xabcd)]
          (is (= (get-reg computer register) 0xabcd))))
      (testing "register-indirect mode"
        (let [computer (set-value computer :indirect false register 0xabcd)]
          (is (= (get-word computer 0x4402) 0xabcd)))))))

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
          single-op-word (fn [op] (single-op op computer false :direct register))
          single-op-byte (fn [op] (single-op op computer  true :direct register))]
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


(deftest perform-jmp-test
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



(deftest dual-op-test
  (testing "dual-op"
    (let [register1 12
          register2 13
          computer (-> (make-computer)
                       (set-C 1)
                       (set-reg register1 0xaaff)
                       (set-reg register2 0xbbe1))
          ;; use these function to wrap single-op for testing
          ;; Note we are only testing direct register access here
          dual-op-word (fn [op] (dual-op op computer false :direct register1 :direct register2))
          dual-op-byte (fn [op] (dual-op op computer true :direct register1 :direct register2))]
      (testing "MOV"
        (testing "word-mode"
          (let [computer (dual-op-word :MOV)]
            (is (= (get-reg computer register2) 0xaaff))))
        (testing "byte-mode"
          (let [computer (dual-op-byte :MOV)]
            (is (= (get-reg computer register2) 0x00ff)))))
      
      (testing "ADD"
        (testing "word-mode"
          (let [computer (dual-op-word :ADD)]
            (is (= (get-reg computer register2) 0x66e0))))
        (testing "byte-mode"
          (let [computer (dual-op-byte :ADD)]
            (is (= (get-reg computer register2) 0xe0)))))

      (testing "ADDC"
        (testing "word-mode"
          (let [computer (dual-op-word :ADDC)]
            (is (= (get-reg computer register2) 0x66e1))))
        (testing "byte-mode"
          (let [computer (dual-op-byte :ADDC)]
            (is (= (get-reg computer register2) 0xe1)))))


      (testing "SUBC"
        (testing "word-mode"
          (let [computer (dual-op-word :SUBC)]
            (is (= (get-reg computer register2) 0x10e2))))
        (testing "byte-mode"
          (let [computer (dual-op-byte :SUBC)]
            (is (= (get-reg computer register2) 0xe2)))))


      (testing "SUB"
        ;; this is the same as SUBC because the carry bit is 1
        (testing "word-mode"
          (let [computer (dual-op-word :SUB)]
            (is (= (get-reg computer register2) 0x10e2))))
        (testing "byte-mode"
          (let [computer (dual-op-byte :SUB)]
            (is (= (get-reg computer register2) 0xe2)))))

      (testing "ADDC"
        (testing "word-mode"
          (let [computer (dual-op-word :ADDC)]
            (is (= (get-reg computer register2) 0x66e1))))
        (testing "byte-mode"
          (let [computer (dual-op-byte :ADDC)]
            (is (= (get-reg computer register2) 0xe1))))))))
