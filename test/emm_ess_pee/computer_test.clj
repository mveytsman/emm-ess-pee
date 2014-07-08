(ns emm-ess-pee.computer-test
  (:require [clojure.test :refer :all]
            [emm-ess-pee.computer :refer :all]))

(deftest get-bytes-test
  (let [computer (-> (make-computer)
                     (set-bytes 0 [0x01 0x02])
                     (set-bytes 65534 [0x05 0x06]))]
    (is (= (get-bytes computer 65534 4) [0x05 0x06 0x01 0x02]))))
