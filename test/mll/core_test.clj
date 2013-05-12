(ns mll.core-test
  (:use clojure.test)
  (:require [mll.llr :as llr])
  )

(deftest llr-test
  (testing "Bigrams llr form dunning"
    (let [r2 (fn [d]
               (double (/ (Math/round (* 100 d))
                          100)))]
      (are [L     AB  AnB  nAB nAnB  A     B]
           (= L (r2 (llr/llr AB AnB nAB nAnB)))
          270.72 110 2442 111 29114 "the" "swiss"
          263.90  29   13 123 31612 "can" "be"
          256.84  31   23 139 31584 "previous" "year"
          57.44    4   11   1 31761 "great" "deal"
          36.98    3   10   5 31759 "current" "transaction"))))

(deftest collect-counts-test
  (let [rows [[1 0 0 1 1]
              [2 1 0 0 0]
              [1 0 1 1 0]]]
    (is (= (llr/collect-counts rows)
           (select-keys (llr/collect-counts-fast rows) [1 2])
          {1 [[0 2 1 0]
              [1 1 0 1]
              [2 0 0 1]
              [1 1 0 1]]
           2 [[1 0 0 2]
              [0 1 1 1]
              [0 1 2 0]
              [0 1 1 1]]}
          ))))

(deftest aclone2d-test
  (let [a (make-array Long/TYPE 2 3)
        _ (aset a 1 2 998)
        ]
    (is (= (llr/aseq2d a) [[0 0 0] [0 0 998]]))
    (let [b (llr/aclone2d a)
          _ (llr/ainc2d a 1 2)]
      (is (= (llr/aseq2d a) [[0 0 0] [0 0 999]]))
      (is (= (llr/aseq2d b) [[0 0 0] [0 0 998]])))))
