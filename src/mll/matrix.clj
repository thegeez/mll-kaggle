(ns mll.matrix
  (:require [cascalog.api :refer :all]
            [cascalog.more-taps :as mt])
  (:import [org.apache.hadoop.io IntWritable]
           [org.apache.mahout.math VectorWritable DenseVector]))

;; with http://mail-archives.apache.org/mod_mbox/mahout-user/201301.mbox/%3C50CFD234CC7D3A4EA1E8910D3866F700095256F2AE@NDA-HCLC-EVS02.HCLC.CORP.HCL.IN%3E

(def matrix
     (map list (range)
          [[1 2 3]
           [4 5 6]]))

(defn to-int-writable [in]
  (doto (IntWritable.)
    (.set in)))

(defn to-vector-writable [in]
  (doto (VectorWritable.)
    (.set (DenseVector. (double-array (seq in))))))

(defn -main [& args]
  (prn "Hello world")
  (let [uri (or (first args)
                "file:///home/mfex/Programming/mll/resources/matrix-out/")
        ]
    (?- (mt/lfs-wrtseqfile uri IntWritable VectorWritable
                           :outfields ["?row-id-iw" "?row-vector"])
        (<- [?row-id-iw ?row-vector]
            (matrix ?row-id ?row)
            (to-int-writable ?row-id :> ?row-id-iw)
            (to-vector-writable ?row :> ?row-vector)))
    ))
