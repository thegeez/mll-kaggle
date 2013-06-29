(ns mll.in-memory
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.pprint :as pprint]
            [clojure.set :as set]
            [clojure.string :as string])
  (:import [org.apache.mahout.math Matrix SparseRowMatrix MatrixSlice Vector Vector$Element DenseVector]
           [org.apache.mahout.math.function Functions]
           [org.apache.mahout.math.stats LogLikelihood]
           [java.io File DataOutputStream FileOutputStream OutputStream
            DataInputStream FileInputStream InputStream]))


(def n-cols (+ (* 28 28) 10)) ;; pixels + labels

(def col-dict (zipmap (concat (map (partial str "PIXEL_") (range (* 28 28)))
                              (map (partial str "LABEL_") (range 10)))
                      (range)))

(def inv-col-dict (zipmap (vals col-dict)
                          (keys col-dict)))

(defn read-occurrences-matrix [file-path n-rows]
  (let [
        ^Matrix m (SparseRowMatrix. n-rows n-cols
                            true ;; randomAccess
                            )]
    (with-open [in-file (io/reader (io/resource file-path))]
      ;; first row is header
      (doseq [[idx row] (map list (range) (drop 1 (csv/read-csv in-file)))]
        ;; first in row is label
        (let [label-str (str "LABEL_"(first row))
              label-col (get col-dict label-str)]
          (.set m (int idx) (int label-col) (double 1))
          (doseq [[p-idx pixel-str] (map list (range) (drop 1 row))
                  :when (< 128 (Long/parseLong pixel-str))
                  :let [pixel-col (get col-dict (str "PIXEL_" p-idx))]]
            (.set m (int idx) (int pixel-col) (double 1))))))
    (prn "m rowSize: " (.numRows m) " colSize: " (.numCols m))
    m))

(defn write-sorted-data [^Matrix m]
  (prn "Write sorted data")
  (let [^File sorted-data (File/createTempFile "raw" ".dat")
        ^OutputStream out (DataOutputStream. (FileOutputStream. sorted-data))]
    (. out (writeInt (. m rowSize)))
    (. out (writeInt (. m columnSize)))
    (doseq [^MatrixSlice row m]
      (let [non-zeroes-idxs (map #(. % index) (iterator-seq (.. row vector iterateNonZero)))]
        (. out (writeInt (count non-zeroes-idxs)))
        (doseq [idx non-zeroes-idxs]
          (. out (writeInt idx)))))
    (prn "Sorted data is " (double (/ (. sorted-data length) 1e6)) " MB")
    sorted-data))

(defn square-file [^File occurrences]
  (let [in (DataInputStream. (FileInputStream. occurrences))
        row-count (. in readInt)
        column-count (. in readInt)
        r (SparseRowMatrix. column-count column-count)]
    (dotimes [row row-count]
      (let [row (int row)
            non-zero-count (. in readInt)
            columns (int-array non-zero-count)]
        (dotimes [j non-zero-count]
          (let [v (. in readInt)]
            (aset columns j v)))

        (doseq [n columns
                m columns
                :when (not= n m)
                :let [old (. r (get (int n) (int m)))
                      new (double (+ 1 old))]]
          (. r (set (int n) (int m) new)))
        ))
    (prn "Cooccurence matrix done")
    r))


(defn -main [& args]
  (prn "Hello world in memory")
  (time
   (let [occurrences #_(read-occurrences-matrix "kaggle-provided/train_1000.csv" 1000)
         (read-occurrences-matrix "kaggle-provided/train_40000.csv" 40000)
         sorted-data (write-sorted-data occurrences)
         cooccurrence (square-file sorted-data)
         _ (prn "Starting sums")
         ^Vector row-sums (DenseVector. (. cooccurrence rowSize))
         ^Vector col-sums (DenseVector. (. cooccurrence columnSize))]
     (doseq [^MatrixSlice row cooccurrence]
       (. row-sums (set (. row index) (.. row vector zSum)))
       (. col-sums (assign (. row vector) Functions/PLUS)))
     (prn "Largest row sum " (. row-sums maxValue))
     (prn "Largest column sum " (. col-sums maxValue))
     (let [total (. row-sums zSum)]
       (prn "Scoring")
       (doseq [^MatrixSlice row cooccurrence
               [e-val e-idx] (map (fn [^Vector$Element e]
                                    [(. e get) (. e index)]) (iterator-seq (.. row vector iterateNonZero)))]
         (let [k11 e-val
               k12 (- (. row-sums (get (. row index))) k11)
               k21 (- (. col-sums (get e-idx )) k11)
               k22 (- total k11 k12 k21)
               score (LogLikelihood/rootLogLikelihoodRatio k11 k12 k21 k22)]
           (.set cooccurrence (int (. row index)) (int e-idx) score))))
     (prn "Filtering, only label rows")
     (let [indicator-mapping  (into {} (for [i (map (partial + (* 28 28)) (range 10))]
                                         (let [row (. cooccurrence (viewRow (int i)))
                                               es (map (fn [^Vector$Element e]
                                                         [(. e get) (. e index)])
                                                       (iterator-seq (.. row iterateNonZero)))
                                               indicators (take-while (comp (partial < 5) first)  (sort-by first > es))]
                                           [(get inv-col-dict i)  (map (comp (partial get inv-col-dict) second) indicators)])))]
       (prn "Overlap in indicators:")
       (->> (for [[l mapping] indicator-mapping
                  :let [mapping-set (set mapping)]]
              (into {:label l}
                    (for [[ol om] indicator-mapping]
                      {ol [(count mapping) (count (filter mapping-set om))]})))
            (pprint/print-table (into [:label] (map (partial str "LABEL_") (range 10)))))
       (doseq [[label pixels] indicator-mapping]
         (println label (string/join " " (sort pixels)))))
     ))
  
  (prn "Done"))
