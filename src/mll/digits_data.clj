(ns mll.digits-data
  (:require [nuroko.gui.visual :as g]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [mikera.vectorz.core :as v]))

(def train-csv
     "kaggle-provided/train_40000.csv"
     #_"kaggle-provided/train_1000.csv")

(def test-csv "kaggle-provided/train_cross_2000.csv")

(def challenge-csv "kaggle-provided/test.csv")

(def ub-to-double-factor (double (/ 1.0 255.0)))

(def data (delay
           (with-open [in-file (io/reader (io/resource train-csv))]
             (doall
              (for [row (drop 1 (csv/read-csv in-file))]
                ;; first in row is label
                (v/vec (->> (drop 1 row)
                            (map #(-> %
                                      Long/parseLong
                                      (* ub-to-double-factor))))))))))

(def labels (delay
             (with-open [in-file (io/reader (io/resource train-csv))]
             (doall
              (map #(Long/parseLong (first %)) (drop 1 (csv/read-csv in-file)))))))

(def test-data (delay
           (with-open [in-file (io/reader (io/resource test-csv))]
             (doall
              (for [row (drop 1 (csv/read-csv in-file))]
                ;; first in row is label
                (v/vec (->> (drop 1 row)
                            (map #(-> %
                                      Long/parseLong
                                      (* ub-to-double-factor))))))))))

(def test-labels (delay
                  (with-open [in-file (io/reader (io/resource test-csv))]
                    (doall
                     (map #(Long/parseLong (first %)) (drop 1 (csv/read-csv in-file)))))))


(def challenge-data (delay
                     (with-open [in-file (io/reader (io/resource challenge-csv))]
                       (doall
                        (for [row (drop 1 (csv/read-csv in-file))]
                          (v/vec (map #(-> %
                                           Long/parseLong
                                           (* ub-to-double-factor)) row)))))))

(defn write-challenge [results out-file]
  (with-open [out-file (io/writer out-file)]
    (csv/write-csv out-file
                   (map vector results))))

(defn compare-scores []
  (let [results {:knn (with-open [in-file (io/reader (io/resource "kaggle-provided/knn_benchmark.csv"))]
                       (doall (csv/read-csv in-file)))
                 :rf (with-open [in-file (io/reader (io/resource "kaggle-provided/rf_benchmark.csv"))]
                      (doall (csv/read-csv in-file)))
                 :nn (with-open [in-file (io/reader (io/resource "kaggle-out-nn-10000-0-1.csv"))]
                       (doall (csv/read-csv in-file)))}
        pairs (let [ks (keys results)]
                (for [i (range (count ks))
                      j (range i (count ks))
                      :when (< i j)]
                  [(nth ks i) (nth ks j)]))]
    (into {} (map
              (fn [[l r]]
                (let [diff (->> (map list (results l) (results r))
                                (filter (fn [[l r]]
                                          (not= l r)))
                                (count))
                      pct (* 100.0 (/ diff
                                      (count (results l))))]
                  {[l r :diff] diff
                   [l r :pct]  pct}))
              pairs))))
