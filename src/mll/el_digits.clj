(ns mll.el-digits
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.string :as string]
            [clojure.pprint :as pprint]
            [clj-http.client :as client]))

(def el-host "http://localhost:9200/")

(def el-domain "mll/digits")

(defn store-doc [fields]
  (client/post (str el-host el-domain)
               {:form-params fields
                :content-type :json}))

(defn store-indicators [file-path]
  (prn "deleting old index")
  (client/delete el-host)
  (prn "count: " (client/get (str el-host "/_count?q=*&pretty=1")))
  (doseq [line (string/split-lines (slurp (io/resource file-path)))]
    (let [[label & indicators] (string/split line #" ")]
      (store-doc {:label label :pixels indicators})))
  (prn "Await index to catch up")
  (Thread/sleep 30000)
  (prn "Number of docs:")
  (prn (client/get (str el-host "/_count?q=*&pretty=1"))))


(defn classify [{:keys [pixels]}]
  (let [pixel-str (string/join " " pixels)]
    (-> (client/get (str el-host "_search?q=pixels:" pixel-str)
                   {:as :json})
        (get-in [:body :hits :hits])
        first
        (get-in [:_source :label]))))

(defn -main [& args]
  (prn "Hello world el-digits")
  (store-indicators "digits_40000_indicators.txt")
  (prn "find something:")
  (prn (client/get (str el-host "_search?q=pixels:PIXEL_512&pretty=1")
                   {:as :json}))
  (let [cases (for [[label & pixels-str] (drop 1 (csv/read-csv (io/reader (io/resource "kaggle-provided/train_cross_2000.csv"))))]
                {:label-orig (str "LABEL_ORIG_" label)
                 :pixels (for [[p-idx p-str] (map list (range) pixels-str)
                               :when (< 128 (Long/parseLong p-str))]
                           (str "PIXEL_" p-idx))})]
        
    (prn "first case" (first cases) (classify (first cases)))
    (let [classified (map #(assoc % :label-classified (classify %)) cases)
          by-label (group-by :label-orig classified)
          by-label-label (zipmap (keys by-label)
                                 (map #(frequencies (map :label-classified %)) (vals by-label)))]
      (->> (for [[l mapping] by-label-label]
             (into {:label-orig l} mapping))
           (sort-by :label-orig)
           (pprint/print-table (into [:label-orig] (map (partial str "LABEL_") (range 10)))))
      )))
