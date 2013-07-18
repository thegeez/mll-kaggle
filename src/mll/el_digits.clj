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
    (let [[label & indicators] (string/split line #" ")
          pixels-on (string/join " " (filter #(.startsWith % "PIXEL_ON_") indicators))
          pixels-off (string/join " " (filter #(.startsWith % "PIXEL_OFF_") indicators))]
      (store-doc {:label label :pixelson pixels-on :pixelsoff pixels-off})))
  (prn "Await index to catch up")
  (Thread/sleep 15000)
  (prn "Number of docs:")
  (prn (client/get (str el-host "/_count?q=*&pretty=1"))))


(defn classify [{:keys [pixels-on pixels-off]}]
  (let [pixel-on-str (string/join " " pixels-on)
        pixel-off-str (string/join " " pixels-off)]
    (->
     ;; give http line larger than 4k exception when using query string
     #_(client/get (str el-host "_search?q=pixelson:" pixel-on-str " AND pixelsoff:" pixel-off-str)
                   {:as :json})
     #_(client/get (str el-host "_search?q=label:LABEL_5")
                   {:as :json})
     #_(client/post (str el-host "_search")
                  {:as :json
                   ;;:match {:label {:query "LABEL_9"}}
                   "query" {"query_string" {"query" "label:LABEL_9"}}
                   ;;{:query_string {:query "label:LABEL_9"}}
                   #_{:term {:label "LABEL_9"}
                    #_{:pixelson pixel-on-str
                       :pixelsoff pixel-off-str}}})
     (client/post (str el-host "_search")
                  {:as :json
                   :content-type :json
                   :form-params {
                                 "query" {"bool" {"should" [{"field" {"pixelson" pixel-on-str}}
                                                            {"field" {"pixelsoff" pixel-off-str}}]}}}})
     #_((fn [res]
           (prn "res" res)
           res))
     (get-in [:body :hits :hits])
     first
     
     (get-in [:_source :label]))))

(defn -main [& args]
  (prn "Hello world el-digits")
  (store-indicators "digits_40000_indicators_with_off.txt")
  (prn "find something:")
  (prn (client/get (str el-host "_search?q=label:LABEL_1&pretty=1")
                   {:as :json}))
  (let [cases (for [[label & pixels-str] (take 20000 (drop 1 (csv/read-csv (io/reader (io/resource "kaggle-provided/train_cross_2000.csv")))))]
                {:label-orig (str "LABEL_ORIG_" label)
                 :pixels-on (for [[p-idx p-str] (map list (range) pixels-str)
                                  :when (< 128 (Long/parseLong p-str))]
                              (str "PIXEL_ON_" p-idx))
                 :pixels-off (for [[p-idx p-str] (map list (range) pixels-str)
                                   :when (> 10 (Long/parseLong p-str))]
                               (str "PIXEL_OFF_" p-idx))})]
        
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
