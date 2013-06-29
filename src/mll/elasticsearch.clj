(ns mll.elasticsearch
  (:require [clj-http.client :as client]
            mll.digits-data
            mll.digits-feature
            [clojure.string :as string]))

(defn add-pixels [dl]
  (assoc dl :pixels (vec (map-indexed (fn [idx p]
                                        (if (zero? p)
                                          (str "ON_" idx)
                                          (str "OFF_" idx))) (:data dl)))))

(defn post-sample [dl]
  (let [pixels (string/join " " (for [[idx p] (map-indexed list (:pixels dl))
                                      :when (contains? mll.digits-feature/llr-pixels idx)]
                                  p))]
    (client/post "http://localhost:9200/pixels/sample"
                 {:form-params {:label (:label dl)
                                :pixels pixels}
                  :content-type :json})))

;;(:label (:_source (first (:hits (:hits (:body res))))))
(defn classify-sample [dl]
  (let [pixel-str (string/join "%20" (take 361 (:pixels dl)))
        q-str (str "http://localhost:9200/_search?q=" pixel-str "&pretty=1")
        res (client/get q-str
                        {:as :json})]
    (when-let [hits (seq (get-in res [:body :hits :hits]))]
      (:label (:_source (first hits))))))

(comment
  (client/get "http://localhost:9200/_search?q=software%20development^20&pretty=1" {:as :json})


  ;; curl -XPOST localhost:9200/wsnparis/talk -d '{
;; "speaker" : "Boris Bokowski",
;; "title" : "Introducing Orion: Embracing the Web for Software Development Tooling"
  ;; }'

  (client/post "http://localhost:9200/wsnparis/talk"
               {:form-params {:speaker "Rich Hickey"
                              :title "Simple ain't Easy"}
                :content-type :json})


  (let [data-in @mll.digits-data/data
        labels-in @mll.digits-data/labels
        n (long (* 0.9 (count data-in)))
        [data data-test] (split-at n data-in)
        [labels labels-test] (split-at n labels-in)]
    (def data (doall (map (comp
                           #(dissoc % :data)
                           add-pixels
                          (fn [d l]
                            {:label l
                             :data d})) data labels)))
    (def data-test
         (doall (map (comp
                      #(dissoc % :data)
                      add-pixels
                     (fn [d l]
                       {:label l
                        :data d})) data-test labels-test))))

  (doseq [dl data]
    (post-sample dl))

  (classify-sample (first data))
  (post-sample (first data))

  (client/delete "http://localhost:9200/")
  )
