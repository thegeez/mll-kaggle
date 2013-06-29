(defproject mll "0.0.1"
  :jvm-opts ["-Xmx1380M"]
  
  :dependencies [[org.clojure/clojure "1.5.1"]
                 ;; references dependencies only, contains no source code
                 [com.nuroko/nurokit "0.0.2"]
                 [org.clojure/data.csv "0.1.2"]
                 [incanter "1.4.1"]

                 [clj-http "0.7.2"]
                 [org.clojure/data.json "0.2.2"]

                 [cascalog "1.10.0"]
                 [cascalog/cascalog-more-taps "1.10.1"]
                 
                 [org.apache.mahout/mahout-core "0.7"]
                 ]
  :profiles {:dev {:dependencies [[org.apache.hadoop/hadoop-core "0.20.2-dev"]]
                   :plugins [[lein-swank "1.4.5"]]}})
