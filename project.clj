(defproject mll "0.0.1"
  :jvm-opts ["-Xmx1380M"]
  
  :dependencies [[org.clojure/clojure "1.5.1"]
                 ;; references dependencies only, contains no source code
                 [com.nuroko/nurokit "0.0.2"]
                 [org.clojure/data.csv "0.1.2"]]
  :profiles {:dev {:plugins [[lein-swank "1.4.5"]]}})
