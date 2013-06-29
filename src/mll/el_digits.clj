(ns mll.el-digits
  (:require [clojure.java.io :as io]))

(defn store-indicators [file-path]
  (let [lines (slurp (io/resource file-path))]
    (prn "lines: " lines)))

(defn -main [& args]
  (prn "Hello world el-digits")
  (store-indicators "digits_40000_indicators.txt"))
