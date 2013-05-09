;; from https://github.com/nuroko/nurokit/blob/master/src/test/clojure/nuroko/demo/conj.clj
(ns mll.scrabble
  (:require [nuroko.lab.core :as nc]
            [nuroko.gui.visual :as g]
            [task.core :as task]))

(def scores (sorted-map \a 1, \b 3, \c 3, \d 2, \e 1
                        \f 4, \g 2, \h 4, \i 1, \j 8
                        \k 5, \l 1, \m 3, \n 1, \o 1
                        \p 3, \q 10,\r 1, \s 1, \t 1
                        \u 1, \v 4, \w 4, \x 8, \y 4
                        \z 10))

;; scores 1 through 10 are 10 classes, can be encoded in 4 bits
(def score-coder (nc/int-coder :bits 4))

(comment
  (nc/encode score-coder 3)
  (nc/decode score-coder *1)
  )

(def letter-coder
     (nc/class-coder :values (keys scores)))

(comment
  (nc/encode letter-coder \c)
  )

(def task
     (nc/mapping-task scores
                      :input-coder letter-coder
                      :output-coder score-coder))

(def net
     (nc/neural-network :inputs 26
                        :outputs 4
                        :hidden-sizes [6]))
(comment
  (g/show (g/network-graph net :line-width 2)
          :title "Neural Net : Scrabble")
  )

(defn scrabble-score [net letter]
  (->> letter
       (nc/encode letter-coder)
       (nc/think net)
       #_((fn [res]
          (prn "res" res)
          res))
       (nc/decode score-coder)))

(comment
  (scrabble-score net \a)
  )

;; evaluation function
(defn evaluate-scores [net]
  (let [net (.clone net)
        chars (keys scores)]
    (count (for [c chars
                 :when (= (scrabble-score net c)
                          (scores c))]
             c))))

(comment
  (g/show (g/time-chart
           [#(evaluate-scores net)]
           :y-max 26)
          :title "Correct letters")
  )

(def trainer (nc/supervised-trainer net task))

(comment
  (task/run
   {:sleep 10 :repeat 1000}
   (trainer net))

  (scrabble-score net \x)
  
  (task/stop-all)
  )
