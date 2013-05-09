;; from https://github.com/nuroko/nurokit/blob/master/src/test/clojure/nuroko/demo/conj.clj
(ns mll.digits
  (:require [nuroko.lab.core :as nc]
            [nuroko.gui.visual :as g]
            [task.core :as task]
            

            mll.digits-data)
  (:import [mikera.vectorz Op Ops]))

(def data @mll.digits-data/data)
(def labels @mll.digits-data/labels)

(comment
  (g/show (map g/img (take 100 data))
          :title "First 100 digits")
  ) 

(def INNER_SIZE 300) 

(def compress-task (nc/identity-task data))
  
(def compressor
     (nc/neural-network :inputs 784
                        :outputs INNER_SIZE
                        :layers 1
                        :output-op Ops/LOGISTIC
                        :dropout 0.5))
  
(def decompressor
     (nc/neural-network :inputs INNER_SIZE
                        :outputs 784
                        :layers 1))
  
(def reconstructor
     (nc/connect compressor decompressor))

(defn show-reconstructions []
  (let [reconstructor (.clone reconstructor)]
    (g/show
     (->> (take 100 data)
          (map (partial nc/think reconstructor))
          (map g/img))
     :title "100 digits reconstructed")))
(comment
  (show-reconstructions)
  )


(def trainer (nc/supervised-trainer reconstructor compress-task))

(comment
  (task/run
   {:sleep 1 :repeat 1000}
   (do
     (trainer reconstructor)
     (show-reconstructions)))
  )

(comment
  (task/stop-all)
  )
(defn feature-img [vector]
  ((g/image-generator :width 28 :height 28 :colour-function g/weight-colour-mono) vector))
(comment
  (g/show
   (map feature-img (g/feature-maps compressor :scale 2)) :title "Feature maps")
  )

;; now for the digit recognition
(def num-coder (nc/class-coder
                :values (range 10)))
(nc/encode num-coder 3)

(def recognition-task
     (nc/mapping-task
      (zipmap data labels)
      :output-coder num-coder))
  
(def recogniser
     (nc/neural-network :inputs INNER_SIZE
                        :outputs 10
                        :layers 3))
  
(def recognition-network
     (nc/connect compressor recogniser))

(def trainer2 (nc/supervised-trainer recognition-network
                                     recognition-task
                                     :loss-function nuroko.module.loss.CrossEntropyLoss/INSTANCE
                                     :learn-rate 0.1
                                     ))

;; test data and task - 10,000 cases
(def test-data @mll.digits-data/test-data)
(def test-labels @mll.digits-data/test-labels)
 
(def recognition-test-task
     (nc/mapping-task (zipmap test-data test-labels)
                   :output-coder num-coder))

(comment
  ;; show chart of training error (blue) and test error (red)
  (g/show (g/time-chart [#(nc/evaluate-classifier
                           recognition-task recognition-network )
                         #(nc/evaluate-classifier
                           recognition-test-task recognition-network )]
                        :y-max 1.0)
          :title "Error rate")
  
  (task/run
    {:sleep 1 :repeat 10000}
    (trainer2 recognition-network :learn-rate 0.1)) ;; demo used 0.3
     ;; can tune learn-rate, lower => fine tuning => able to hit better overall accuracy
    
  (task/stop-all)
  


  )

(defn recognise [image-data]
  (->> image-data
       (nc/think recognition-network)
       (nc/decode num-coder)))

(comment

  (recognise (nth data 0))
  
  ;; show results, errors are starred
  (g/show (map (fn [l r] (if (= l r) l (str r "*")))
               (take 100 labels)
               (map recognise (take 100 data)))
          :title "Recognition results")

  (let [rnet (.clone recognition-network)]
    (->> (map list test-labels test-data)
         (filter (fn [[l d]]
                   (= l (->> d
                             (nc/think rnet)
                             (nc/decode num-coder)))))
         (count)))
  
  (g/show (g/class-separation-chart recognition-network (take 1000 test-data) (take 1000 test-labels)))


  (task/stop-all)
  
  (g/show (map feature-img (g/feature-maps recognition-network :scale 10)) :title "Recognition maps")
  (g/show (map feature-img (g/feature-maps reconstructor :scale 10)) :title "Round trip maps") 
  ) 
(comment
  (def challenge-data @mll.digits-data/challenge-data)
  (count challenge-data)
  (mll.digits-data/write-challenge (map recognise challenge-data) "resources/kaggle-out-nn-10000-0-1.csv")
  (g/show (map g/img (take 100 challenge-data))
          :title "First 100 challenge digits")

  (g/show (map recognise (take 100 challenge-data))
          :title "Recognition results")
  
  (g/show (map (fn [l r] (if (= l r) l (str r "*")))
               (take 100 labels)
               (map recognise (take 100 data)))
          :title "Recognition results")
  )
