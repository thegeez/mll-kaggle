;; from https://github.com/nuroko/nurokit/blob/master/src/test/clojure/nuroko/demo/conj.clj
(ns mll.digits-feature
  (:require [nuroko.lab.core :as nc]
            [nuroko.gui.visual :as g]
            [task.core :as task]
            [mikera.vectorz.core :as v]            

            mll.digits-data)
  (:import [mikera.vectorz Op Ops]))

(def llr-pixels #{96 128 160 192 288 320 352 384 512 544 576 97 129 161 193 257 289 321 353 385 481 513 545 577 737 98 130 162 194 290 322 354 386 482 514 546 578 610 738 99 131 163 291 323 355 387 483 515 579 611 739 100 164 260 292 324 356 484 516 548 580 740 101 261 293 325 357 453 485 517 549 581 709 741 70 102 262 326 358 454 486 518 550 582 710 742 71 103 263 295 327 359 455 487 519 551 711 743 72 104 264 296 328 456 488 520 552 584 712 73 233 297 329 425 457 489 521 553 713 74 234 298 330 426 458 490 522 554 650 682 714 75 235 267 299 331 427 459 491 523 555 651 683 715 76 236 268 300 428 460 524 556 652 684 237 269 301 429 461 493 525 653 685 717 174 206 238 270 302 398 430 462 494 622 654 686 175 207 239 271 303 399 431 463 495 527 623 655 176 208 240 272 400 432 464 496 528 624 656 177 209 241 401 433 465 497 625 657 178 210 242 370 402 434 466 594 626 658 147 179 211 243 371 403 435 467 595 627 659 148 180 212 244 276 372 404 436 468 596 628 660 149 181 213 245 277 373 405 437 469 597 629 661 150 182 214 374 406 438 470 566 598 151 183 215 247 343 375 407 439 567 599 631 152 184 216 248 344 376 408 440 568 600 632 153 185 217 249 345 377 409 441 537 569 122 154 186 218 346 378 410 442 538 570 123 155 187 219 347 379 411 539 571 124 156 188 220 284 316 348 380 412 540 572 125 157 189 221 285 317 349 381 413 509 541 573 605 126 158 190 222 318 350 382 414 510 542 574 638 95 127 159 191 319 351 383 415 511 543 575 639})

(def llr-pixels-count (count llr-pixels))

(def data-in @mll.digits-data/data)

(def data (doall
           (for [row data-in]
             (v/vec (for [[idx d] (map-indexed list row)
                          :when (contains? llr-pixels idx)]
                      d)))))
(def labels @mll.digits-data/labels)

(comment
  (g/show (map g/img (take 100 data))
          :title "First 100 digits")
  ) 

(def INNER_SIZE 300) 

(def compress-task (nc/identity-task data))
  
(def compressor
     (nc/neural-network :inputs llr-pixels-count
                        :outputs INNER_SIZE
                        :layers 1
                        :output-op Ops/LOGISTIC
                        :dropout 0.5))
  
(def decompressor
     (nc/neural-network :inputs INNER_SIZE
                        :outputs llr-pixels-count
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
(def test-data-in  @mll.digits-data/test-data)
(def test-data (doall
           (for [row test-data-in]
             (v/vec (for [[idx d] (map-indexed list row)
                          :when (contains? llr-pixels idx)]
                      d)))))
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
  (def challenge-data-in @mll.digits-data/challenge-data)
  (def challenge-data (doall
           (for [row challenge-data-in]
             (v/vec (for [[idx d] (map-indexed list row)
                          :when (contains? llr-pixels idx)]
                      d)))))
  
  (count challenge-data)
  (mll.digits-data/write-challenge (map recognise challenge-data) "resources/kaggle-out-nn-10000-0-1-llr.csv")
  (g/show (map g/img (take 100 challenge-data))
          :title "First 100 challenge digits")

  (g/show (map recognise (take 100 challenge-data))
          :title "Recognition results")
  
  (g/show (map (fn [l r] (if (= l r) l (str r "*")))
               (take 100 labels)
               (map recognise (take 100 data)))
          :title "Recognition results")
  )

