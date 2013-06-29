(ns mll.llr-digits  
  (:require mll.digits-data
            [mll.llr :as llr]
            [nuroko.gui.visual :as g]
            [mikera.vectorz.core :as v]
            [incanter.core :as icore]
            [incanter.charts :as icharts]))

(defn normalize [xs]
  (let [best (reduce max xs)]
    (for [x xs
          :let [norm (if (zero? best)
                       0.0
                       (/ x best))]]
      (if (> norm 0.0)
        norm
        0.0))))

(defn relevant-pixels [llrs]
  (let [norms (map normalize (vals llrs))
        pixels (map
                (fn [norm]
                  (set (for [[idx p] (map-indexed list norm)
                             :when (> p 0.3)]
                         idx)))
                norms)]
    (reduce into #{} pixels)
    ))

(comment 
  (def data @mll.digits-data/data)
  (def labels @mll.digits-data/labels)

  (def rows (doall
             (map #(into [%1] %2) labels data)))

  (do
    #_(prn "cc")
    #_(time (def counts (llr/collect-counts rows)))
    (prn "ccllr")
    (time (def llr (llr/collect-llr rows)))
    #_(prn "ccf")
    #_(time (def fcounts (llr/collect-counts-fast rows)))
    #_(prn "ccfllr")
    #_(time (def fllr (llr/collect-llr-fast rows)))
    #_(prn "llr eq?")
    #_(time (= llr fllr)))
  
  (g/show [(g/img (v/vec (for [i (range (* 28 28))]
                           (/ i (* 28 28)))))])
  
  (g/show (map (comp g/img v/vec rest) (take 100 rows))
          :title "samples")
  
  (g/show (map (comp g/img v/vec) (map #(map :llr %) (vals llr)))
          :title "llr")

  (g/show (map (comp g/img v/vec normalize) (map #(map :llr %) (vals llr)))
          :title "Norm llr")

  (doseq [[k v] llr]
    (let [vs (normalize v)]
      (let [h1 (icharts/histogram vs
                                  :series-label (str "label: " k))]
        (icore/view h1)
        (icharts/add-histogram h1 vs
                               :series-label (str "second " k)))))
  ;; remove [0.0 0.3] as noise, leaves ~16% 
  (doseq [[k v] llr]
    (icore/view (icharts/histogram (map :llr v)
                                   :title (str k))))
  
  (g/show (map (comp str count (partial filter #(< 0.3 %)) normalize) (map #(map :llr %) (vals llr)))
          :title "rel pixs")

  (g/show (map (comp str count (partial llr/keep-n-best identity (long (* 0.20 (* 28 28))))) (map #(map :llr %) (vals llr)))
          :title "rel pixs")

  ;; 0.2 per cat
  (count (set (flatten (map (comp #(map first %) (partial llr/keep-n-best second (long (* 0.10 (* 28 28))))) (map #(map-indexed (fn [i p] [i (:llr p)]) %) (vals llr))))))

  (count (set (flatten (map first (llr/keep-n-best second (long (* 0.20 (* 28 28))) (mapcat #(map-indexed (fn [i p] [i (:llr p)]) %) (vals llr)))))))

  ;; 

  (g/show (map (comp #(g/img % )
                     v/vec
                     (fn [px]
                       (let [best-indices (set (map first
                                                    (remove (fn [[idx p]]
                                                              (zero? (:AB p)))
                                                     (llr/keep-n-best
                                                            (comp (fn [p]
                                                                    (if false #_(> (:AnB p)
                                                                           (:AB p))
                                                                      0.0
                                                                      (:llr p)))

                                                                  second) (long (* 0.20 (* 28 28)))
                                                            (map-indexed list px)))))]
                         (map-indexed
                          (fn [i p]
                            (if (contains? best-indices i)
                              (if false #_(< (:AB p) 2000)
                                  0.0
                                  (*
                                   (if (< (:AnB p)
                                          (:AB p))
                                     1
                                     -1)
                                   (Math/log (/ (+ (:AB p) (:AnB p) (:nAB p) (:nAnB p)) (+ 0.00001 (:B p)))))
                                  #_(if (< (/ 1 (:nB p)) (/ (:B p)))
                                    #_(< 2000 (:AB p))
                                  #_(> (:AB p) (:nAB p))
                                 1.0
                                 -1.0))
                              0.0))
                          px)))) (vals llr))
          :title "rel pixs top 20%- cut AB")

  ;; leaning of pos or neg indicator of best pixels
  (doseq [[k px] llr]
    (let [vs (filter (comp not zero?) (let [best-indices (set (map first (llr/keep-n-best (comp :llr second) (long (* 0.20 (* 28 28)))
                                                                  (map-indexed list px))))]
                (map-indexed
                 (fn [i p]
                   (if (contains? best-indices i)
                     (Math/abs (- (:B p) (:nB p)))
                     0.0))
                 px)))]
      (icore/view (icharts/histogram vs
                                     :title (str "leaning best pixs: " k)))))
  
  (g/show (map (comp g/img v/vec (partial map #(if (< 0.3 %) 1.0 0.0)) normalize) (map #(map :llr %) (vals llr)))
          :title "rel pixs")
  
  (g/show (map (comp g/img v/vec (partial map #(if (< 0.3 %) 1.0 0.0)) normalize) (map #(map :llr %) (vals llr)))
          :title "rel pixs")
 
  ;; relevant pixels per label (cutoff 0.3):
  ;; 1000 samples ("71" "130" "124" "108"
  ;; "105" "49" "92" "113" "72" "102")
  ;; 40000 samples ("87" "131" "126" "111" "85" "62" "97" "122" "93" "99")

  ;; relevant pixels per label (cutoff 0.0): ("612" "612" "612" "611"
  ;; "612" "612" "612" "612" "612" "612")

  (def match-to (zipmap (keys llr)
                        (map llr/best-indices-lean (vals llr))))

  (def out (llr/match match-to (first rows)))

  (sort-by (comp #(+ (::mll.llr/PS %) (::mll.llr/nPnS %)) val) > out)

  (def s (frequencies (map #(= (first %) (llr/classify match-to (vec (rest %)))) (take 100000 rows))))

  (def ss (frequencies (map #(let [p (llr/classify match-to (vec (rest %)))]
                               [(first %) (= (first %) p)]) (take 1000 rows))))

  (def iw (llr/idx-weight llr))

  (def norm-iw (zipmap (keys iw)
                       (map
                        (fn [sum ws]
                          (zipmap (keys ws)
                                  (map #(/ % sum)
                                       (vals ws))))
                        (map (fn [s] (reduce + (map #(Math/abs %) (vals s)))) (vals iw))
                        (vals iw))))
  
  (g/show (map (comp g/img v/vec (fn [ws]
                                   (map #(get ws % 0.0) (range (* 28 28))))) (vals iw))
          :title "idx-weight")

    (g/show (map (comp g/img v/vec (fn [ws]
                                   (map #(get ws % 0.0) (range (* 28 28))))) (vals norm-iw))
          :title "idx-weight")

  (doseq [[k px] iw]
    (let []
      (icore/view (icharts/histogram (vals px)
                                     :title (str "w dist: " k)))))
  
  (def ss (frequencies (map #(let [r (vec (rest %))
                                   p (first (apply max-key second
                                                   (map
                                                    (fn [[l s]]
                                                      (prn "l s" l s)
                                                      [l s])
                                                    (for [[l ps] norm-iw]
                                                      [l (reduce +
                                                                 (map (fn [[p s]]
                                                                        ((fn [n] (* n n)) (if (< 0 s)
                                                                                            (* 2 (if (zero? (nth r p))
                                                                                               0.0
                                                                                               s))
                                                                                            (if (zero? (nth r p))
                                                                                              (* -1 s)
                                                                                              0.0))))
                                                                      ps))]
                                                      ))))]
                               [(first %) (= (first %) p)]) (take 100 rows))))

  (g/show (map (comp g/img v/vec) (take 10 rows)))
  )






