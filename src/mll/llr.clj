(ns mll.llr)

(defn llr [AB AnB nAB nAnB]
  (let [H (fn [ks]
            (let [N (reduce + ks)]
              (->> (for [k ks
                         :when (not (zero? N))
                         :let [kN (/ k N)]
                         :when (not (zero? kN))]
                     (* kN (Math/log kN)))
                   (reduce +))))]
    (* 2
       (+ AB AnB nAB nAnB)
       (- (let [Hall (H [AB AnB nAB nAnB])]
            ;;(prn "Hall" Hall)
            Hall)
          (let [Hrow (H [(+ AB AnB) (+ nAB nAnB)])]
            ;;(prn "Hrow" Hrow)
            Hrow)
          (let [Hcol (H [(+ AB nAB) (+ AnB nAnB)])]
            ;;(prn "Hcol" Hcol)
            Hcol)))))

;; http://en.wikipedia.org/wiki/Likelihood-ratio_test 
(defn llr2 [AB AnB nAB nAnB]
  (let [obs {1 {1 AB  2 AnB}
             2 {1 nAB 2 nAnB}}
        k (fn [i j]
            (get-in obs [i j]))
        n (fn [i j]
            (/ (k i j)
               (+ (k i 1)
                  (k i 2))))
        sum-obs (reduce + (map #(reduce + (vals %)) (vals obs))) 
        m (fn [j]
            (/ (+ (k 1 j)
                  (k 2 j))
               sum-obs))
           ]
    (* 2
      (reduce + (for [i [1 2]
                      j [1 2]]
                  (* (k i j)
                     (Math/log (/ (n i j)
                                  (m j)))))))))

(defn root-llr [AB AnB nAB nAnB]
  ;;  total = sum(k)
  ;;  s = sign(k[1,1] - (k[1,1] + k[1,2]) * (k[1,1] + k[2,1]) / total)
  ;;  s * sqrt(llr(k))
  (let [total (+ AB AnB nAB nAnB)
        sign (fn [d]
               (cond
                (zero? d) 0
                (pos? d) 1
                :else -1))
        s (sign (- AB (/ (* (+ AB AnB) (+ AB nAB))
                         total)))]
    (* s 0.5 (Math/sqrt (llr AB AnB nAB nAnB)))))

(defn collect-counts [rows]
  (let [;; matrix indices for LLR per label, stored in vector
        AB 0
        AnB 1
        nAB 2
        nAnB 3

        ;; accumulator holds a seed score
        ;; this is to keep track of nAB and nAnB, while AB and AnB are
        ;; 0, so new labels don't need a scan of
        ;; all previous rows
        init {::unseen (repeat (dec (count (first rows)))
                               [0 0 0 0])}
        ]
    (-> (reduce
         (fn [scores [label & row]]
           (let [scores (if (contains? scores label)
                          scores
                          (assoc scores label (get scores ::unseen)))]
             (into {}
                   (for [[k old] scores]
                     (let [A (= k label)]
                       [k (doall
                           (map
                            (fn [m bit]
                              (let [B (not (zero? bit))]
                                (update-in m [(cond
                                               (and A B) AB
                                               (and (not A) B) nAB
                                               (and A (not B)) AnB
                                               (and (not A) (not B)) nAnB)] inc)))
                            old row))])))))
         init rows)
        (dissoc ::unseen))))

;; deep clone array 2 levels deep
(defn aclone2d [orig]
  (let [outer-length (alength orig)
        inner-length (alength (aget orig 0))
        out (make-array Long/TYPE outer-length inner-length)]
    (dotimes [i outer-length]
      (dotimes [j inner-length]
        (aset out i j (aget orig i j))))
    out))

(defn aseq2d [in]
  (seq (map seq in)))

(defn ainc2d [arr i j]
  (aset arr i j (inc (aget arr i j))))

(defn ainc3d [arr i j k]
  (aset arr i j k (inc (aget arr i j k))))

(defn collect-counts-fast [rows]
  (let [;; matrix indices for LLR per label, stored in vector
        AB 0
        AnB 1
        nAB 2
        nAnB 3

        label-count 10
        row-count (dec (count (first rows)))
        scores (make-array Long/TYPE
                           label-count
                           ;; 28 * 28
                           row-count
                           ;; AB AnB nAB nAnB counts
                           4)]
    (doseq [row rows]
      (let [label (nth row 0)]
        (dotimes [i row-count]
          (let [B (not (zero? (nth row (inc i))))]
            (dotimes [k label-count]
              (let [A (= k label)
                    j (cond
                       (and A B) AB
                       (and (not A) B) nAB
                       (and A (not B)) AnB
                       (and (not A) (not B)) nAnB)]
                (ainc3d scores k i j)))))))
    (prn "to clj map")
    (into {}
          (map-indexed vector (map #(seq (map seq %)) scores)))))


(defn collect-llr [rows]
  (let [counts (collect-counts rows)]
    (into {}
          (for [[label ms] counts]
            [label (doall (for [[AB AnB nAB nAnB :as scores] ms]
                            {:llr (llr AB AnB nAB nAnB)
                             :AB AB
                             :AnB AnB
                             :nAB nAB
                             :nAnB nAnB
                             :A (+ AB AnB)
                             :nA (+ nAB nAnB)
                             :B (+ AB nAB)
                             :nB (+ AnB nAnB)}))]))))

(defn collect-llr-fast [rows]
  (let [counts (collect-counts-fast rows)]
    (into {}
          (for [[label ms] counts]
            [label (doall (for [[AB AnB nAB nAnB :as scores] ms]
                            {:llr (llr AB AnB nAB nAnB)
                             :A (+ AB AnB)
                             :nA (+ nAB nAnB)}))]))))

(defn keep-n-best [k n coll]
  (take n (sort-by k > coll)))

(defn best-indices-lean [scores]
  ;; use 0.2 pct of pixels
  (into {} (for [[idx score] (keep-n-best (comp :llr second) (long (* 0.20 (* 28 28)))
                                          (map-indexed list scores))]
             [idx (< 2000 (:AB score))])))

(defn match [match-to row]
  (zipmap (keys match-to)
          (for [pxs (vals match-to)]
            (frequencies
             (map-indexed
              (fn [ridx p]
                (if (contains? pxs ridx)
                  (let [lean (get pxs ridx)
                        on (< 0.0000001 p)]
                    (cond
                     (and on lean) ::PS
                     (and (not on) lean) ::nPS
                     (and on (not lean)) ::PnS
                     (and (not on) (not lean)) ::nPnS))
                  ::irrelevant))
              row)))))

(defn classify [match-to row]
  (let [m (match match-to row)]
    (ffirst (sort-by (comp
                      #_(::mll.llr/PS % 0)
                      #(+ (::mll.llr/PS % 0) (::mll.llr/nPnS % 0))
                      #_(- (+ (::mll.llr/PS % 0) (::mll.llr/nPnS % 0))
                               (+ (::mll.llr/PnS % 0) (::mll.llr/nPS % 0))) val) > m))))

(defn idx-weight [llr]
  (zipmap (keys llr)
          (for [px (vals llr)]
            (into {}
             (for [[idx p] (keep-n-best
                            (comp :llr
                                  second) (long (* 0.20 (* 28 28)))
                                  (map-indexed list px))
                   :when (not (zero? (:AB p)))
                   :let [s (* (if (< (:AnB p)
                                    (:AB p))
                                1
                                -1)
                               (Math/log (/ (+ (:AB p) (:AnB p) (:nAB p) (:nAnB p)) (+ 0.00001 (:B p)))))]]
               [idx s])))))

(defn cooccurrence-matrix [hist]
  (let [inv-hist (apply map list hist)]
    (for [r inv-hist]
      (for [c inv-hist]
        (reduce + (map * r c))))))
