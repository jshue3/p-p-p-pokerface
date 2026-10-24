(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn rank-counts [hand]
  (vals (frequencies (map rank hand))))

(defn pair? [hand]
  (= 2 (apply max (rank-counts hand))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (rank-counts hand))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (rank-counts hand))))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= [2 3] (sort (rank-counts hand))))

(defn two-pairs? [hand]
  (or (= [1 2 2] (sort (rank-counts hand)))
      (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [ranks      (map rank hand)
        low-ranks  (replace {14 1} ranks)
        consecutive? (fn [rs] (let [sorted (sort rs) start  (first sorted)] (= sorted (range start (+ start 5)))))]
    (or (consecutive? ranks)
        (consecutive? low-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1] [two-pairs? 2] [three-of-a-kind? 3] [straight? 4] [flush? 5] [full-house? 6] [four-of-a-kind? 7] [straight-flush? 8]}
        matching (filter (fn [checker] ((first checker) hand)) checkers)]
    (apply max (map second matching))))
