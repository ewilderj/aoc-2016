(ns day15.core)

(def discs [[13 1] [19 10] [3 2] [7 1] [5 3] [17 5]])
(defn d-at-t? [[m o] t] (= 0 (mod (+ o t) m)))
(defn win? [ds t]
  (every? identity (for [x (range (count ds))] (d-at-t? (nth ds x) (+ t x 1)))))
(defn find-winner [d] (first (filter some? (for [i (range)] (if (win? d i) i nil)))))
(defn part-one [] (find-winner discs))
(defn part-two [] (find-winner (conj discs [11 0])))
