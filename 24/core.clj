(ns day24.core
  (:require [clojure.string :as str] [clojure.set :as set]))

(def inp (str/split-lines (slurp "/Users/edd/git/aoc-2016/24/puzzle.txt")))

(def targets [\0 \1 \2 \3 \4 \5 \6 \7])

(defn pairs [ts]
  (apply concat
    (for [x (range (count ts))]
      (for [y (range (inc x) (count ts))]
        [(nth ts x) (nth ts y)]))))

(defn permutations [s]
  (if (seq (rest s))
     (apply concat
       (for [x s] (map #(cons x %) (permutations (remove #{x} s)))))
     [s]))

(defn locate [m t]
  ((set/map-invert m) t))

(defn make-maze [i]
  (apply merge
    (for [y (range (count i))]
      (into {} (map #(vec [[(first %) y] (second %)])
                    (partition 2 (interleave (range) (nth i y))))))))

(defn open-neighbors [m [x y]]
  (let [c [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]]
    (filter (fn [p] (and
                     (some? (m p))
                     (not (= (m p) \#))))
      c)))

; breadth first search; finds t2 from t1
(defn distance [m [t1 t2]]
  (let [a (locate m t1) b (locate m t2)]
    (loop [queue [a] dist {a 0}]
      (if (= b (first queue))
        (dist b)
        (let [cur (first queue)
              cn (filter #(not (dist %)) (open-neighbors m cur))
              nd (merge (into {} (map #(vec [% (inc (dist cur))]) cn)) dist)
              nq (concat (rest queue) cn)]
            (recur nq nd))))))

(defn distances [m]
  (apply assoc {}
    (interleave (pairs targets) (map #(distance m %) (pairs targets)))))

(defn best-route [return-to-base]
  (let [ds (distances (make-maze inp))
        rs (if return-to-base
             (map #(concat [\0] % [\0]) (permutations (rest targets)))
             (map #(cons \0 %) (permutations (rest targets))))]
    (for [r rs]
      [r (reduce + (map #(ds (sort %)) (partition 2 1 r)))])))

(defn part-one []
  (second (first (sort-by second (best-route false)))))

(defn part-two []
  (second (first (sort-by second (best-route true)))))
