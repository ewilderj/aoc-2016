(ns day24.core
  (:require [clojure.string :as str] [clojure.set :as set]))

(def inp (str/split-lines (slurp "/Users/edd/git/aoc-2016/24/example.txt")))

(def targets [\0 \1 \2 \3 \4])

; pairs of [start target] to measure the distances for
(defn pairs [ts]
  (apply concat
    (for [x (range (count ts))]
      (for [y (range (inc x) (count ts))]
        [(nth ts x) (nth ts y)]))))

; eval route lngths for (permutations (rest targets)))
; (map #(cons \0 %) (permutations (rest targets)))
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
