(ns day24.core
  (:require [clojure.string :as str] [clojure.set :as set]))

(def inp (str/split-lines (slurp "/Users/edd/git/aoc-2016/24/example.txt")))

(def targets [\0 \1 \2 \3 \4])

; pairs of [start target] to measure the distances for
(defn pairs []
  (apply concat
    (for [x (range (count targets))]
      (for [y (range (inc x) (count targets))]
        [(nth targets x) (nth targets y)]))))

(defn make-maze [i]
  (apply merge
    (for [y (range (count i))]
      (into {} (map #(vec [[(first %) y] (second %)])
                    (partition 2 (interleave (range) (nth i y))))))))
