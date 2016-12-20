(ns day20.core
  (:require [clojure.string :as str]))

(def inp (str/split-lines (slurp "/Users/edd/git/aoc-2016/20/puzzle.txt")))
(defn lo-hi [s] (map #(Long/parseLong %) (rest (first (re-seq #"^(\d+)-(\d+)" s)))))
(defn ranges [] (sort-by first (map lo-hi inp)))

(defn consider-pair [[a b] [c d]] (if (>= b (dec c)) [[a (max b d)]] [[a b] [c d]]))

(defn combine-overlaps []
  (loop [r (rest (ranges)) v [(first (ranges))]]
    (if (empty? r) v
      (recur (rest r) (concat (drop-last v) (consider-pair (last v) (first r)))))))

(defn part-one [] (inc (second (first (combine-overlaps)))))
; data precludes 0 and the highest-ip, so count gaps
(defn part-two [] (reduce + (map (fn [[[a b] [c d]]] (- c b 1)) (partition 2 1 (combine-overlaps)))))
