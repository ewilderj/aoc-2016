(ns day18.core
  (:require [clojure.string :as str]))

(def inp "^.....^.^^^^^.^..^^.^.......^^..^^^..^^^^..^.^^.^.^....^^...^^.^^.^...^^.^^^^..^^.....^.^...^.^.^^.^")

(defn nextrow [^String s]
  (->> (str/join ["." s "."])
    (partition 3 1)
    (map #{[\^ \^ \.] [\. \^ \^] [\^ \. \.] [\. \. \^]})
    (map #(if % \^ \.))
    (str/join)))

(defn part-one []
  ((frequencies (str/join (take 40 (iterate nextrow inp)))) \.))

(defn part-two []
  ((frequencies (str/join (take 400000 (iterate nextrow inp)))) \.))
