(ns eddpod.day6
  (:require [clojure.string :as str]))

(def inp (str/split-lines (slurp "/Users/edd/git/aoc-2016/06/puzzle.txt")))

(def part1
  (str/join
    (for [n (range 8)]
      (first (last (sort-by last (frequencies (map #(nth % n) inp))))))))

(def part2
  (str/join
    (for [n (range 8)]
      (first (first (sort-by last (frequencies (map #(nth % n) inp))))))))
