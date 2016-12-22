(ns day22.core
  (:require [clojure.string :as str]))

(def inp (drop 2 (str/split-lines (slurp "/Users/edd/git/aoc-2016/22/puzzle.txt"))))

(defn parse-line [l]
  (->> l
    (re-find #"x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)")
    (rest)
    (map #(Integer/valueOf %))))

(defn read-input [ls]
  (loop [i ls a {}]
    (if (empty? i) a
      (let [[x y si us av] (parse-line (first i))]
        (recur (rest i) (assoc a [x y] [si us av]))))))

(defn viable-pair? [[[x y] [u a]] [[ex ey] [eu ea]]]
    (not (or (= 0 u) (> u ea) (= [x y] [ex ey]))))

(defn part-one []
  (let [a (read-input inp)
        entries (for [x (range 34) y (range 30)]
                  (vec [[x y] (rest (a [x y]))]))]
    (reduce + (for [n entries]
                (count (filter #(viable-pair? n %) entries))))))

(defn make-grid [a]
  (partition 34
    (for [y (range 30) x (range 34)]
      (let [[si us av] (a [x y])]
        (cond
          (>= si 500) \#
          (= 0 us) \_
          (= [33 0] [x y]) \G
          :else \.)))))

; to print grid, use
; (println (str/join "\n" (map str/join (make-grid (read-input inp)))))

; part two, let's look at a few assumptions about the data
; first, there's exactly one empty node (17 6) capacity 89T
; our target data G is currently in (33 0) sized 71T
; every node has at least 85T capacity
; nodes with >500T capacity are too full to use

; so, given G in the top-right, its goal is to get to 0,0
; the cost of each move is the cost of emptying the node that
; we want to move to.
;
; so, this starts to look like two embedded A* search problems
; the outer one is relatively simple to figure out cost functions
; for
;
; the inner one is, what's the quickest way to empty any node?
; well, that looks like a search problem finding the quickest
; path of the empty node to the target node.
;
; permissible moves for the empty node are any adjacent nodes
; that can be swapped with.
;
; we can start by transforming the grid into dots, hashes and G

;
; pertinent part of grid is this-

; .................................G
; ..................................
; ..................................
; ..................................
; ..................................
; ........##########################
; ................._................

; grid looks like this. you can actually
; then hand calculate how far it needs
; to go.
; from 17 6 to 32 0, going around the
; wall at (8 5)
;
; 17-801 = 10 to the left to reach (7 6)
; 6-0    = 6 up to reach (7 0)
; 32-7   = 25 to the right to reach (32 0)
; total  = 41
;
; then we cost what it takes to move each
; G left.
; to get G to (1 0) with the underscore
; to its left, we need 5 moves per x move of G
; so 33-1 = 32 x 5 = 160
; plus one more to move G = 161
; so total = 41 + 161 = 202
