(ns eddpod.day3
  (:require [clojure.string :as str]))


(def inp (str/split-lines (slurp "/Users/edd/git/adventofcode/03/puzzle.txt")))

(defn strings-to-sorted-ints [s]
    (sort (map #(Integer. %) s)))

(defn parse-sorted-dimensions [i]
    (map (fn [l] (strings-to-sorted-ints (str/split (str/trim l) #"\s+")))
      i))

(defn possible-triangles [i]
  (filter (fn [[a b c]] (> (+ a b) c)) i))

(defn answer-part-one []
  (count (possible-triangles (parse-sorted-dimensions inp))))

; part 2

(defn third [x] (nth x 2))

(defn strings-to-ints [s]
    (map #(Integer. %) s))

(defn parse-dimensions [i]
    (map (fn [l] (strings-to-ints (str/split (str/trim l) #"\s+")))
      i))

(defn rotate-rows [rs]
  [(map first rs) (map second rs) (map third rs)])

(defn sorted-vertical-triangles [r]
  (map sort (mapcat identity (map rotate-rows (partition 3 r)))))

(defn answer-part-two []
  (count (possible-triangles
          (sorted-vertical-triangles
            (parse-dimensions inp)))))
