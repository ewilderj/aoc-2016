(ns eddpod.day9
  (:require [clojure.string :as str]))

(def inp (slurp "/Users/edd/git/aoc-2016/09/puzzle.txt"))

(defn next-chunk [s]
  (rest (first (re-seq #"^(\w*)\((\d+)x(\d+)\)(.*)$" s))))

(defn expand [s]
  (let [m (next-chunk s)]
    (if (empty? m) [s "" ""]
      (let [p (first m) n (read-string (second m))
            o (read-string (nth m 2))
            q (str/join (flatten (repeat o (take n (last m)))))
            r (str/join (drop n (last m)))]
        [p q r]))))

(defn expand-code [s]
  (loop [r "" s s]
    (if (= s "") r (let [[d e f] (expand s)] (recur (str/join [r d e]) f)))))

(defn part-one [] (count (expand-code inp)))

; part 2

(defn deep-expand [s]
  (let [m (next-chunk s)]
    (if (empty? m) (count s)
      (let [p (count (first m))
            n (read-string (second m))
            o (read-string (nth m 2))
            q (str/join (take n (last m)))
            r (str/join (drop n (last m)))]
        (+ p (* o (deep-expand q)) (deep-expand r))))))

(defn part-two [] (deep-expand inp))
