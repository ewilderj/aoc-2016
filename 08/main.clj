(ns eddpod.day8
  (:require [clojure.string :as str]))

(def inp (str/split-lines (slurp "/Users/edd/git/aoc-2016/08/puzzle.txt")))

(defn board [w h]
  (apply assoc {} (interleave (for [x (range w) y (range h)] [x y]) (repeat 0))))

(defn rect [w h b]
  (apply assoc b (interleave (for [x (range w) y (range h)] [x y]) (repeat 1))))

(defn right [l] (cons (last l) (drop-last l)))

(defn rotate [which n tms b]
  (let [ks (sort (filter #(= (which %) n) (keys b)))
        rvs (nth (iterate right (map b ks)) tms)]
    (apply assoc b (interleave ks rvs))))

(defn move [l b]
  (let [w (str/split l #"\s+") c (first w) a (rest w)]
    (cond
      (= "rect" c)
      (apply rect (concat (map read-string (str/split (first a) #"x")) [b]))
      (= "rotate" c)
      (let [rc (if (= "row" (first a)) second first)
            n (read-string (second (str/split (second a) #"=")))
            tms (read-string (nth a 3))]
        (rotate rc n tms b)))))

(defn compute-board []
  (loop [i inp b (board 50 6)]
    (if (empty? i) b (recur (rest i) (move (first i) b)))))

(defn draw-board [b]
  (for [y (range 6)]
       (str/join (map str (for [x (range 50)]
                            (if (= 0 (b [x y])) " " "X"))))))

(defn part-one []
  (((comp frequencies vals) (compute-board)) 1))

(defn part-two []
  (println (str/join "\n" (draw-board (compute-board)))))
