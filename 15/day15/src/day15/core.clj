(ns day15.core
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def discs [[13 1] [19 10] [3 2] [7 1] [5 3] [17 5]])
(defn d-at-t? [[m o] t] (= 0 (mod (+ o t) m)))
(defn win? [ds t]
  (every? identity (map #(d-at-t? (second %) (+ (first %) (inc t))) (partition 2 (interleave (range) ds)))))
(defn find-winner [d]
  (second (first (filter first (partition 2 (interleave (map #(win? d %) (range)) (range)))))))
(defn part-one [] (find-winner discs))
(defn part-two [] (find-winner (conj discs [11 0])))
