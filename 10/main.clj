(ns eddpod.day10
  (:require [clojure.string :as str]))

(def inp (str/split-lines (slurp "/Users/edd/git/aoc-2016/10/puzzle.txt")))

(defn assign [a t n v]
  (if (= t :output)
    (assoc-in a [:output n] (sort (cons v (get-in a [:output n] []))))
    (let [ap (assoc-in a [:bot n] (sort (cons v (get-in a [:bot n] []))))]
      (check-and-exec ap n))))

(defn give-rule [bot [lw ln] [hw hn]]
  (fn [a]
    (let [[l h] (get-in a [:bot bot] [])]
      (assign (assign a lw ln l) hw hn h))))

(defn check-and-exec [a bot]
  (let [vs (get-in a [:bot bot]) r (get-in a [:rules bot])]
    (if (and (= (count vs) 2) (some? r))
      (do (if (= [17 61] vs) (println "bot" bot "has two values" vs))
          (r a))
      a)))

(defn parse-line [a l]
  (let [ws (str/split l #"\s+")]
    (if (= (first ws) "value")
      (let [v (Integer. (second ws)) b (Integer. (nth ws 5))]
        (assign a :bot b v))
      (let [b (Integer. (second ws))
            lw (keyword (nth ws 5)) ln (Integer. (nth ws 6))
            hw (keyword (nth ws 10)) hn (Integer. (nth ws 11))]
        (assoc-in a [:rules b] (give-rule b [lw ln] [hw hn]))))))

(defn make-machine []
  (loop [i (filter #(str/starts-with? % "bot") inp) a {}]
    (if (empty? i) a (recur (rest i) (parse-line a (first i))))))

(defn assign-values [a]
  (loop [i (filter #(str/starts-with? % "value") inp) a a]
    (if (empty? i) a (recur (rest i) (parse-line a (first i))))))

(defn part-one [] (assign-values (make-machine)))

(defn part-two []) (reduce * (flatten (map ((part-one) :output) [0 1 2])))
