(ns eddpod.day10
  (:require [clojure.string :as str]))

(def inp (str/split-lines (slurp "/Users/edd/git/aoc-2016/10/puzzle.txt")))

(defn assign [a t n v]
  (let [ap (assoc-in a [t n] (sort (cons v (get-in a [t n] []))))]
    (if (= t :bot) (check-and-exec ap n) ap)))

(defn check-and-exec [a bot]
  (let [vs (get-in a [:bot bot]) r (get-in a [:rules bot])]
    (if (and (= (count vs) 2) (some? r))
      (do (if (= [17 61] vs) (println "bot" bot "has two values" vs))
          (r a))
      a)))

(defn give-rule [bot [lw ln] [hw hn]]
  (fn [a] (let [[l h] (get-in a [:bot bot] [])]
            (assign (assign a lw ln l) hw hn h))))

(defn parse-line [a l]
  (let [ws (str/split l #"\s+")]
    (if (= (first ws) "value")
      (let [v (Integer. (second ws)) b (Integer. (nth ws 5))]
        (assign a :bot b v))
      (let [b (Integer. (second ws))
            lw (keyword (nth ws 5)) ln (Integer. (nth ws 6))
            hw (keyword (nth ws 10)) hn (Integer. (nth ws 11))]
        (assoc-in a [:rules b] (give-rule b [lw ln] [hw hn]))))))

(defn parse [i a]
  (loop [i i a a] (if (empty? i) a (recur (rest i) (parse-line a (first i))))))

(defn make-machine [] (parse (filter #(str/starts-with? % "bot") inp) {}))
(defn assign-values [a] (parse (filter #(str/starts-with? % "value") inp) a))

(defn part-one [] (assign-values (make-machine)))
(defn part-two [] (reduce * (flatten (map ((part-one) :output) [0 1 2]))))
