(ns day21.core
  (:require [clojure.string :as str]))

(def inp (str/split-lines (slurp "/Users/edd/git/aoc-2016/21/puzzle.txt")))

(defn sw [s m n] (let [v (vec s)] (assoc v m (v n) n (v m))))
(defn swl [s a b] (replace {a b b a} s))
(defn rotl [s n] (let [m (mod n (count s))] (concat (drop m s) (take m s))))
(defn rotr [s n] (let [m (mod n (count s))] (concat (take-last m s) (drop-last m s))))

(defn rpos [s x]
  (let [n (str/index-of s x)]
    (if (nil? n) s
      (let [m (if (>= n 4) (+ 2 n) (inc n))]
        (rotr s m)))))

(defn rev [s a b]
  (let [v (vec s)]
    (concat (subvec v 0 a)
            (reverse (subvec v a (inc b)))
            (subvec v (inc b)))))

(defn mov [s m n]
  (let [v (vec s)
        w (vec (concat (subvec v 0 m) (subvec v (inc m))))]
      (concat (subvec w 0 n) (subvec v m (inc m)) (subvec w n))))

(defn interpret [l s]
  (let [[c & a] (str/split l #"\s+")]
    (apply str
      (cond
        (= c "rotate") (cond (= (first a) "right") (rotr s (Integer/parseInt (second a)))
                        (= (first a) "left") (rotl s (Integer/parseInt (second a)))
                        (= (first a) "based") (rpos s (first (last a))))
        (= c "move") (mov s (Integer/parseInt (second a)) (Integer/parseInt (last a)))
        (= c "swap") (cond (= (first a) "position") (sw s (Integer/parseInt (second a)) (Integer/parseInt (last a)))
                      (= (first a) "letter") (swl s (first (second a)) (first (last a))))
        (= c "reverse") (rev s (Integer/parseInt (second a)) (Integer/parseInt (last a)))
        :else (println "ERROR on " c a)))))

(defn part-one []
  (loop [prog inp s "abcdefgh"]
    (if (empty? prog) s (recur (rest prog) (interpret (first prog) s)))))

; only works for 8 char passwords, lookup table to put rpos back
(def rposlk {2 6 4 7 6 0 0 1 1 1 3 2 5 3 7 4})
(defn rposr [s x]
  (let [n (str/index-of s x)]
    (if (nil? n) s
      (let [m (rposlk n)]
        (rotl s m)))))

(defn interpret-r [l s]
  (let [[c & a] (str/split l #"\s+")]
    (apply str
      (cond
        (= c "rotate") (cond (= (first a) "right") (rotl s (Integer/parseInt (second a)))
                        (= (first a) "left") (rotr s (Integer/parseInt (second a)))
                        (= (first a) "based") (rposr s (first (last a))))
        (= c "move") (mov s (Integer/parseInt (last a)) (Integer/parseInt (second a)))
        (= c "swap") (cond (= (first a) "position") (sw s (Integer/parseInt (second a)) (Integer/parseInt (last a)))
                      (= (first a) "letter") (swl s (first (second a)) (first (last a))))
        (= c "reverse") (rev s (Integer/parseInt (second a)) (Integer/parseInt (last a)))
        :else (println "ERROR on " c a)))))


(defn part-two []
  (loop [prog (reverse inp) s "fbgdceah"]
    (if (empty? prog) s (recur (rest prog) (interpret-r (first prog) s)))))
