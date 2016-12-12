(ns eddpod.day12
  (:require [clojure.string :as str]))

(def inp (str/split-lines (slurp "/Users/edd/git/aoc-2016/12/puzzle.txt")))

(def regs {:a 0 :b 0 :c 0 :d 0 :pc 0})

(defn vorn [r v] (if (keyword? v) (r v) v))
(defn inc_pc [r] (assoc r :pc (inc (r :pc))))
(defn decode [s] (if (some #{s} ["a" "b" "c" "d"]) (keyword s) (Integer. s)))

(defn c_inc [r w] (assoc r w (inc (r w))))
(defn c_dec [r w] (assoc r w (dec (r w))))
(defn c_cpy [r v w] (assoc r w (vorn r v)))
(defn c_jnz [r w i] (if (= (vorn r w) 0) r (assoc r :pc (+ (r :pc) i -1))))


(defn interp [r l]
  (let [[op a1 a2] (str/split l #"\s+") rs (inc_pc r)]
    (cond
      (= "inc" op) (c_inc rs (decode a1))
      (= "dec" op) (c_dec rs (decode a1))
      (= "cpy" op) (c_cpy rs (decode a1) (decode a2))
      (= "jnz" op) (c_jnz rs (decode a1) (decode a2)))))

(defn gogo [regs]
  (let [maxpc (count inp)]
    (loop [rs regs]
      (if (>= (rs :pc) maxpc) rs
        (recur (interp rs (nth inp (rs :pc))))))))

(defn part-one [] ((gogo regs) :a))
(defn part-two [] ((gogo (assoc regs :c 1)) :a))
