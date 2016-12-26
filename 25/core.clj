(ns eddpod.day25
  (:require [clojure.string :as str]))

(def inp (str/split-lines (slurp "/Users/edd/git/aoc-2016/25/puzzle.txt")))

(def regs {:a 0 :b 0 :c 0 :d 0 :pc 0 :buf []})

(defn vorn [r v] (if (keyword? v) (r v) v))
(defn inc_pc [r] (assoc r :pc (inc (r :pc))))
(defn decode [s] (if (some #{s} ["a" "b" "c" "d"]) (keyword s) (Integer. s)))

(defn c_inc [r w] (assoc r w (inc (r w))))
(defn c_dec [r w] (assoc r w (dec (r w))))
(defn c_cpy [r v w] (if (keyword? w) (assoc r w (vorn r v)) r))
(defn c_jnz [r w i] (if (= (vorn r w) 0) r (assoc r :pc (+ (r :pc) (vorn r i) -1))))

(defn toggle [l]
  (let [[op & args] (str/split l #"\s+")]
    (apply str
      (if (= 1 (count args))
        (if (= op "inc") "dec" "inc")
        (if (= op "jnz") "cpy" "jnz"))
      " " (str/join " " args))))

(defn c_tgl [r w]
  (let [n (+ (r :pc ) (vorn r w) -1)]
    (if (>= n (count (r :prog))) r
      (let [i (get-in r [:prog n])
            j (toggle i)]
        (assoc r :prog (assoc (r :prog) n j))))))

(defn c_out [r w]
  (assoc r :buf (conj (r :buf) (vorn r w))))

(defn interp [r]
  (let [l (nth (r :prog) (r :pc))
        [op a1 a2] (str/split l #"\s+")
        rs (inc_pc r)]

    (cond
      (= "inc" op) (c_inc rs (decode a1))
      (= "out" op) (c_out rs (decode a1))
      (= "dec" op) (c_dec rs (decode a1))
      (= "tgl" op) (c_tgl rs (decode a1))
      (= "cpy" op) (c_cpy rs (decode a1) (decode a2))
      (= "jnz" op) (c_jnz rs (decode a1) (decode a2)))))

(defn gogo [regs m]
  (let [maxpc (count inp)]
    (loop [rs (assoc regs :prog (vec inp)) n 0]
      (if (or (>= n m) (>= (rs :pc) maxpc)) [n rs]
        (recur (interp rs) (inc n))))))

(defn part-one []
  (loop [a 0]
    (let [b (take 10 ((second (gogo (assoc regs :a a) 40000)) :buf))]
      (if (= [0 1 0 1 0 1 0 1 0 1] b) a
        (recur (inc a))))))
