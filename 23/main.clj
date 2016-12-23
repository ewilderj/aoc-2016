(ns eddpod.day23
  (:require [clojure.string :as str]))

(def inp (str/split-lines (slurp "/Users/edd/git/aoc-2016/23/puzzle.txt")))

(def regs {:a 6 :b 0 :c 0 :d 0 :pc 0})

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

(defn interp [r]
  (let [l (nth (r :prog) (r :pc))
        [op a1 a2] (str/split l #"\s+")
        rs (inc_pc r)]
    ; dump registers at branches so we can view the multiplications
    (if (some #{(r :pc)} #{16 18 25})
       (println "** at PC " (r :pc) ":" l "a=" (rs :a) "b=" (rs :b) "c=" (rs :c) "d=" (rs :d)))
    (cond
      (= "inc" op) (c_inc rs (decode a1))
      (= "dec" op) (c_dec rs (decode a1))
      (= "tgl" op) (c_tgl rs (decode a1))
      (= "cpy" op) (c_cpy rs (decode a1) (decode a2))
      (= "jnz" op) (c_jnz rs (decode a1) (decode a2)))))

(defn gogo [regs]
  (let [maxpc (count inp)]
    (loop [rs (assoc regs :prog (vec inp)) n 0]
      (if (or (>= n 8000000) (>= (rs :pc) maxpc)) [n rs]
        (recur (interp rs) (inc n))))))

(defn part-one [] ((second (gogo (assoc regs :a 7)) :a)))
; answer is 12703, which turns out to be a prime. the 1517th one

; 5 ->
; 6 ->   8383 ~ e^9.03
; 7 ->  12703 ~ e^9.45
; 8 ->  47983 ~ e^10.78
; 9 -> 370543 ~ e^12.82


; snapshot of the prog at iteration 800000
;  0 cpy a b             12 -> b
;  1 dec b               11 -> b
;  2 cpy a d             12 -> d
;  3 cpy 0 a             0 -> a
;  4 cpy b c             11 -> c
;  5 inc a               1 -> a
;  6 dec c               10 ->c
;  7 jnz c -2            .... o-> c 11 -> a
;  8 dec d               11 -> d
;  9 jnz d -5
; 10 dec b
; 11 cpy b c
; 12 cpy c d
; 13 dec d
; 14 inc c
; 15 jnz d -2
; 16 tgl c
; 17 cpy -16 c
; 18 jnz 1 c    ; n * (n-1) * (n-2) ... = n! when it gets here
; 19 cpy 97 c
; 20 jnz 79 d   ; turns into cpy 79 d
; 21 inc a
; 22 inc d
; 23 jnz d -2
; 24 inc c      : + 79
; 25 jnz c -5
;
; n! + 79  * 97 (7663)
; formula yields 479009263 for 12
