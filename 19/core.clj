(ns day19.core
  (:require [clojure.string :as str]))

(defn del-k [e k]
  (if (= k (count e)) (rest e)
    (keep-indexed #(if (not (= %1 k)) %2) e)))

(defn slow-part-one [elves]
  (loop [e (vec (range elves)) k 1]
    (if (= 1 (count e)) (inc (first e))
      (recur (del-k e k) (mod (inc k) (dec (count e)))))))

; drop MSB, shift-left one, add one.
(defn part-one [elves]
  (inc (* 2 (read-string (apply str "2r" (apply str (rest (Integer/toBinaryString elves))))))))


; part 2 still unsolved
(defn choose-vic [n k] k
  (mod (int (+ (/ n 2) k)) n))

(defn slow-part-two [elves]
  (loop [e (vec (range elves)) k 0 n 0]
;    (println k e n)
    (if (= 1 (count e)) (first e)
      (let [v (choose-vic (count e) k)
            nk (if (>= (inc k) (dec (count e))) 0 (inc k))]
        (recur (del-k e v) nk (inc n))))))

(defn pad [n]
  (apply str (take-last 8 (str/join ["0000000" (Integer/toBinaryString n)]))))

(defn pattern []
  (for [n (rest (range 129))]
    (let [m (slow-part-two n)]
      (apply str [n " -> " m ": " (pad n) " -> " (pad m) " - "
                ;  (- n m)
                  "\n"]))))
