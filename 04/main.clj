(ns eddpod.day4
  (:require [clojure.string :as str]))

(def inp (str/split-lines (slurp "/Users/edd/git/adventofcode/04/puzzle.txt")))

(defn make-checksum [letters]
  (let [f (frequencies letters) u (keys f)]
    (str/join (take 5 (sort (fn [a b] (> (f a) (f b))) (sort u))))))

(defn id-if-valid [l]
  (let [b (str/split l #"-")
      [id checksum] (rest (flatten (re-seq #"(\d+)\[(\w+)\]" (last b))))
      letters (vec (str/join (drop-last b)))
      real-checksum (make-checksum letters)
      ] (if (= checksum real-checksum) (Integer. id) 0)))

(defn answer-part-one []
    (reduce + (map id-if-valid inp)))

; part 2

(defn rotate-char [n c]
  (if (= c \-) (if (odd? n) \space \-)
    (char (+ 97 (mod (+ n (- (int c) 97)) 26)))))

(defn valid-rooms []
  (let [k (filter #(> (second %) 0)
    (apply assoc {} (interleave inp (map id-if-valid inp))))] k))

(defn decoded-rooms []
  (for [[code id] (valid-rooms)]
    [(str/join (map #(rotate-char id %) code)) id]))

(defn answer-part-two []
  (second (first (filter #(str/includes? (first %) "northpole") (decoded-rooms)))))
