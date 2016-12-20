(ns day19.core
  (:require [clojure.string :as str]))

; (defn del-k [e k]
;   (if (= k (count e)) (rest e)
;     (keep-indexed #(if (not (= %1 k)) %2) e)))

(defn del-k [e k]
  (if (= k (count e)) (rest e)
    (vec (concat (subvec e 0 k) (subvec e (inc k))))))

(defn slow-part-one [elves]
  (loop [e (vec (range elves)) k 1]
    (if (= 1 (count e)) (inc (first e))
      (recur (del-k e k) (mod (inc k) (dec (count e)))))))

; drop MSB, shift-left one, add one.
(defn part-one [elves]
  (inc (* 2 (read-string (apply str "2r" (apply str (rest (Integer/toBinaryString elves))))))))

(defn rotate [l r]
  (let [nl (if (< (count r) (count l)) (drop-last l) l)
        nr (if (< (count r) (count l)) r (rest r))
        ql (conj (vec (rest nl)) (first nr))
        qr (conj (vec (rest nr)) (first nl))]
    [ql qr]))

(defn split-list-two [n]
  (let [m (quot n 2) l (range 0 m) r (range m n)]
    (loop [l l r r]
      (if (or (nil? (first l)) (nil? (first r)))
        [l r]
        (let [[nl nr] (rotate l r)]
          (recur nl nr))))))

; executes in hideous time, use linear method instead
(defn split-part-two [] (inc (first (filter some? (flatten (split-list-two 5))))))

; uses the progression to move from the answer to the next one
(defn linear-part-two [n]
  (loop [p 2 i 4]
    (if (> i n) p
      (let [np (mod (inc p) (dec i))
            qp (if (>= np (quot i 2)) (inc np) np)]
        (recur qp (inc i))))))

(defn part-two [] (inc (linear-part-two 3014603)))
