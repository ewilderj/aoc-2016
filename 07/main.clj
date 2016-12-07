(ns eddpod.day7
  (:require [clojure.string :as str]))

(def inp (str/split-lines (slurp "/Users/edd/git/aoc-2016/07/puzzle.txt")))

; not really proud of these solutions in that they're
; inefficient, but the data set isn't large enough to
; worry about it

(defn pieces [l]
  (let [m (str/split l #"[\[\]]")]
    [(take-nth 2 m) (take-nth 2 (rest m))]))

(defn make-abba [x]
  (if (= (first x) (second x)) nil
    (str/join [(first x) (second x) (second x) (first x)])))

(defn has-abba? [s]
  (loop [s s found false]
    (if (or found (< (count s) 4)) found
      (let [a (make-abba s)]
        (recur (str/join (rest s))
               (if a (str/includes? s a) false))))))

(defn tls-support? [l]
  (let [[a-c a-h] (map (partial map has-abba?) (pieces l))]
    (and (some #{true} a-c) (not (some #{true} a-h)))))

(defn part-one [] (count (filter tls-support? inp)))

; part 2

(defn make-aba-bab [x]
  (if (= (first x) (second x)) nil
    [(str/join [(first x) (second x) (first x)])
     (str/join [(second x) (first x) (second x)])]))

(defn find-abas-bab [s]
  (loop [s s found []]
    (if (< (count s) 3) found
      (let [[a b] (make-aba-bab s)]
        (recur (str/join (rest s))
               (if (and a (str/includes? s a)) (conj found b) found))))))

(defn ssl-support? [l]
  (let [[a h] (pieces l)
          babs (flatten (map find-abas-bab a))
          hbs (flatten (for [b babs] (map #(str/includes? % b) h)))]
    (some #{true} hbs)))

(defn part-two [] (count (filter ssl-support? inp)))
