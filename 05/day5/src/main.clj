(ns eddpod.day5
  (:require [clojure.string :as str] [digest]))

(def inp "cxdnnyjw")

(defn hsh [n] (digest/md5 (str inp n)))

(defn valid-hashes [] (filter #(str/starts-with?  % "00000") (map hsh (range))))

(defn find-code []
  (str/join (map #(nth % 5) (take 8 (valid-hashes)))))

; part 2

(defn position [s]
  (if-some [c (some #{(nth s 5)} [\0 \1 \2 \3 \4 \5 \6 \7])] (Integer. (str c))))

(defn valid-pos-hashes []
  (filter #(some? (position %)) (valid-hashes)))

(defn find-code2 []
  (loop [cmap {} vh (valid-pos-hashes)]
    (if (= 8 (count cmap)) (str/join (map cmap (range 8)))
      (let [h (first vh) p (position h) k (nth h 6)]
        (recur (if-not (cmap p) (assoc cmap p k) cmap) (rest vh))))))
