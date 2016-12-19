(ns day16.core
  (:require [clojure.string :as str]))

(defn swap [x] (str/join (map #(if (= \0 %) \1 \0) x)))
(defn dragon [x] (str/join [x "0" (swap (reverse x))]))
(defn dragons [x n]
  (str/join (take n (loop [out x]
                      (if (>= (count out) n) out
                        (recur (dragon out)))))))
(defn parity [x] (str/join (map #(if (apply = %) \1 \0) (partition 2 x))))
(defn checksum [x] (loop [c (parity x)] (if (odd? (count c)) c (recur (parity c)))))
(defn example-q [] (checksum (dragons "10000" 20)))
(defn part-one [] (checksum (dragons "01110110101001000" 272)))
(defn part-two [] (checksum (dragons "01110110101001000" 35651584)))
