(ns day13.core
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn bit-count
  ([v] (bit-count v 0))
  ([v c] (if (zero? v) c (recur (bit-and v (dec v)) (inc c)))))

(defn is-wall? [x y m]
  (odd? (bit-count (+ m (* x x) (* 3 x) (* 2 x y) y (* y y)))))

(defn draw-grid [n m]
  (str/join "\n"
    (for [y (range n)]
      (str/join (map #(if % \# \.) (map #(is-wall? % y m) (range n)))))))

(defn estimate [x y tx ty] (+ (Math/abs (- x tx)) (Math/abs (- y ty))))

(defn best-choice [s f] (first (sort-by f s)))

(defn neighbors [x y]
  (set [[(Math/abs (- x 1)) y] [(+ x 1) y]
        [x (Math/abs (- y 1))] [x (+ y 1)]]))

(defn open-neighbors [x y m]
  (set (filter #(not (is-wall? (first %) (second %) m)) (neighbors x y))))

(defn tent-gscore [[x y] [nx ny] gscore]
  (+ (gscore [x y]) (estimate x y nx ny)))

(defn new-fscore [[x y] [gx gy] gscore]
  (+ (gscore [x y]) (estimate x y gx gy)))

(defn unwind-path [came-from gx gy]
  (loop [p [] cur [gx gy]]
    (let [o (came-from cur)]
      (if (nil? o) p (recur (conj p o) o)))))

(defn solve [tx ty gx gy m]
  (loop [closed #{} open #{[tx ty]} came-from {} gscore {[tx ty] 0}
         fscore {[tx ty] (estimate tx ty gx gy)} n 0]
    (if (or (> n 1000) (empty? open))
      "FAIL"
      (let [cur (best-choice open fscore)]
        (if (= cur [gx gy])
            (do (println "WIN")
                (unwind-path came-from gx gy))
              ; not won yet, now let's dig in
            (let [nc (conj closed cur)
                  [x y] cur
                  cn (set/difference (open-neighbors x y m) closed)
                  no (set/union cn (remove #{cur} open))
                  tgs (into {} (map #(vec [% (tent-gscore cur % gscore)]) cn))
                  np (set (filter #(or (not (gscore %)) (< (tgs %) (gscore %))) cn))
                  ng (merge gscore (select-keys tgs np))
                  nf (merge fscore (into {} (map #(vec [% (new-fscore % [gx gy] ng)]) np)))
                  ncf (merge came-from (into {} (map #(vec [% cur]) np)))]

              ; for each candidate neighbor cn...
              (recur nc no ncf ng nf (inc n))))))))

(def part-one (count (solve 1 1 31 39 1364)))
(def part-two )
