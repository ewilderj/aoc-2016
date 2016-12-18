(ns day17.core
  (:require [clojure.string :as str] [clojure.set :as set] [digest]))

(def max-x 3)
(def max-y 3)
(def seed "qtetzkpl")

(defn door-open? [c] (some #{c} "bcdef"))

(defn open-doors [s]
  (->> s (map door-open?)
    (interleave "UDLR")
    (partition 2)
    (filter second)
    (map first)))

(defn code [p] (take 4 (digest/md5 (apply str seed p))))

(def delta {\U [0 -1] \D [0 1] \L [-1 0] \R [1 0]})

(defn move [[x y] d]
  (let [[dx dy] (delta d)
        nx (+ x dx) ny (+ y dy)] [nx ny]))

(defn trim-directions [[x y] ds]
  (filter
    (fn [d]
      (let [[nx ny] (move [x y] d)]
        (and (>= nx 0) (<= nx max-x)
             (>= ny 0) (<= ny max-y))))
    ds))

(defn directions [[x y] p]
  (trim-directions [x y] (open-doors (code p))))

; score is made from Manhattan distance and # closed doors
; if it's a dead-end, that's a real bad score, if it's the
; target we'll give a perfect score, regardless of dead-ends
(defn estimate [[x y] p]
  (let [n (count (directions [x y] p))
        m (+ (Math/abs (- x max-x)) (Math/abs (- y max-y)))]
    (cond
      (= [max-x max-y] [x y]) 0
      (= n 0) 1000
      :else (+ (* 2 (- 4 n)) (* 1 m)))))

(defn best-choice [s f] (first (sort-by f s)))

(defn candidate-moves [[x y] p]
  (set
   (map (fn [d]
          (let [[nx ny] (move [x y] d)
                np (str/join [p (str d)])]
            [[nx ny] np])) (directions [x y] p))))

(defn dist-between [[[x y] p] [[gx gy] gp]]
    (- (estimate [x y] p) (estimate [gx gy] gp)))

(defn tent-gscore [cur mov gscore]
  (+ (gscore cur) 1))

(defn new-fscore [[[nx ny] np] gscore]
  (+ (gscore [[nx ny] np]) (estimate [nx ny] np)))

; a* search, see 'estimate' for heuristic
(defn solve [tx ty]
  (loop [closed #{} open #{[[tx ty] ""]}
         gscore {[[tx ty] ""] 0}
         fscore {[[tx ty] ""] (estimate [tx ty] "")}
         n 0]
    (if (or (> n 200) (empty? open))
      ; n is a safety valve for when i screw up and infinitely recurse
     (println "FAILED IN " (str n))
     (let [cur (best-choice open fscore) [[x y] p] cur]
       ; (println "\ncur" cur)
       (if (= [x y] [max-x max-y])
         p
         (let [nc (conj closed cur)
               cn (set/difference (candidate-moves [x y] p) closed)
               no (set/union cn (remove #{cur} open))
               tgs (into {} (map #(vec [% (tent-gscore cur % gscore)]) cn))
               np (set (filter #(or (not (gscore %)) (< (tgs %) (gscore %))) cn))
               ng (merge gscore (select-keys tgs np))
               nf (merge fscore (into {} (map #(vec [% (new-fscore % ng)]) np)))]

          (recur nc no  ng nf (inc n))))))))

(defn part-one [] (solve 0 0))

; part 2 is going to need breadth-first and just record every soln

(defn solve2 [tx ty]
  (loop [queue [[[tx ty] ""]]
         paths [] seen #{}]
    (if (empty? queue)
      paths
      (let [cur (first queue)
            [[x y] p] cur
            goal (and (= x max-x) (= y max-y))
            cn (if goal [] (filter #(not (seen %)) (candidate-moves [x y] p)))
            nseen (set/union seen (set cn))
            nq (concat (rest queue) cn)]
        (recur nq (if goal (conj paths p) paths) nseen)))))

(defn part-two [] (last (sort (map count (solve2 0 0)))))
