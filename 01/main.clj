                                        ; http://adventofcode.com/2016/day/1
(def instruc "L4, R2, R4, L5, L3, L1, R4, R5, R1, R3, L3, L2, L2, R5, R1, L1, L2, R2, R2, L5, R5, R5, L2, R1, R2, L2, L4, L1, R5, R2, R1, R1, L2, L3, R2, L5, L186, L5, L3, R3, L5, R4, R2, L5, R1, R4, L1, L3, R3, R1, L1, R4, R2, L1, L4, R5, L1, R50, L4, R3, R78, R4, R2, L4, R3, L4, R4, L1, R5, L4, R1, L2, R3, L2, R5, R5, L4, L1, L2, R185, L5, R2, R1, L3, R4, L5, R2, R4, L3, R4, L2, L5, R1, R2, L2, L1, L2, R2, L2, R1, L5, L3, L4, L3, L4, L2, L5, L5, R2, L3, L4, R4, R4, R5, L4, L2, R4, L5, R3, R1, L1, R3, L2, R2, R1, R5, L4, R5, L3, R2, R3, R1, R4, L4, R1, R3, L5, L1, L3, R2, R1, R4, L4, R3, L3, R3, R2, L3, L3, R4, L2, R4, L3, L4, R5, R1, L1, R5, R3, R1, R3, R4, L1, R4, R3, R1, L5, L5, L4, R4, R3, L2, R1, R5, L3, R4, R5, L4, L5, R2")

(defn get-pairs [i]
  (map (fn [[t d]] (vec [t (Integer. d)]))
       (map rest (re-seq #"([LR])(\d+)" i))))

(def compass [[0 1] [1 0] [0 -1] [-1 0]])

(defn move [x y n [dx dy]]
  [(+ x (* n dx)) (+ y (* n dy))])

(defn rotate-left [d] (mod (dec d) 4))
(defn rotate-right [d] (mod (inc d) 4))

(defn abs "(abs n) is the absolute value of n" [n]
  (cond
   (not (number? n)) (throw (IllegalArgumentException.
                             "abs requires a number"))
   (neg? n) (- n)
   :else n))

(defn aplus [x y] (+ (abs x) (abs y)))

(defn rot [turn d]
  (mod (if (= turn "L") (dec d) (inc d)) 4))

(defn step [[x y d] [turn dist]]
  (let [nd (rot turn d)]
    (conj (move x y dist (get compass nd)) nd)))


(defn distance [inp]
  (reduce aplus (take 2 (reduce step [0 0 0] (get-pairs inp)))))

;; problem part 2

(defn expand-move [[x y] [dx dy] n]
  (for [i (range 1 (inc n))]
    [(+ x (* i dx)) (+ y (* i dy))]))


(defn pace [[trail d] [turn dist]]
  (let [nd (rot turn d)]
    [(concat trail (expand-move (last trail) (get compass nd) dist)) nd]))

(defn follow-trail [ii]
  (loop [traild [[[0 0]] 0] moves ii]
    (if (empty? moves)
      traild
      (recur (pace traild (first moves)) (rest moves)))))


(defn distance-of-hq [inp]
  (let [l (first (follow-trail (get-pairs inp)))
        f (frequencies l)]
    (reduce aplus(first (filter (fn [loc] (> (f loc) 1)) l)))))
