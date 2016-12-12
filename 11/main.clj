(ns eddpod.day11
  (:require [clojure.string :as str] [clojure.set :as set]))

; This is my abortive brute-force search of the problem
; space. After some bashing my head against this -- and a solution
; that works for the input example -- it becomes plain that this is
; a problem that has a mathematically derived answer. It would take too
; long to search the problem space for the answers otherwise: or I
; would have to abandon depth-first search and try other search
; strategies, in the end taking too much time up.
;
; so I guess I failed to write code for this, but got the mathy answer


(defn chips [f] (set (flatten (map drop-last (filter #(= (last %) \M) f)))))

(defn gens [f] (set (flatten (map drop-last (filter #(= (last %) \G) f)))))

(def init-state {0 #{"PM" "PG"}
                 1 #{"CG" "UG" "RG" "NG"}
                 2 #{"CM" "UM" "RM" "NM"}
                 3 #{}})


(def example-state {0 #{"HM" "LM"} 1 #{"HG"} 2 #{"LG"} 3 #{}})

(defn seen-before? [h s] (some #{s} h))
(defn record-state [h s]
  (if (seen-before? h s) h (conj h s)))

(defn grumble [s] (spit "/tmp/clog.txt" (str/join (concat (apply str s) "\n")) :append true))

(defn legal-floor? [f]
  (let [g (gens f)] (or (empty? g) (empty? (set/difference (chips f) g)))))

(defn legal-state? [h s]
  (and
    (every? identity (map legal-floor? (map s (range 4))))
    (not (seen-before? h s))))

(defn winning-state? [s]
  (every? empty? (map s (range 3))))

(defn move [s [items from to]]
  (assoc s from (set/difference (s from) items)
    to (set/union (s to) items)))

(defn possible-pairs [f]
  (set (for [x f y f] (set/union #{x} #{y}))))

(defn possible-ups [s n]
  (if (= n 3) #{} (possible-pairs (s n))))

(defn possible-downs [s n]
  (if (= n 0) #{} (possible-pairs (s n))))

(defn legal-ups [s n h]
  (let [l (possible-ups s n) to (+ n 1)]
    (filter #(legal-state? h (move s [% n to])) l)))

(defn legal-downs [s n h]
  (let [l (possible-downs s n) to (- n 1)]
    (filter #(legal-state? h (move s [% n to])) l)))

(defn moves-from-here [s f h]
  (concat
   (mapcat #(list [% f (+ f 1)]) (legal-ups s f h))
   (mapcat #(list [% f (- f 1)]) (legal-downs s f h))))

; if we win, return the state plus moves it took

(defn ergo [s f h m lim]
;  (grumble ["On floor " f " after " (count m) " moves out of " lim])
;  (grumble ["History " h])
  (if (> (count m) lim)
    (do
;      (grumble ["bailing after " lim " moves"])
      [s nil lim])
    (if (winning-state? s)
      (do (grumble [">>>> found a winner in " (count m)])
        [s m (min lim (count m))])
      (loop [mm (moves-from-here s f h) acc [] hs h l lim]
;        (grumble ["trying " (first mm)])
        (if (empty? mm)
          (do
;           (grumble ["dead end"])
           [s nil l])
          (let [hp (record-state hs s)
                [end-state res nl]
                (ergo
                  (move s (first mm))
                  (last (first mm))
                  hp
                  (conj m (first mm))
                  l)
                nh (record-state hp end-state)]
            (recur (rest mm) (concat res acc) nh nl)))))))
;            (recur (moves-from-here s f nh) (concat res acc) nh nl)))))))

(defn example-problem [] (last (ergo example-state 0 [] [] 500)))
