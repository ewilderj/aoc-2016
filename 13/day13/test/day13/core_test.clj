(ns day13.core-test
  (:require [clojure.test :refer :all]
            [day13.core :refer :all]))

(deftest bc-zero-test
  (testing "bit-count zero"
    (is (= (bit-count 0) 0))))

(deftest bc-one-test
  (testing "bit-count 64"
    (is (= (bit-count 64) 1))))

(deftest bc-seven-test
  (testing "bit-count 127"
    (is (= (bit-count 127) 7))))

(deftest bc-seven-shifted-test
  (testing "bit-count 127 shifted"
    (is (= (bit-count (* 127 65536) 7)))))

(deftest wall-zero
  (testing "wall 0 0"
    (is (not (is-wall? 0 0 10)))))

(deftest wall-first-row
  (testing "wall first row"
    (is (= (map #(is-wall? % 0 10) (range 10))
           [false true false true true true true false true true]))))


(deftest draw-grid-10-test
  (testing "draw-grid 10 10"
    (is (= (draw-grid 10 10)
           ".#.####.##\n..#..#...#\n#....##...\n###.#.###.\n.##..#..#.\n..##....#.\n#...##.###\n.##..#.##.\n#.###....#\n###.####.#"))))

(deftest bestchoice-choose
  (testing "best choice"
    (is (= [3 4] (best-choice #{[1 2] [3 4]} {[0 0] 34 [1 2] 12 [3 4] 9})))))

(deftest estimate-test
  (testing "estimate"
    (is (= 20
           (estimate -5 -5 -20 -10)
           (estimate 0 0 10 10)
           (estimate 10 10 0 0)
           (estimate 0 0 -10 10)))))

(deftest neighbors-normal
  (testing "neigbors normal"
    (is (= (set [[1 0] [2 1] [1 2] [0 1]])
          (neighbors 1 1)))))

(deftest neighbors-origin
  (testing "neigbors origin"
    (is (= (set [[1 0] [0 1]])
          (neighbors 0 0)))))

(deftest open-neighbors-origin
  (testing "open neighbors origin"
    (is (= #{[0 1]} (open-neighbors 0 0 10)))))

(deftest open-neighbors-1-3
  (testing "open neighbors 1 3"
    (is (= #{[1 2]} (open-neighbors 1 3 10)))))
