(ns day17.core-test
  (:require [clojure.test :refer :all]
            [day17.core :refer :all]))


(deftest a-test-door
  (testing "Open states."
    (is (every? identity (map door-open? [\b \c \d \e \f]))))
  (testing "Closed states"
    (is (every? identity (map #(not (door-open? %)) [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a])))))

(deftest a-test-open-doors
  (testing "doors are open"
    (is (= #{\U \D \L} (set (open-doors "ced9"))))))

(deftest a-trim-directions-test
  (testing "top-left"
    (is (= #{\D \R} (set (trim-directions [0 0] #{\U \D \L \R})))))
  (testing "top-right"
    (is (= #{\D \L} (set (trim-directions [max-x 0] #{\U \D \L \R})))))
  (testing "bottom-left"
    (is (= #{\U \R} (set (trim-directions [0 max-y] #{\U \D \L \R})))))
  (testing "bottom-right"
    (is (= #{\U \L} (set (trim-directions [max-x max-y] #{\U \D \L \R})))))
  (testing "1 1"
    (is (= #{\U \D \L \R} (set (trim-directions [1 1] #{\U \D \L \R}))))))
