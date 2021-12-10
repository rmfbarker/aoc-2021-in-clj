(ns aoc-2021-in-clj.day07
  (:require [clojure.test :refer :all]
            [aoc-2021-in-clj.core :refer [read-input parse-int]]))

(defn parse-crabs [s] (map parse-int (clojure.string/split s #",")))

(defn simple-cost [a b] (Math/abs (- a b)))

(defn sum-of-dis [cost-fn crabs pos]
  (apply + (map
             (partial cost-fn pos)
             crabs)))

(defn part-1 [crabs]
  (let [crabs (sort crabs)]
    (first (sort (map
                   #(sum-of-dis simple-cost crabs %)
                   (range (first crabs) (inc (last crabs))))))))

(defn expensive-cost [a b]
  (let [target (max a b)]
    (loop [pos (min a b) cost 1 total 0]
      (if (= target pos)
        total
        (recur (inc pos) (inc cost) (+ total cost))))))

(defn part-2 [crabs]
  (let [crabs (sort crabs)]
    (first (sort (map
                   #(sum-of-dis expensive-cost crabs %)
                   (range (first crabs) (inc (last crabs))))))))

(deftest test-day07

  (let [crabs (parse-crabs "16,1,2,0,4,2,7,1,2,14")]
    (is (= 37 (sum-of-dis simple-cost crabs 2)))
    (is (= 37 (part-1 crabs)))
    (is (= 37 (part-1 (parse-crabs (slurp "resources/Day07_test.txt")))))
    (is (= 349769 (part-1 (parse-crabs (slurp "resources/Day07.txt")))))

    (is (= 66 (expensive-cost 5 16)))
    (is (= 66 (expensive-cost 16 5)))
    (is (= 45 (expensive-cost 14 5)))
    (is (= 168 (part-2 crabs)))
    (is (= 99540554 (part-2 (parse-crabs (slurp "resources/Day07.txt")))))))

