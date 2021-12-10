(ns aoc-2021-in-clj.day07
  (:require [clojure.test :refer :all]
            [aoc-2021-in-clj.core :refer [read-input parse-int]]))

(defn parse-crabs [s] (map parse-int (clojure.string/split s #",")))

(defn sum-of-dis [crabs pos]
  (apply + (map
             (fn [crab] (Math/abs (- crab pos)))
             crabs)))

(deftest test-day06

  (let [crabs (parse-crabs "16,1,2,0,4,2,7,1,2,14")]
    (is (= 37 (sum-of-dis crabs 2)))
    )

  ;(is (= 362666 (part-1 (clojure.string/trim (slurp "resources/Day06.txt"))
  ;                      80)))
  ;
  ;(is (= 1640526601595 (part-1 (clojure.string/trim (slurp "resources/Day06.txt"))
  ;                             256)))
  )

