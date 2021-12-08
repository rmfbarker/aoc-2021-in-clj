(ns aoc-2021-in-clj.day3-test
  (:require [clojure.test :refer :all])
  (:require [aoc-2021-in-clj.day3 :refer [determine-rating part2 part-1]]))

(deftest check-part1
  (is (= 198 (part-1 (slurp "resources/Day03_test.txt"))))
  (is (= 2035764 (part-1 (slurp "resources/Day03.txt"))))
  )

(deftest check-part2
  (is (= 230 (part2 "resources/Day03_test.txt")))
  (is (= 2817661 (part2 "resources/Day03.txt")))
  )

