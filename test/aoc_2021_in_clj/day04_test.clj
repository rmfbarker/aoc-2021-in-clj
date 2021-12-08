(ns aoc-2021-in-clj.day04-test
  (:require [clojure.test :refer :all])
  (:require [aoc-2021-in-clj.day04 :refer :all]))

(deftest check-part1-test-board3
  (let [drawn-numbers "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"

        board         "14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7"

        drawn-numbers (parse-numbers drawn-numbers)
        rows-and-cols (parse-rows-cols (clojure.string/split-lines board))
        rows (parse-board board)
        bingo         (bingo-idx drawn-numbers rows-and-cols)]
    (println "bingo" bingo)
    (println "number" (nth drawn-numbers bingo)))

  )

#_(deftest check-part2
    (is (= 230 (part2 "resources/Day03_test.txt")))
    (is (= 2817661 (part2 "resources/Day03.txt")))
    )

