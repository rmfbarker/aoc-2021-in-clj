(ns aoc-2021-in-clj.day04
  (:require [clojure.test :refer :all]
            [aoc-2021-in-clj.core :refer [read-input parse-int]]))

(defn parse-numbers [number-data]
  (clojure.string/split number-data #","))

(defn parse-board [board-data]
  (let
    [;board-data   ["22 13 17 11  0" " 8  2 23  4 24" "21  9 14 16  7" " 6 10  3 18  5" " 1 12 20 15 19"]
     board (map #(clojure.string/split (clojure.string/trim %) #"\s+")
                board-data)]
    board))

(defn parse-rows-cols [board-data]
  "returns array of rows and columns"
  (let
    [;board-data   ["22 13 17 11  0" " 8  2 23  4 24" "21  9 14 16  7" " 6 10  3 18  5" " 1 12 20 15 19"]
     board   (map #(clojure.string/split (clojure.string/trim %) #"\s+") board-data)
     columns (for [i (range (count (first board)))]
               (map #(nth % i) board))]
    (concat board columns)
    ))

(defn remaining-numbers-on-board [used-numbers rows]
  (map parse-int
       (apply concat (map
                       (fn [row]
                         (filter (complement (set used-numbers)) row))
                       rows))))

(defn calculate-score [remaining-numbers bingo-number]
  (let [sum (apply + remaining-numbers)]
    (println "sum" sum "bingo number" bingo-number)
    (* bingo-number sum)))

(defn bingo-idx [drawn-numbers first-board]
  (let [drawn-numbers-by-idx (zipmap drawn-numbers (range))
        rows-sorted          (map
                               #(sort-by drawn-numbers-by-idx %)
                               first-board)]
    drawn-numbers
    ;first-board
    (println drawn-numbers)
    (println rows-sorted)

    "returns the idx when each row is fully matched"
    (first (sort (map
                   (fn [row] (get drawn-numbers-by-idx (last row)))
                   rows-sorted)))
    ))

(deftest part1-test

  (let [drawn-numbers  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"

        board          "14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7"

        drawn-numbers  (parse-numbers drawn-numbers)
        rows-and-cols  (parse-rows-cols (clojure.string/split-lines board))
        bingo-idx      (bingo-idx drawn-numbers rows-and-cols)
        used-numbers   (take (inc bingo-idx) drawn-numbers)
        board          (parse-board (clojure.string/split-lines board))
        remaining-nums (remaining-numbers-on-board used-numbers board)
        bingo-number   (parse-int (nth drawn-numbers bingo-idx))]

    (println "bingo index" bingo-idx)
    (println "number" bingo-number)

    (is (= 11 bingo-idx)
        "bingo number")

    (is (= 188 (apply + remaining-nums))
        "sum of remaining")
    (is (= 4512 (calculate-score remaining-nums bingo-number))
        "score of board")
    )

  )

;(defn parse-boards [lines]
;  (filter #(not= % [""]) (partition-by #(= "" (clojure.string/trim %)) lines)))







