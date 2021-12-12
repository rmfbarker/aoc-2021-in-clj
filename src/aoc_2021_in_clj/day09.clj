(ns aoc-2021-in-clj.day09
  (:require [clojure.test :refer :all]
            [aoc-2021-in-clj.core :refer [read-input parse-int]]))

(defn neighbours [[x y]]
  (for [dx [-1 0 1] dy (if (zero? dx) [-1 1] [0])]
    [(+ dx x) (+ dy y)]))

(defn find-lowpoints [file-path]
  (let [lines      (clojure.string/split-lines (slurp file-path))
        width      (count (first lines))
        all-coords
                   (for [x (range width) y (range (count lines))]
                     [x y])
        get-height (fn [[x y]]
                     (Integer/parseInt (str (nth (nth lines y) x))))]

    (filter
      (fn [[height neighouring-heights]]
        (every? #(< height %) neighouring-heights)
        )

      (map
        (fn [coord]
          [(get-height coord) (map get-height
                                   (filter
                                     (fn [[x y]] (and (< -1 x width)
                                                      (< -1 y (count lines))))
                                     (neighbours coord)))])
        all-coords
        ))))

(defn part-1 [file-path]
  (apply + (map (comp inc first) (find-lowpoints file-path)))
  )

(deftest test-day09-part1
  (is (= 4 (count (find-lowpoints "resources/Day09_test.txt"))))
  (is (= 15 (part-1 "resources/Day09_test.txt")))
  (is (= 548 (part-1 "resources/Day09.txt")))
  )

(comment

  (find-lowpoints "resources/Day09_test.txt")

  )

