(ns aoc-2021-in-clj.day05
  (:require [clojure.test :refer :all]
            [aoc-2021-in-clj.core :refer [read-input parse-int]]))

(defn start-end [vent-line]
  (map #(mapv parse-int (-> %
                            (clojure.string/trim)
                            (clojure.string/split #",")
                            ))
       (clojure.string/split vent-line #"->")))


(defn parse-vent [s]
  (let [[[x1 y1] [x2 y2]] (start-end s)]
    (cond
      (= x1 x2)
      (for [i (range (min y1 y2) (inc (max y1 y2)))]
        [x1 i])

      (= y1 y2)
      (for [i (range (min x1 x2) (inc (max x1 x2)))]
        [i y1]))))

(defn part-1 [file-path]
  (let [coords (apply concat (map parse-vent (clojure.string/split-lines (slurp file-path))))
        coord-counts
               (reduce
                 (fn [attr coord]
                   (update attr coord (fnil inc 0)))
                 {}
                 coords)]

    (count (filter
             (fn [[k v]] (< 1 v))
             coord-counts))
    ))

(deftest day05-tests

  (is (= [[1 1] [1 3]]
         (start-end "1,1 -> 1,3")
         ))

  (is (= [[1, 1], [1, 2], [1, 3]]
         (parse-vent "1,1 -> 1,3")
         ))

  (is (= [[1, 1], [1, 2], [1, 3]]
         (parse-vent "1,3 -> 1,1")
         ))

  (is (= (sort [[9, 7], [8, 7], [7, 7]])
         (parse-vent "9,7 -> 7,7")
         ))

  (is (= 5 (part-1 "resources/Day05_test.txt")))
  (is (= 5774 (part-1 "resources/Day05.txt")))
  )

(defn parse-numbers [number-data]
  (clojure.string/split number-data #","))

;(defn part-1 [file-path]
;  (let [data (parse-file file-path)
;        [bingo-i board] (winning-board-and-idx data)]
;
;    (calculate-overall-score (:drawn-numbers data) board bingo-i)))
;
;(defn part-2 [file-path]
;  (let [data (parse-file file-path)
;        [bingo-i board] (losing-board-and-idx data)]
;
;    (calculate-overall-score (:drawn-numbers data) board bingo-i)))
;
;(deftest overall-checks
;
;  (is (= 4512 (part-1 "resources/Day04_test.txt")) "test part 1")
;  (is (= 50008 (part-1 "resources/Day04.txt")) "part 1")
;
;  (is (= 17408 (part-2 "resources/Day04.txt")) "part 2"))
;
