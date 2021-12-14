(ns aoc-2021-in-clj.day09
  (:require [clojure.test :refer :all]
            [aoc-2021-in-clj.util :refer :all]))

(defn neighbours [[x y]]
  (for [dx [-1 0 1] dy (if (zero? dx) [-1 1] [0])]
    [(+ dx x) (+ dy y)]))

(defn get-height [lines]
  (fn [[x y]]
    (Character/getNumericValue
      (get-in lines [y x]))))

(defn dimensions [lines]
  {:width  (count (first lines))
   :height (count lines)})

;; a board has lines, dimensions
;; get-height
;; on-board

(defn find-lowpoints [file-path]
  (let [lines      (parse-lines file-path)
        width      (count (first lines))
        height     (count lines)
        all-coords (for [x (range width) y (range height)]
                     [x y])
        get-height (fn [[x y]]
                     (Character/getNumericValue
                       (get-in lines [y x])))
        on-board   (fn [[x y]]
                     (and (< -1 x width)
                          (< -1 y height)))]

    (filter
      (fn [xy]
        (every? (partial < (get-height xy)) (map get-height (filter
                                                              on-board
                                                              (neighbours xy)))))

      all-coords)))

(defn part-1 [file-path]
  (apply + (map inc (map (get-height (parse-lines file-path)) (find-lowpoints file-path)))))

(deftest test-day09-part1
  (is (= 4 (count (find-lowpoints "resources/Day09_test.txt"))))
  (is (= 15 (part-1 "resources/Day09_test.txt")))
  (is (= 548 (part-1 "resources/Day09.txt")))
  )

(defn part-2 [path]
  (let [lows       (find-lowpoints path)
        dims       (dimensions (parse-lines path))
        on-board   (fn [[x y]]
                     (and (< -1 x (:width dims))
                          (< -1 y (:height dims))))
        height-fn  (get-height (parse-lines path))
        basin-size (fn [xy]
                     (loop [visited  #{}
                            to-visit [xy]]
                       (if (seq to-visit)
                         (let [[cell & to-visit] to-visit
                               new-cells
                               (filter
                                 (fn [xy]
                                   (and
                                     ((complement visited) xy)
                                     (on-board xy)
                                     (< (height-fn xy) 9)))
                                 (neighbours cell))]
                           (recur (conj visited cell) (into to-visit new-cells)))
                         visited)))]
    (apply * (take 3 (sort > (map (comp count basin-size) lows))))))

(deftest test-part-2

  (is (= 1134 (part-2 "resources/Day09_test.txt")))
  (is (= 786048 (part-2 "resources/Day09.txt"))))

(comment

  (find-lowpoints "resources/Day09_test.txt")

  )

