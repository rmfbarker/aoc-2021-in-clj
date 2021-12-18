(ns aoc-2021-in-clj.day11
  (:require [clojure.string :refer [split-lines]]
            [clojure.test :refer :all]
            [aoc-2021-in-clj.util :refer :all]))

;; represent the board as a map of xy co-ords and energy levels {[x y] e}
;; this makes it easy to locate a cell and increase its energy levels

(def input-lines "11111\n19991\n19191\n19991\n11111\n")
(def lines (clojure.string/split-lines input-lines))

(defn parse-board [lines]
  (let [width  (count (first lines))
        height (count lines)]
    (into {}
          (for [x (range width) y (range height)
                :let [energy (Character/getNumericValue
                               (get-in lines [y x]))]]
            [[x y] energy]))))

(defn print-board [board]
  (let [get-x #(ffirst %) get-y #(second (first %))
        rows  (group-by get-y board)]
    (doseq [row (range (count rows))]
      (println (apply str (map (comp #(format "%2d " %) second) (sort-by get-x (filter #(= row (get-y %)) board))))))))

(comment

  (print-board (parse-board lines)))

(defn neighbours [[x y]]
  (for [dx [-1 0 1] dy (if (zero? dx) [-1 1] [-1 0 1])]
    [(+ dx x) (+ dy y)]))

(map neighbours (map first (parse-board lines)))

;; each position is represented by a tuple of an xy co-ord
;; and the energy level e.g. [[x y] e]

(defn inc-energy [board]
  (map #(vector (first %) (inc (second %))) board))

(defn flash-board [board]
  (loop [flashed {} un-flashed board]
    (let [{flashing true not-flashing false}
          (group-by
            #(< 9 (second %)) un-flashed)]

      (if (seq flashing)
        (let [boost-cells (apply concat (map (comp neighbours first)
                                             flashing))
              boosted     (reduce
                            (fn [board cell]
                              (if (contains? board cell)
                                (update board cell inc)
                                board))
                            (into {} not-flashing)
                            boost-cells)]

          (recur (merge flashed (into {} flashing))
                 boosted))

        (merge flashed (into {} un-flashed))))))

(defn count-flashes [board]
  (let [{have-flashed true} (group-by
                       #(< 9 (val %))
                       board)]
    (count have-flashed)))

(defn cycle-octopus [board]
  (into {} (map (fn [[xy e]]
                  [xy (if (< 9 e) 0 e)])
                board)))

(defn step-board [[flash-count board]]
  (let [after-flashes (flash-board (inc-energy board))
        flashes       (count-flashes after-flashes)]
    [(+ flash-count flashes) (cycle-octopus after-flashes)]))

(deftest test-larger-ex
  (testing
    (let [larger-example "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"
          large-board    (parse-board (clojure.string/split-lines larger-example))
          steps          (iterate step-board [0 large-board])]

      (is
        (= 204 (nth (map first steps) 10)))

      (is
        (= 1656 (nth (map first steps) 100))))))

(deftest test-part1
  (testing

    (let [part1-board (parse-board (clojure.string/split-lines (slurp "resources/Day11.txt")))
          part1-steps (iterate step-board [0 part1-board])
          step-100    (nth (map first part1-steps) 100)]
      (is
        (= 1725 step-100)))))

(defn flash-together [board]
  (let [steps (iterate step-board [0 board])]

    (reduce
      (fn [i board]
        (if (every? #{0} (vals board))
          (reduced i)
          (inc i)))
      0
      (map second steps))))

(deftest test-part2
  (testing "find when they flash together"
    (is (= 195 (flash-together (parse-board (clojure.string/split-lines (slurp "resources/Day11_test.txt"))))))
    (is (= 308 (flash-together (parse-board (clojure.string/split-lines (slurp "resources/Day11.txt"))))))
    ))