(ns aoc-2021-in-clj.day11
  (:require [clojure.string :refer [split-lines]]
            [clojure.test :refer :all]
            [aoc-2021-in-clj.util :refer :all]))

;; represent the board as a map of xy co-ords and energy levels {[x y] e}
;; this makes it easy to locate a cell and increase its energy levels


(defn get-adjacent [board xy]
  (let [{:keys [width height]} board]
    (filter (fn [[x y]] (and (< -1 x width)
                             (< -1 y height)))
            (neighbours xy))))

(defn step [lines] lines)

(defn part-1 [lines steps] 0)

(deftest test-day11-part1
  (testing "basic example single step"
    (let [input "11111\n19991\n19191\n19991\n11111\n"
          lines (split-lines input)]

      (is (= (split-lines "34543\n40004\n50005\n40004\n34543")
             (step lines)))

      (is (= (split-lines "45654\n51115\n61116\n51115\n45654")
             (step (step lines))))
      ))

  (testing "count flashes after 100 flashes"
    (is (= 1656 (part-1 (parse-lines "resources/Day11_test.txt")
                        100))))

  )


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
      (println (apply str (map second (sort-by get-x (filter #(= row (get-y %)) board))))))))

(print-board (parse-board lines))

(defn neighbours [[x y]]
  (for [dx [-1 0 1] dy (if (zero? dx) [-1 1] [-1 0 1])]
    [(+ dx x) (+ dy y)]))

(map neighbours (map first (parse-board lines)))

;; each position is represented by a tuple of an xy coord
;; and the energy level e.g. [[x y] e]

(defn step-board [board]
  (let [aged          (map #(vector (first %) (inc (second %))) board)
        {flashing true not-flashing false} (group-by
                                             #(< 9 (second %)) aged)

        step          (loop [flashing flashing not-flashing not-flashing]
                        (let [boost-cells (apply concat (map (comp neighbours first)
                                                             flashing))
                              boosted     (reduce
                                            (fn [board cell]
                                              (if (get board cell)
                                                (update board cell inc)
                                                board))
                                            (into {} not-flashing)
                                            boost-cells)
                              {new-flashing true new-not-flashing false} (group-by
                                                                           #(< 9 (second %)) boosted)]
                          (if (seq new-flashing) (recur new-flashing new-not-flashing)
                                                 (merge (into {} flashing) boosted))))
        after-boost   (merge (into {} flashing) step)
        flash-count   (reduce (fn [c [_ e]]
                                (if (= 10 e) (inc c) c))
                              0 after-boost)
        step-complete (into {} (map (fn [[xy e]]
                                      [xy (if (= 10 e) 0 e)])
                                    after-boost))
        ]
    (println flash-count)
    (print-board step-complete)

    ))

(step-board (parse-board lines))