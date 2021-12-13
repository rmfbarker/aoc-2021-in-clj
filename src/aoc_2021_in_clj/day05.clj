(ns aoc-2021-in-clj.day05
  (:require [clojure.test :refer :all]
            [aoc-2021-in-clj.util :refer [read-input parse-int]]))

(defn start-end [vent-line]
  (map #(mapv parse-int (-> %
                            (clojure.string/trim)
                            (clojure.string/split #",")))
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

(defn move [a b] (cond (= a b) a
                       (< a b) (inc a)
                       (> a b) (dec a)))

(defn move-coord [[x y] [x-target y-target]]
  [(move x x-target)
   (move y y-target)])

(defn parse-vent-di [vent-line]
  (let [[start end] (start-end vent-line)]
    (reduce
      (fn [acc next]
        (let [acc (conj acc next)]
          (if (= next end)
            (reduced acc)
            acc)))
      []
      (iterate #(move-coord % end)
               start))))

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
             coord-counts))))

(defn part-2 [file-path]
  (let [coords (apply concat
                      (map parse-vent-di
                           (clojure.string/split-lines
                             (slurp file-path))))
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

(deftest day05-part2-tests

  (is (= [[1 1] [2 2] [3 3]]
         (sort (parse-vent-di "1,1 -> 3,3"))
         ))

  (is (= (sort [[1 1] [2 2] [3 3]])
         (sort (parse-vent-di "3,3 -> 1,1"))
         ))

  (is (= (sort [[9 7], [8 8], [7 9]])
         (sort (parse-vent-di "9,7 -> 7,9"))
         ))

  (is (= (sort [[9 7], [8 8], [7 9]])
         (sort (parse-vent-di "7,9 -> 9,7"))
         ))

  (is (= 12 (part-2 "resources/Day05_test.txt")))
  (is (= 18423 (part-2 "resources/Day05.txt")))
  )

(comment

  "We can do a reduce or a loop-recur to generate the lines of co-ords"

  (let [[start end] (start-end "9,7 -> 7,9")]
    (reduce
      (fn [acc next]
        (let [acc (conj acc next)]
          (if (= next end)
            (reduced acc)
            acc)))
      []
      (iterate #(move-coord % end)
               start)))

  (let [[start end] (start-end "9,7 -> 7,9")]
    (loop [[coord & coords] (iterate #(move-coord % end)
                                     start)
           result []]
      (let [result (conj result coord)]
        (if (= coord end)
          result
          (recur coords result)))))

  )