(ns aoc-2021-in-clj.day06
  (:require [clojure.test :refer :all]
            [aoc-2021-in-clj.util :refer [read-input parse-int]]))

(defn parse-fish [s] (map parse-int (clojure.string/split s #",")))

(defn part-1 [input-fish-str days]

  (let [fish (parse-fish input-fish-str)
        population (frequencies fish)
        age-population (fn [population]
                         (let [aged (into {} (map (fn [[age n]] [(dec age) n])
                                                  population))
                               dead-fish (get aged -1 0)]
                           (-> aged
                               (update 6 (fnil + 0) dead-fish)
                               (assoc 8 dead-fish)
                               (dissoc -1))))
        pop-after-days (nth (iterate age-population population)
                            days)
        fish-count (apply + (vals pop-after-days))]
    fish-count))

(deftest test-day06

  (is (= 26 (part-1 "3,4,3,1,2" 18)))
  (is (= 5934 (part-1 "3,4,3,1,2" 80)))
  (is (= 362666 (part-1 (clojure.string/trim (slurp "resources/Day06.txt"))
                        80)))

  (is (= 1640526601595 (part-1 (clojure.string/trim (slurp "resources/Day06.txt"))
                               256)))
  )

