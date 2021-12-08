(ns aoc-2021-in-clj.day1
  (:require [aoc-2021-in-clj.core :refer [read-input]])
  (:gen-class))

(defn part-1 [lines]
  (count
    (filter
      #(apply < %)
      (partition
        2 1
        (map
          read-string
          lines)))))

(defn part-2 [lines]
  "apply a sliding window of three measurements"
  (count (filter #(apply < %) (partition 2 1
                                         (map #(apply + %)
                                              (partition 3 1
                                                         (map read-string lines)))))))



(defn -main
  [& args]
  (let [test-data (read-input "resources/Day01_test.txt")
        day1-data (read-input "resources/Day01.txt")]
    (println "test example" (= 7 (part-1 test-data)))
    (println "part 1" (part-1 day1-data))
    (println "part 2" (part-2 day1-data))
    )

  )
