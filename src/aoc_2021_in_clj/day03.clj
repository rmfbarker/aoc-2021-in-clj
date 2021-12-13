(ns aoc-2021-in-clj.day03
  (:require [aoc-2021-in-clj.util :refer [read-input parse-int]]))

(defn parse-binary [b] (Integer/parseInt b 2))

(defn part-1 [input]
  (let [epsilon (reduce
                  (fn [res bits]
                    (let [most-common
                          (->> bits frequencies (sort-by val) last key)]
                      (str res most-common)))
                  ""
                  (apply map vector (clojure.string/split-lines input)))
        gamma   (apply str (map #(case %
                                   \0 \1
                                   \0)
                                epsilon))]
    (println "epsilon" epsilon "gamma" gamma)
    (* (parse-binary epsilon) (parse-binary gamma))))

(println "Test answer:" (part-1 (slurp "resources/Day03_test.txt")))
(println "Part 1 answer:" (part-1 (slurp "resources/Day03.txt")))
(println "Part 1 answer:" (= 2035764 (part-1 (slurp "resources/Day03.txt"))))

(defn determine-rating [f input-lines]
  (reduce
    (fn [remaining idx]
      (if (= 1 (count remaining))
        (reduced remaining)
        (let [by-idx-bit (group-by
                           #(nth % idx)
                           remaining)
              chosen-bit (first
                           (f (sort-by #(count (val %))
                                       by-idx-bit)))]
          (filter
            #(= chosen-bit (nth % idx))
            remaining))))
    (sort
      input-lines)
    (range)))

(defn oxygen-generator-rating [input-lines]
  ;; chose the most common bit - when sorted by bit, this will be the last
  (determine-rating last input-lines))

(defn co2-scrubber-rating [input-lines]
  ;; chose the most common bit - when sorted by bit, this will be the first
  (determine-rating first input-lines))

(defn part2 [input-path]
  (let [input-lines    (clojure.string/split-lines (slurp input-path))
        ox-rating      (oxygen-generator-rating input-lines)
        co2-rating     (co2-scrubber-rating input-lines)
        ox-rating-dec  (parse-binary (apply str ox-rating))
        co2-rating-dec (parse-binary (apply str co2-rating))
        result         (* ox-rating-dec co2-rating-dec)]
    (println "oxygen generator rating" ox-rating " " ox-rating-dec)
    (println "co2 scrubber rating" co2-rating " " co2-rating-dec)
    (println "result" result)
    result))

(println "part 2 test result" (part2 "resources/Day03_test.txt"))
(println "part 2 result" (part2 "resources/Day03.txt"))