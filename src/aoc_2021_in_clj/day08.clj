(ns aoc-2021-in-clj.day08
  (:require [clojure.test :refer :all]
            [aoc-2021-in-clj.core :refer [read-input parse-int]]))

(defn parse-lines [input-path]
  (clojure.string/split-lines (slurp input-path)))

(defn part-1 [input-lines]
  (count
    (filter
      #{2 3 4 7}
      (apply concat (map
                      (fn [line] (map count (clojure.string/split line #"\s")))
                      (map #(-> %
                                (clojure.string/split #"\|")
                                second
                                clojure.string/trim)
                           input-lines))))))

(deftest test-day08

  (let [test-input (parse-lines "resources/Day08_test.txt")]
    (is (= 26 (part-1 test-input)))
    )

  (let [test-input (parse-lines "resources/Day08.txt")]
    (is (= 278 (part-1 test-input)))
    )

  )

(comment

  (map #(clojure.string/split % #"\|") (clojure.string/split-lines (slurp "resources/Day08_test.txt")))

  )