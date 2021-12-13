(ns aoc-2021-in-clj.day08
  (:require [clojure.test :refer :all]
            [aoc-2021-in-clj.util :refer [read-input parse-int]]))

(defn parse-lines [input-path]
  (clojure.string/split-lines (slurp input-path)))

(defn part-1 [input-lines]
  (count
    (filter
      #{2 3 4 7}
      (apply concat (map
                      (fn [line] (map count (clojure.string/split line #"\s+")))
                      (map #(-> %
                                (clojure.string/split #"\|+")
                                second)
                           input-lines))))))

(deftest test-day08

  (let [test-input (parse-lines "resources/Day08_test.txt")]
    (is (= 26 (part-1 test-input))))

  (let [test-input (parse-lines "resources/Day08.txt")]
    (is (= 278 (part-1 test-input)))))

(def easy-signals
  {2 1
   3 7
   4 4
   7 8})

(defn parse-easy [sig]
  (get easy-signals (count sig)))

(defn contains-seq? [signal sub]
  (every? (set signal) sub))

(defn parse-med [sig known]
  (when (= 6 (count sig))
    (cond
      (contains-seq? sig (first (get known 4)))             ;; 4
      9

      (contains-seq? sig (first (get known 7)))             ;; 7
      0

      :else 6)))

(defn differences
  "How many chars are different in two seqs"
  [s1 s2]
  (count (filter #(= 1 %) (vals (frequencies (concat s1 s2))))))

(defn parse-hard [sig known]
  (cond (contains-seq? sig (first (get known 7)))
        3

        (= 1 (differences sig (first (get known 9))))
        5

        :else
        2))

(defn map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn map-keys [m f]
  (into {} (for [[k v] m] [(f k) v])))

(defn sorted-str [s] (apply str (sort s)))

(defn flip-map [m]
  (zipmap (vals m) (keys m)))

(defn parse-signal-line [signal-line]
  (let [signals         (clojure.string/split signal-line #"\s+")
        first-parse     (group-by parse-easy signals)
        second-parse    (group-by #(parse-med % first-parse) (get first-parse nil))
        combined-parses (merge first-parse second-parse)
        all-signals     (-> combined-parses
                            (merge (group-by #(parse-hard % combined-parses) (get combined-parses nil)))
                            (dissoc nil))]
    (map-vals all-signals first)))

(defn part2-signal-line-output [line]
  (let [[unique-sigs output] (clojure.string/split line #"\|+")
        decoded-sigs (flip-map (parse-signal-line unique-sigs))
        decoded-sigs (map-keys decoded-sigs sorted-str)
        output-sigs  (clojure.string/split output #"\s+")]
    (apply str
           (map (comp decoded-sigs sorted-str)
                output-sigs))))

(deftest test-day08-part2

  (let [signal-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
        signals     (flip-map (parse-signal-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab"))
        ]

    (is (true? (contains-seq? "cagedb" "dab")))
    (is (true? (contains-seq? "cefabd" "ab")))

    ;; 5 & 9 only have one difference
    (is (= 1 (differences "cefabd" "cdfbe")))

    ;; 2 & 9 only have one difference
    (is (= 3 (differences "cefabd" "gcdfa")))

    (is (= 8 (get signals "acedgfb")))
    (is (= 2 (get signals "gcdfa")))
    (is (= 3 (get signals "fbcad")))
    (is (= 7 (get signals "dab")))
    (is (= 5 (get signals "cdfbe")))
    (is (= 9 (get signals "cefabd")))
    (is (= 6 (get signals "cdfgeb")))
    (is (= 4 (get signals "eafb")))
    (is (= 0 (get signals "cagedb")))
    (is (= 1 (get signals "ab")))

    (is (= "5353" (part2-signal-line-output signal-line)))
    ))

(deftest test-part2-overall
  (is (= 986179
         (apply + (map parse-int (map part2-signal-line-output
                                      (clojure.string/split-lines (slurp "resources/Day08.txt"))))))))

