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
    (is (= 278 (part-1 test-input)))))

(def easy-signals
  {2 1
   3 7
   4 4
   7 8})

(defn parse-easy [sig]
  (get easy-signals (count sig)))

(defn contains-subsignal [signal sub]
  (every? (set signal) sub))

(defn parse-med [sig easies]
  (cond
    (and (= 6 (count sig))
         (contains-subsignal sig (first (get easies 1)))    ;; 1
         (contains-subsignal sig (first (get easies 4))))   ;; 4
    9

    (and (= 6 (count sig))
         (contains-subsignal sig (first (get easies 1)))    ;; 1
         (contains-subsignal sig (first (get easies 7))))   ;; 7
    0

    (= 6 (count sig))
    6

    :else nil))

(defn differences [s1 s2]
  (count (filter (fn [[k v]]
                   (= 1 v)) (frequencies (concat s1 s2))))
  )

(defn parse-hard [sig meds]
  (cond (contains-subsignal sig (first (get meds 7)))
        3
        (= 1 (differences sig (first (get meds 9))))
        5

        :else
        2))

(defn map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn parse-signal-line [signal-line]
  (let [first-parse
        (group-by parse-easy
                  (clojure.string/split signal-line #"\s+"))]
    (println "first parse" first-parse)
    (println "unmatched" (get first-parse nil))
    (let [second-parse    (group-by #(parse-med % first-parse) (get first-parse nil))
          combined-parses (merge first-parse second-parse)]
      (println "unmatched 3" (get combined-parses nil))
      (let [all-signals (-> combined-parses
                            (merge (group-by #(parse-hard % combined-parses) (get combined-parses nil)))
                            (dissoc nil))]
        (map-vals all-signals first)))))

(defn flip-map [m]
  (zipmap (vals m) (keys m)))

(defn part2-signal-line-output [line]
  (let [[unique-sigs output] (map clojure.string/trim (clojure.string/split line #"\|"))
        decoded-sigs (flip-map (parse-signal-line unique-sigs))
        decoded-sigs (into {} (map (fn [[k v]] [(apply str (sort k)) v]) decoded-sigs))
        output-sigs  (clojure.string/split output #"\s")]
    (println decoded-sigs)
    (println output-sigs)

    (apply str (map (fn [sig] (get decoded-sigs (apply str (sort sig)))) output-sigs))
    )

  )

(deftest test-day08-part2

  (let [signal-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
        signals     (flip-map (parse-signal-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab"))
        ]

    (is (true? (contains-subsignal "cagedb" "dab")))
    (is (true? (contains-subsignal "cefabd" "ab")))

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

