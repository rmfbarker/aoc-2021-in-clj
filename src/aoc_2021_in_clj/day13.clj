(ns aoc-2021-in-clj.day13
  (:require [clojure.test :refer :all]
            [aoc-2021-in-clj.util :refer [read-input parse-int]]))

(defn parse-lines [input-path]
  (clojure.string/split-lines (slurp input-path)))

(defn calculate-dims [dots]
  (reduce
    (fn [[width height] [x y]]
      [(max width x) (max height y)])
    [0 0]
    dots))

(defn print-dots [dots]
  (let [[width height] (calculate-dims dots)]
    (doseq [h (range (inc height))]
      (do
        (doseq [x (range (inc width))]
          (if (contains? dots [x h])
            (print "#")
            (print ".")))
        (println)))))

(defn parse-xys [input-path]
  (let [co-ords (take-while
                  #(not= % "")
                  (parse-lines input-path))
        xys     (mapv #(mapv parse-int (clojure.string/split % #","))
                      co-ords)]
    xys))

(defn parse-fold-line [fold-line]
  (let [[xy _ & m] (drop (count "fold along ") fold-line)
        fold    (parse-int (apply str m))
        do-fold #(if (< fold %) (- (* 2 fold) %) %)]
    (if (= \x xy)
      (fn [[x y]]
        [(do-fold x) y])
      (fn [[x y]]
        [x (do-fold y)]))))

(defn parse-folds [fold-lines]
  (map parse-fold-line fold-lines))

(defn process-dots [input-path]
  (let [[co-ords [_ & folds]] (split-with
                                #(not= % "")
                                (parse-lines input-path))
        xys   (mapv #(mapv parse-int (clojure.string/split % #",")) co-ords)
        folds (map parse-fold-line folds)]

    (->>
      (reduce #(map %2 %1) xys folds)
      (into #{}))))

(defn part-1 []
  (let [input-path "resources/Day13.txt"
        x-fold-fn  (parse-fold-line "fold along x=655")]
    (count (into #{} (map x-fold-fn (parse-xys input-path))))))

(println (part-1))

(defn part-2 []
  (print-dots
    (set
      (reduce #(map %2 %1) (parse-xys "resources/Day13.txt")
              (parse-folds (drop-while
                             #(not (clojure.string/starts-with? % "fold"))
                             (parse-lines "resources/Day13.txt")))))))

(part-2)

(comment
  ;; part 2 short
  (print-dots
    (process-dots "resources/Day13.txt")))


