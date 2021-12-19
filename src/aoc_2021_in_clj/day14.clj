(ns aoc-2021-in-clj.day14
  (:require [clojure.test :refer :all]
            [aoc-2021-in-clj.util :refer [read-input parse-int]]))

(defn parse-lines [input-path]
  (clojure.string/split-lines (slurp input-path)))

(defn parse-insertion-rules [input-path]
  (let [[_ _ & insertion-mappings] (parse-lines input-path)]
    (into {} (map
               #(clojure.string/split % #" -> ")
               insertion-mappings))))

(defn grow-pair [[elem n] insertions state]
  (if (contains? insertions elem)
    (let [insert (get insertions elem)
          a      (str (first elem) insert)
          b      (str insert (last elem))]
      (-> state
          (update a (fnil + 0) n)
          (update b (fnil + 0) n)))
    (update state elem (fnil + 0) n)))

(defn parse-polymer [polymer]
  (let [pairs (map #(apply str %) (partition 2 1 polymer))]
    (frequencies pairs)))

(defn step
  "returns the count of the elements after performing a single step of insertions"
  [insertion-rules pairs]
  (reduce
    #(grow-pair %2 insertion-rules %1)
    {}
    pairs))

(defn count-elems [pair-counts]
  (let [elems (set (apply str (keys pair-counts)))
        [first-count last-count] (reduce
                                   (fn [[firsts lasts] [pair n]]
                                     [(update firsts (first pair) (fnil + 0) n)
                                      (update lasts (last pair) (fnil + 0) n)])
                                   [(zipmap elems (repeat 0))
                                    (zipmap elems (repeat 0))]
                                   pair-counts)]
    (reduce (fn [acc elem]
              (assoc acc elem (max (get first-count elem)
                                   (get last-count elem))))
            {}
            elems)))

(defn elem-diff [elem-counts]
  (let [counts (sort (vals elem-counts))]
    (- (last counts) (first counts))))

(deftest test-part-2


  (testing "perform a single step of insertions on a polymer"

    (is
      (=
        (frequencies "NCNBCHB")
        (count-elems (step
                       (parse-insertion-rules "resources/Day14_test.txt")
                       (parse-polymer "NNCB")))))

    (is
      (=
        (frequencies "NBCCNBBBCBHCB")
        (count-elems (nth (iterate
                            (partial step (parse-insertion-rules "resources/Day14_test.txt"))
                            (parse-polymer "NNCB"))
                          2))))

    (is (= (frequencies "NBBBCNCCNBBNBNBBCHBHHBCHB")
           (count-elems (nth (iterate
                               (partial step (parse-insertion-rules "resources/Day14_test.txt"))
                               (parse-polymer "NNCB"))
                             3))))

    (is (= (frequencies "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")
           (count-elems
             (nth (iterate
                    (partial step (parse-insertion-rules "resources/Day14_test.txt"))
                    (parse-polymer "NNCB"))
                  4))))

    (is (= {\B 1749, \C 298, \H 161, \N 865}
           (count-elems
             (nth (iterate
                    (partial step (parse-insertion-rules "resources/Day14_test.txt"))
                    (parse-polymer "NNCB"))
                  10))))

    (is (= 1588
           (elem-diff
             (count-elems
               (nth (iterate
                      (partial step (parse-insertion-rules "resources/Day14_test.txt"))
                      (parse-polymer "NNCB"))
                    10)))))

    (is (= 2188189693529
           (elem-diff
             (count-elems
               (nth (iterate
                      (partial step (parse-insertion-rules "resources/Day14_test.txt"))
                      (parse-polymer "NNCB"))
                    40)))))

    "this is part 1"
    (is (= 2408
           (elem-diff
             (count-elems
               (nth (iterate
                      (partial step (parse-insertion-rules "resources/Day14.txt"))
                      (parse-polymer "KHSNHFKVVSVPSCVHBHNP"))
                    10)))))

    "this is part 2"
    (is (= 2651311098752
           (elem-diff
             (count-elems
               (nth (iterate
                      (partial step (parse-insertion-rules "resources/Day14.txt"))
                      (parse-polymer "KHSNHFKVVSVPSCVHBHNP"))
                    40)))))))

