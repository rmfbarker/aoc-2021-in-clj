(ns aoc-2021-in-clj.day15
  (:require [clojure.test :refer :all]))

(defn parse-char-to-int [c] (Character/getNumericValue c))

(defn parse-lines [input-path]
  (clojure.string/split-lines (slurp input-path)))

(defn read-input [file-path]
  (mapv #(mapv parse-char-to-int %)
        (map seq
             (clojure.string/split-lines (slurp file-path)))))


(defn part-1 [file-path]
  (let [world  (read-input file-path)
        target [(dec (count (first world))) (dec (count world))]]

    (loop [visited #{}
           points  (sorted-set [0 [0 0]])]                  ;; priority queue

      (let [[cost [x y] :as point] (first points)]
        ;; visit this node - the node with the least cost so far
        (if (= [x y] target)
          cost
          (let [visited (conj visited [x y])
                points  (reduce (fn [acc [x1 y1]]
                                  (if-let [risk (and (not (visited [x1 y1]))
                                                     (get-in world [y1 x1]))]
                                    (conj acc [(+ cost risk) [x1 y1]])
                                    acc))
                                (disj points point)
                                [[x (inc y)] [x (dec y)] [(inc x) y] [(dec x) y]])]
            (recur visited points)))))))

(read-input "resources/Day15_test.txt")
(part-1 "resources/Day15_test.txt")
(part-1 "resources/Day15.txt")

(defn repeating-rows []
  (let [world (read-input "resources/Day15_test.txt")]
    (for [r (range (count world))]
      (iterate (fn [row]
                 (mapv #(mod (inc %) 9) row))
               (get world r)))))

(defn build-rows []
  (apply concat (take 5 (repeating-rows))))


(defn build-new-world []
  (let [rows     (build-rows)
        all-rows (apply concat (for [r (range (count rows))]
                                 (take 5 (iterate (fn [row]
                                                    (mapv #(mod (inc %) 9) row))
                                                  (nth rows r)))))

        ]
    (for [r (apply concat (take 5 (iterate #(map inc %) (range 0 (inc 25) 5))))]
      (nth all-rows r)
      )
    ))

(defn build-part-2-world [file-path]
  (mapv #(vec (apply concat %))
        (vec (apply concat
                    (vec (for [z (range 5)]
                           (mapv
                             #(take 5 (drop z %))
                             (let [world (read-input file-path)
                                   rows  (mapv (fn [row]
                                                 (iterate (fn [old] (mapv
                                                                      (fn [a]
                                                                        (if (< 9 (inc a)) 1 (inc a)))
                                                                      old)) row))
                                               world)]
                               (for [i (range (count world))]
                                 (nth rows i)
                                 )
                               ))))))))

(build-part-2-world "resources/Day15_test.txt")

(defn part-2 [file-path]
  (let [world  (build-part-2-world file-path)
        target [(dec (count (first world))) (dec (count world))]]

    (loop [visited #{}
           points  (sorted-set [0 [0 0]])]                  ;; priority queue

      (let [[cost [x y] :as point] (first points)]
        ;; visit this node - the node with the least cost so far
        (if (= [x y] target)
          cost
          (let [visited (conj visited [x y])
                points  (reduce (fn [acc [x1 y1]]
                                  (if-let [risk (and (not (visited [x1 y1]))
                                                     (get-in world [y1 x1]))]
                                    (conj acc [(+ cost risk) [x1 y1]])
                                    acc))
                                (disj points point)
                                [[x (inc y)] [x (dec y)] [(inc x) y] [(dec x) y]])]
            (recur visited points)))))))

(deftest test-part-2
  (is (= 315 (part-2 "resources/Day15_test.txt")))
  (is (= 2938 (part-2 "resources/Day15.txt"))))
