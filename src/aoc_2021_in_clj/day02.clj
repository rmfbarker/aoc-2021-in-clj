(ns aoc-2021-in-clj.day02
  (:require [aoc-2021-in-clj.util :refer [read-input parse-int]]))


(defn part-1 [input-path]
  (let [{:strs [forward down up]}
        (reduce
          (fn [old instruction]
            (let [[dir mag] (re-seq #"\w+" instruction)
                  mag (parse-int mag)]
              (assoc old dir (+ mag (get old dir 0)))))
          {}
          (read-input input-path))]
    (* forward (- down up))))



(println "Test answer:" (part-1 "resources/Day02_test.txt"))
(println "Part 1 answer:" (part-1 "resources/Day02.txt"))

;down X increases your aim by X units.
;up X decreases your aim by X units.
;forward X does two things:
;It increases your horizontal position by X units.
;It increases your depth by your aim multiplied by X.

(def init-coords {:aim 0 :horizontal 0 :depth 0})

(defn part-2 [input-path]
  (let [{:keys [horizontal depth]}
        (reduce
          (fn [old instruction]
            (let [[dir mag] (re-seq #"\w+" instruction)
                  mag (parse-int mag)
                  aim (get old :aim)]
              (case dir
                "down" (update old :aim + mag)
                "up" (update old :aim - mag)
                "forward" (-> old
                              (update :horizontal + mag)
                              (update :depth + (* mag aim))))))
          init-coords
          (read-input input-path))]
    (* horizontal depth)))

(println "Part 2 test answer:" (part-2 "resources/Day02_test.txt"))
(println "Part 2 answer:" (part-2 "resources/Day02.txt"))
