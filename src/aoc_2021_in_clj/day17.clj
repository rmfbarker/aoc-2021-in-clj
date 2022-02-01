(ns aoc-2021-in-clj.day17
  (:require [clojure.test :refer :all]
            [aoc-2021-in-clj.util :refer [read-input parse-int]]))


;; The probe's x,y position starts at 0,0.
;; Then, it will follow some trajectory by moving in steps.
;; On each step, these changes occur in the following order:
;
;    The probe's x position increases by its x velocity.
;    The probe's y position increases by its y velocity.
;    Due to drag, the probe's x velocity changes by 1 toward the value 0;
;       that is, it decreases by 1 if it is greater than 0,
;       increases by 1 if it is less than 0, or does not change if it is already 0.
;    Due to gravity, the probe's y velocity decreases by 1.

(defn move-probe [[position velocity]]
  ; position and velocity are [x,y] co-ords

  (let [[x y] position
        [x-vel y-vel] velocity]
    [[(+ x x-vel) (+ y y-vel)] [(cond
                                  (< 0 x-vel) (dec x-vel)
                                  (< x-vel 0) (inc x-vel)
                                  (= 0) 0)
                                (dec y-vel)]]))

;; target area: x=20..30, y=-10..-5

(defn get-bounds [target-range]
  (let [[low high] (clojure.string/split target-range #"\.\.")]
    [(parse-int low) (parse-int high)]))

(def matcher (re-matcher #"[-]?\d+\.\.[-]?\d+" "target area: x=20..30, y=-10..-5"))
(get-bounds (re-find matcher))

(defn parse-bounds [s]
  (let [matcher  (re-matcher #"[-]?\d+\.\.[-]?\d+" s)
        x-bounds (get-bounds (re-find matcher))
        y-bounds (get-bounds (re-find matcher))]
    [x-bounds y-bounds]))

(defn get-positions [velocity]
  (iterate move-probe [[0 0] velocity]))

(defn passes-target? [target-str positions]
  (let [[x-bounds y-bounds] (parse-bounds target-str)]

    (loop [positions positions]
      (let [[[x y] [x-v y-v]] (first positions)]
        (println "pos" x y "vel" x-v y-v)
        (cond

          (and (<= (first x-bounds) x (second x-bounds))
               (<= (first y-bounds) y (second y-bounds))) true

          (or (and (neg? x-v) (< x (first x-bounds)))

              (and (pos? x-v) (< (second x-bounds) x))

              (and (neg? y-v) (< y (first y-bounds)))) false

          :else (recur (rest positions))

          )
        )
      )
    )
  )

;; perform this check by reduction, and when the probe has moved past the window, then complete
;; is the co-ord past the higher bound

(deftest examples
  (let [target-str "target area: x=20..30, y=-10..-5"]
    (is (true? (passes-target? target-str (get-positions [7, 2]))))
    (is (true? (passes-target? target-str (get-positions [6 3]))))
    (is (true? (passes-target? target-str (get-positions [9 0]))))
    (is (false? (passes-target? target-str (get-positions [17 -4]))))
    )
  )