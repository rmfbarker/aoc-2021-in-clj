(ns aoc-2021-in-clj.day10
  (:require [clojure.test :refer :all]
            [aoc-2021-in-clj.util :refer :all]))

(def reverse-bracket {\) \(
                      \] \[
                      \} \{
                      \> \<})

(def illegal-points {\) 3
                     \] 57
                     \} 1197
                     \> 25137})

(defn valid-line? [input]
  (loop [input input]
    (let [[i c :as closing] (first (filter (fn [[_ c]] (#{\) \] \} \>} c))
                                           (map vector (range) input)))]

      (cond
        (nil? closing) true                                 ;; no closing braces remain, successful

        (= (get input (dec i))
           (reverse-bracket c))                             ;; yup, this is still valid, keep checking
        (recur (str (subs input 0 (dec i))
                    (subs input (inc i))))

        :else false))))

(defn line-value [input]
  (loop [input input]
    (let [[i c :as closing] (first (filter (fn [[_ c]] (#{\) \] \} \>} c))
                                           (map vector (range) input)))]

      (cond
        (nil? closing) nil

        (= (get input (dec i))
           (reverse-bracket c))                             ;; yup, this is still valid, keep checking
        (recur (str (subs input 0 (dec i))
                    (subs input (inc i))))

        :else c))))

(deftest test-day10-part1
  (is (true? (valid-line? "()")))
  (is (false? (valid-line? "(]")))

  (is (true? (valid-line? "{()()()}")))
  (is (false? (valid-line? "{()()()>")))

  (is (true? (valid-line? "(((())))")))
  (is (false? (valid-line? "(((()))}")))

  (is (true? (valid-line? "<([]){()}[{}]>")))
  (is (false? (valid-line? "<([]){()}[{}])")))

  ;; when I close, all containing chunks must be closed
  (is (false? (valid-line? "{([(<{}[<>[]}>{[]{[(<()>")))

  (is (false? (valid-line? "[[<[([]))<([[{}[[()]]]")))
  (is (false? (valid-line? "[{[{({}]{}}([{[{{{}}([]")))

  (is (false? (valid-line? "[<(<(<(<{}))><([]([]()")))

  (is (false? (valid-line? "<{([([[(<>()){}]>(<<{{")))

  ;(is (false? (first (parse-lines "resources/Day10_test.txt"))))

  (is (= 26397 (apply + (map (comp illegal-points line-value) (filter (complement valid-line?) (parse-lines "resources/Day10_test.txt"))))))
  (is (= 311949 (apply + (map (comp illegal-points line-value) (filter (complement valid-line?) (parse-lines "resources/Day10.txt"))))))
  )

(comment

  (find-lowpoints "resources/Day09_test.txt")

  )

