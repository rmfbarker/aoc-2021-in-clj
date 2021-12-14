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
  (is (true? (valid-line? "{()()()}")))

  (is (false? (valid-line? "(]")))
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

(defn get-incomplete-chunks [input]
  (loop [input input]
    (let [[i c :as closing] (first (filter (fn [[_ c]] (#{\) \] \} \>} c))
                                           (map vector (range) input)))]

      (cond
        (nil? closing) input

        (= (get input (dec i))
           (reverse-bracket c))
        (recur (str (subs input 0 (dec i))
                    (subs input (inc i))))))))

(defn autocomplete [input]
  (apply str (reverse (map (flip-map reverse-bracket)
                           (get-incomplete-chunks input)))))

(def closing-values {\) 1
                     \] 2
                     \} 3
                     \> 4})

(defn completion-score [braces]

  (reduce
    (fn [sum c] (+ (* 5 sum) (closing-values c)))
    0
    braces))

(defn part-2 [lines]
  ; filter incomplete lines
  (let [scores (map (comp completion-score autocomplete)
                    (filter valid-line? lines))]
    (nth (sort scores)
         (quot (count scores) 2))))

(deftest test-day10-part2
  (is (= "}}]])})]" (autocomplete "[({(<(())[]>[[{[]{<()<>>")))
  (testing
    "autocompletions"
    (are [completion line] (= completion (autocomplete line))
                           "}}]])})]" "[({(<(())[]>[[{[]{<()<>>"
                           ")}>]})" "[(()[<>])]({[<{<<[]>>("
                           "}}>}>))))" "(((({<>}<{<{<>}{[]{[]{}"
                           "]]}}]}]}>" "{<[[]]>}<{[{[{[]{()[[[]"
                           "])}>" "<{([{{}}[<[[[<>{}]]]>[]]"))

  (testing "compute autocomplete values"
    (are [completion value] (= value (completion-score completion))
                            "])}>" 294

                            "}}]])})]" 288957
                            ")}>]})" 5566
                            "}}>}>))))" 1480781
                            "]]}}]}]}>" 995444
                            "])}>" 294
                            ))

  (testing "part 2"
    (is (= 288957
           (part-2 (parse-lines "resources/Day10_test.txt"))
           ))
    (is (= 3042730309
           (part-2 (parse-lines "resources/Day10.txt"))
           ))
    )
  )

(comment

  (find-lowpoints "resources/Day09_test.txt")

  )

