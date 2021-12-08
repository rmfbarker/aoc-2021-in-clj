(ns aoc-2021-in-clj.core
  (:gen-class))

(defn read-input [path]
  (clojure.string/split-lines (slurp path)))

(defn parse-int [s] (Integer/parseInt s))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
