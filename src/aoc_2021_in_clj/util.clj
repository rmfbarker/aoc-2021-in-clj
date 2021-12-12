(ns aoc-2021-in-clj.util)

(defn parse-lines [input-path]
  (clojure.string/split-lines (slurp input-path)))

(defn contains-seq? [signal sub]
  (every? (set signal) sub))

(defn differences
  "How many chars are different in two seqs"
  [s1 s2]
  (count (filter #(= 1 %) (vals (frequencies (concat s1 s2))))))

(defn map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn map-keys [m f]
  (into {} (for [[k v] m] [(f k) v])))

(defn sorted-str [s] (apply str (sort s)))

(defn flip-map [m]
  (zipmap (vals m) (keys m)))










