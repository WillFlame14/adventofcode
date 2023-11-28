(ns clojure-aoc.core (:gen-class)
    (:require [clojure-aoc.tools :as tools]
              clojure.string))

(def input (slurp "input.txt"))

(defn to-matrix [line]
  (->> (clojure.string/split line #"/") (map seq)))

(defn parse-mapping [line]
  (map to-matrix (clojure.string/split line #" => ")))

(defn matrix-rotate-cw [matrix]
  (->> (tools/transpose matrix) (map reverse)))

(defn matrix-flip [matrix]
  (map reverse matrix))

(defn all-forms [matrix]
  (->> [matrix (matrix-flip matrix)]
       (map #(take 4 (iterate matrix-rotate-cw %)))
       (apply concat)
       (into #{})))

(defn hash-form [matrix]
  (apply str (flatten matrix)))

(defn insert-mapping [dict [input output]]
  (let [input-forms (all-forms input)]
    (reduce #(assoc %1 (hash-form %2) output) dict input-forms)))

(defn parse-mappings [input]
  (->> (clojure.string/split input #"\n")
       (map parse-mapping)
       (reduce insert-mapping {})))

(defn square-partition [size grid]
  (->> grid
       (partition size)
       (map #(->> (tools/transpose %)
                  (partition size)
                  (map tools/transpose)))
       (apply concat)))

(defn reconstruct [partitions]
  (let [dim (Math/sqrt (count partitions))]
    (->> (partition (int dim) partitions)
         (map #(->> (tools/transpose %) (map flatten)))
         (apply concat))))

(defn enhance [mappings grid]
  (let [partition-size (if (even? (count grid)) 2 3)
        partitions (square-partition partition-size grid)
        new-partitions (map #(mappings (hash-form %)) partitions)]
    (reconstruct new-partitions)))

(def initial-grid ".#./..#/###")

(defn on-after-iterations [iters]
  (let [mappings (parse-mappings input)]
    (->> (to-matrix initial-grid)
         (iterate #(enhance mappings %))
         (take (inc iters))
         last
         flatten
         (filter #(= % \#))
         count)))

(def part1 (on-after-iterations 5))

(def part2 (on-after-iterations 18))

(defn -main []
  (println (str part1 " " part2)))
