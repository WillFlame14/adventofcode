(ns clojure-aoc.core (:gen-class)
  (:require [clojure-aoc.tools :as tools]
    clojure.string))

(def input (-> (slurp "input.txt") (clojure.string/split #"\n")))

(defn differences [x1 x2]
  (->> [x1 x2]
    tools/transpose
    (filter (fn [[a b]] (not= a b)))
    count))

(defn check-reflect [smudges grid i]
  (= smudges (->> (for [x (range 0 (min (inc i) (- (count grid) (inc i))))]
                    (->> [(- i x) (+ (inc i) x)]
                      (map #(nth grid %))
                      (apply differences)))
               (apply +))))

(defn find-reflect-row [smudges grid]
  (first (filter #(check-reflect smudges grid %) (range 0 (dec (count grid))))))

(defn find-reflect [smudges grid]
  (if-let [reflect-row (find-reflect-row smudges grid)]
    (* 100 (inc reflect-row))
    (inc (find-reflect-row smudges (tools/transpose grid)))))

(def part1
  (let [puzzles (tools/split-all #(= % "") input)]
    (->> puzzles
      (map #(find-reflect 0 %))
      (apply +))))

(def part2
  (let [puzzles (tools/split-all #(= % "") input)]
    (->> puzzles
      (map #(find-reflect 1 %))
      (apply +))))

(defn -main [& args]
  (println (str part1 " " part2)))
