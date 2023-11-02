(ns clojure-aoc.core (:gen-class)
    (:require [clojure-aoc.tools :as tools]))

(def input (slurp "input.txt"))

(def step_size (Integer/parseInt input))

(defn step [[buffer curr] val]
  (let [index (-> curr (+ step_size) (mod (count buffer)) inc)]
    [(tools/insert_at buffer val index) index]))

(def part1
  (let [[buffer curr] (reduce step [[0] 0] (rest (range 2018)))]
    (nth buffer (inc curr))))

(defn after0 [iterations]
  (->> (reductions #(-> %1 (+ step_size) (mod %2) inc) (range iterations))
       (map vector (range iterations))
       (filter #(= (second %) 1))
       last
       first))

(def part2 (after0 50000000))

(defn -main [& args]
  (println (str part1 " " part2)))
