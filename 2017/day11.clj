(ns clojure-aoc.core (:gen-class)
    (:require clojure.string))

(def input (slurp "input.txt"))

(defn hex_dir [s]
  (case s
    "nw" [-1 1]
    "n" [0 2]
    "ne" [1 1]
    "se" [1 -1]
    "s" [0 -2]
    "sw" [-1 -1]))

(defn steps [[x y]]
  (let [dist (+ (Math/abs x) (Math/abs y))]
    (quot dist 2)))

(def part1
  (->> (clojure.string/split input #",")
       (map hex_dir)
       (reduce #(mapv + %1 %2) [0 0])
       steps))

(defn reducer [[max_steps last_loc] dir]
  (let [new_loc (map + last_loc dir)
        curr_steps (steps new_loc)]
    [(max curr_steps max_steps) new_loc]))

(def part2
  (->> (clojure.string/split input #",")
       (map hex_dir)
       (reduce reducer [0 [0 0]])
       first))

  (defn -main [& args]
    (println (str part1 " " part2)))
