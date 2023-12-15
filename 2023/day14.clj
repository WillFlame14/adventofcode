(ns clojure-aoc.core (:gen-class)
  (:require [clojure-aoc.tools :as tools]
    clojure.string))

(def input (-> (slurp "input.txt") (clojure.string/split #"\n")))

(defn tilt-west-row [row]
  (let [parts (tools/split-including #(= % \#) row)
        tilt (fn [chars]
               (if (= (first chars) \#)
                 chars
                 (let [rocks (count (filter #(= % \O) chars))]
                   (concat (repeat rocks \O) (repeat (- (count chars) rocks) \.)))))]
    (->> parts
      (map tilt)
      (apply concat))))

(defn tilt-west [grid]
  (map tilt-west-row grid))

(defn tilt-dir [grid dir]
  (cond
    (= dir :left) (tilt-west grid)
    (= dir :up) (->> grid tools/transpose tilt-west tools/transpose)
    (= dir :right) (->> grid (map reverse) tilt-west (map reverse))
    (= dir :down) (->> grid tools/transpose (map reverse) tilt-west (map reverse) tools/transpose)))

(defn calc-load-row [row]
  (loop [i (count row) sum 0]
    (if (= i 0)
      sum
      (let [char (nth row (- (count row) i))]
        (cond
          (= char \O) (recur (dec i) (+ sum i))
          :else (recur (dec i) sum))))))

(defn calc-load [grid]
  (->> (tools/transpose grid)
    (map calc-load-row)
    (apply +)))

(defn next-iter [grid]
  (reduce #(tilt-dir %1 %2) grid [:up :left :down :right]))

(def part1
  (let [grid (map seq input)]
    (calc-load (tilt-dir grid :up))))

(def part2
  (let [grid (map seq input)
        {offset :offset tilt-cycle :cycle} (tools/find-cycle grid next-iter #(->> % flatten (apply str)))
        cycle-len (count tilt-cycle)
        final-grid (-> 1000000000 (- offset) (mod cycle-len) (+ offset))]
    (calc-load (nth (iterate next-iter grid) final-grid))))

(defn -main [& args]
  (println (str part1 " " part2)))
