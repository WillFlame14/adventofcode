(ns clojure-aoc.core (:gen-class)
  (:require [clojure-aoc.grids :as grids]
    clojure.string))

(def input (-> (slurp "input.txt") (clojure.string/split #"\n")))

(def movements
  (memoize
    (fn [grid loc]
      (->> (grids/neighbours grid loc)
        (filter #(let [char (grids/at grid %)] (or (= char \.) (= char \S))))))))

;; The number of steps can be calculated by keeping track of a frontier of nodes and the previous steps.
;; The new number of steps can be calculated using (new frontier + number of steps from 2 iterations ago).
(defn dests [grid start steps]
  (loop [i 0 frontier #{start} last-frontier #{} last-sum 1 ll-sum 0]
    (if (= i steps)
      last-sum
      (let [reducer (fn [new-frontier loc]
                      (let [dests (movements grid loc)]
                        (into new-frontier (filter #(not (contains? last-frontier %)) dests))))
            new-frontier (reduce reducer #{} frontier)]
        (recur (inc i) new-frontier frontier (+ ll-sum (count new-frontier)) last-sum)))))

(def part1
  (let [grid (map seq input)
        start (grids/find-pred grid #(= \S (grids/at grid %)))]
    (dests grid start 64)))

;; Not needed as part of the answer, but helpful to prove that 131 steps (from the centre) will cover the entire grid.
;; Thus, being able to reach the corner of a cell is sufficient to prove that the cell will be filled.
(defn steps-to-fill [grid start]
  (loop [i 0 frontier #{start} last-frontier #{}]
    (if (empty? frontier)
      i
      (let [reducer (fn [new-frontier loc]
                      (let [dests (movements grid loc)]
                        (into new-frontier (filter #(not (contains? last-frontier %)) dests))))
            new-frontier (reduce reducer #{} frontier)]
        (recur (inc i) new-frontier frontier)))))

(defn get-corners [dim]
  [[0 0] [0 (dec dim)] [(dec dim) 0] [(dec dim) (dec dim)]])

(defn get-middles [dim]
  (let [mid (quot dim 2)]
    [[0 mid] [mid 0] [(dec dim) mid] [mid (dec dim)]]))

;; Because S is in the middle of a 131x131 grid and there's an unobstructed path along that row+column, along with the edges,
;; it is always optimal to start a cell at either one of the middle points or one of the corners.
;; Then, the number of tiles you can reach in cell (4, 3) is the same number of tiles as (3, 4), since both start in the bottom left corner.
;; After that, it's just math based on a checkerboard pattern and making sure you count the frontier of the diamond properly.
(def part2
  (let [grid (map seq input)
        dim (count grid)
        mid (quot dim 2)
        radius (dec (quot 26501365 dim))
        start (grids/find-pred grid #(= \S (grids/at grid %)))
        odd-steps (dests grid start dim)
        even-steps (dests grid start (inc dim))
        outers (map #(dests grid % (dec mid)) (get-corners dim))
        inners (map #(dests grid % (+ mid dim -1)) (get-corners dim))
        middles (map #(dests grid % (dec dim)) (get-middles dim))]
    (->> [(* (Math/pow radius 2) odd-steps)
          (* (Math/pow (inc radius) 2) even-steps)
          (map #(* (inc radius) %) outers)
          (map #(* radius %) inners)
          middles]
      flatten
      (apply +))))

(defn -main [& args]
  (println (str part1 " " part2)))
