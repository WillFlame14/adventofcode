(ns clojure-aoc.core (:gen-class)
  (:require [clojure-aoc.tools :as tools]
    clojure.set
    clojure.string))

(def input (-> (slurp "input.txt") (clojure.string/split #"\n")))

(defn expand-line [lines]
  (reduce (fn [acc line] (-> (conj acc line) ((if (every? #(= % \.) line) #(conj % line) identity)))) [] lines))

(defn expand [grid]
  (->> (expand-line grid) tools/transpose expand-line tools/transpose))

(defn find-galaxies [grid]
  (for [y (range 0 (count grid))
        x (range 0 (count (nth grid y)))
        :when (= (tools/at grid [x y]) \#)]
    [x y]))

(defn in-bounds [grid [x y]]
  (and
    (< -1 y (count grid))
    (< -1 x (count (nth grid y)))))

(defn find-expanses-line [grid]
  (->> (map-indexed vector grid) (filter (fn [[_ line]] (every? #(= % \.) line))) (map first)))

(defn find-expanses [grid]
  {:rows (find-expanses-line grid)
   :cols (find-expanses-line (tools/transpose grid))})

(defn distance [[x1 y1] [x2 y2] expanses expanse-len]
  (let [axis-dist (fn [a1 a2 type] (+ (Math/abs (- a1 a2)) (->> (expanses type) (filter #(< (min a1 a2) % (max a1 a2))) count (* (dec expanse-len)))))
        [x-dist y-dist] (map #(apply axis-dist %) [[x1 x2 :cols] [y1 y2 :rows]])]
    (+ x-dist y-dist)))

(defn galaxy-dists [grid expanse-len]
  (let [expanses (find-expanses grid)
        galaxies (find-galaxies grid)]
    (->> (for [x galaxies y galaxies :while (not= x y)]
           (distance x y expanses expanse-len))
      (apply +))))

(def part1 (let [grid (map seq input)] (galaxy-dists grid 2)))

(def part2 (let [grid (map seq input)] (galaxy-dists grid 1000000)))

(defn -main [& args]
  (str part1 " " part2))
