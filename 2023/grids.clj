(ns clojure-aoc.grids
  (:require [clojure-aoc.tools :as tools]))

(def grid-dirs
  {:up [0 -1]
   :right [1 0]
   :down [0 1]
   :left [-1 0]})

(def opposite-dir
  {:up :down
   :left :right
   :down :up
   :right :left})

(defn turn [orig dir]
  (let [index (tools/find-index #(= orig %) (keys grid-dirs))
        new-index (-> index ((cond
                               (= dir :none) identity
                               (= dir :right) inc
                               (= dir :reverse) #(+ 2 %)
                               (= dir :left) #(+ 3 %))) (mod 4))]
    (nth (keys grid-dirs) new-index)))

(defn at [grid [x y]]
  (nth (nth grid y) x))

(defn rotate-cw [grid]
  (->> (tools/transpose grid) (map reverse)))

(defn flip [grid]
  (map reverse grid))

(defn square-partition [size grid]
  (->> grid
    (partition size)
    (map #(->> (tools/transpose %)
            (partition size)
            (map tools/transpose)))
    (apply concat)))

(defn in-bounds [grid [x y]]
  (and
    (< -1 y (count grid))
    (< -1 x (count (nth grid y)))))

(defn neighbours [grid loc]
  (->> (vals grid-dirs)
    (map #(map + loc %))
    (filter #(in-bounds grid %))))