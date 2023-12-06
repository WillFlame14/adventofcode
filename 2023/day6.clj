(ns clojure-aoc.core (:gen-class)
  (:require [clojure-aoc.tools :as tools]
    clojure.string))

(def input (-> (slurp "input.txt") (clojure.string/split #"\n")))

(defn wins [time distance]
  (->> (for [x (range 1 time)] (* x (- time x)))
    (filter #(> % distance))
    count))

(def part1
  (let [[times distances] (map (fn [line] (->> (clojure.string/split line #"\s+")
                                            next
                                            (map #(Integer/parseInt %)))) input)]
    (->> (tools/transpose [times distances])
      (map #(apply wins %))
      (apply *))))

(def part2
  (let [[time distance] (map (fn [line] (->> (clojure.string/split line #"\s+")
                                          next
                                          (apply str)
                                          Long/parseLong)) input)]
    (wins time distance)))

(defn -main [& args]
  (str part1 " " part2))
