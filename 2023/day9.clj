(ns clojure-aoc.core (:gen-class)
  (:require clojure.string))

(def input (-> (slurp "input.txt") (clojure.string/split #"\n")))

(defn extrapolate [coll back?]
  (if (every? #(= % 0) coll)
    0
    (let [differences (map - (next coll) coll)
          ex-next (extrapolate differences back?)]
      (if back?
        (-> (first coll) (- ex-next))
        (-> (last coll) (+ ex-next))))))

(defn oasis-report [back?]
  (->> input
    (map (fn [line] (map #(Integer/parseInt %) (clojure.string/split line #" "))))
    (map #(extrapolate % back?))
    (apply +)))

(def part1 (oasis-report false))

(def part2 (oasis-report true))

(defn -main [& args]
  (str part1 " " part2))
