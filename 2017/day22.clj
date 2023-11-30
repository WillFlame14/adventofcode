(ns clojure-aoc.core (:gen-class)
    (:require [clojure-aoc.tools :as tools]
              clojure.string))

(def input (slurp "input.txt"))

(defn turn [orig dir]
  (let [index (tools/find-index (keys tools/grid-dirs) #(= orig %))
        new-index (-> index ((cond
                               (= dir :none) identity
                               (= dir :right) inc
                               (= dir :reverse) #(+ 2 %)
                               (= dir :left) #(+ 3 %))) (mod 4))]
    (nth (keys tools/grid-dirs) new-index)))

(defn parse-grid [grid]
  (->> (for [x (range (count grid))
             y (range (count grid))
             :when (= \# (tools/at grid [x y]))]
         [x y])
       (into #{})))

(defn burst [infected loc dir]
  (let [infected? (contains? infected loc)
        new-infected ((if infected? disj conj) infected loc)
        new-dir (turn dir (if infected? :right :left))
        new-loc (vec (map + loc (get tools/grid-dirs new-dir)))]
    [new-infected new-loc new-dir (not infected?)]))

(def part1
  (let [grid (->> (clojure.string/split input #"\n") (map seq))
        infected (parse-grid grid)
        start (vec (repeat 2 (quot (count grid) 2)))
        iterations (iterate #(apply burst (pop %)) [infected start :up false])]
    (->> (take (inc 10000) iterations)
         (filter #(last %))
         count)))

(def states [nil :weakened :infected :flagged])

(defn burst2 [infected loc dir]
  (let [curr-type (get infected loc)
        new-infected (if (= curr-type :flagged)
                       (dissoc infected loc)
                       (let [ind (tools/find-index states #(= curr-type %))]
                         (assoc infected loc (nth states (inc ind)))))
        new-dir (turn dir (cond
                            (nil? curr-type) :left
                            (= curr-type :weakened) :none
                            (= curr-type :infected) :right
                            (= curr-type :flagged) :reverse))
        new-loc (vec (map + loc (get tools/grid-dirs new-dir)))]
    [new-infected new-loc new-dir (= curr-type :weakened)]))

(def part2
  (let [grid (->> (clojure.string/split input #"\n") (map seq))
        infected (->> (parse-grid grid) (reduce #(assoc %1 %2 :infected) {}))
        start (vec (repeat 2 (quot (count grid) 2)))
        iterations (iterate #(apply burst2 (pop %)) [infected start :up false])]
    (->> (take (inc 10000000) iterations)
         (filter #(last %))
         count)))

(defn -main []
  (println (str part1 " " part2)))
