(ns clojure-aoc.core (:gen-class)
  (:require [clojure-aoc.tools :as tools]
    [clojure-aoc.grids :as grids]
    clojure.string))

(def input (-> (slurp "input.txt") (clojure.string/split #"\n")))

(defn longest-path [grid start end]
  (loop [to-visit [[start #{start} 0]] curr-max 0]
    (if-let [[curr-loc visited path-len] (peek to-visit)]
      (if (= curr-loc end)
        (recur (pop to-visit) (max curr-max path-len))
        (let [char (grids/at grid curr-loc)
              neighbours (cond
                           (= char \.) (grids/neighbours grid curr-loc)
                           (= char \v) [(map + [0 1] curr-loc)]
                           (= char \>) [(map + [1 0] curr-loc)])
              valid? #(and (not= (grids/at grid %) \#) (not (contains? visited %)))
              valid-neighbours (filterv valid? neighbours)
              new-visits (map #(vector % (conj visited %) (inc path-len)) valid-neighbours)]
          (recur (into (pop to-visit) new-visits) curr-max)))
      curr-max)))

(def part1
  (let [grid (map seq input)
        start [(tools/find-index #(= \. %) (first grid)) 0]
        end [(tools/find-index #(= \. %) (last grid)) (dec (count grid))]]
    (longest-path grid start end)))

(defn find-intersections [grid]
  (for [y (range 0 (count grid))
        x (range 0 (count (nth grid y)))
        :when (let [neighbours (grids/neighbours grid [x y])]
                (and (= \. (grids/at grid [x y]))
                  (> (count (filter #(contains? #{\> \v} (grids/at grid %)) neighbours)) 1)))]
    [x y]))

(defn find-connections [grid node nodes]
  (loop [to-visit [[node 0]] visited #{node} connections []]
    (if-let [[curr-loc path-len] (peek to-visit)]
      (if (and (not= curr-loc node) (some #(= % curr-loc) nodes))
        (recur (pop to-visit) (conj visited curr-loc) (conj connections [curr-loc path-len]))
        (let [neighbours (grids/neighbours grid curr-loc)
              valid? #(and (not= (grids/at grid %) \#) (not (contains? visited %)))
              valid-neighbours (filterv valid? neighbours)]
          (recur (into (pop to-visit) (map #(vector % (inc path-len)) valid-neighbours)) (into visited valid-neighbours) connections)))
      connections)))

(defn longest-path2 [start end adjacency]
  (loop [to-visit [[start #{start} 0]] curr-max 0]
    (if-let [[curr-loc visited path-len] (peek to-visit)]
      (if (= curr-loc end)
        (recur (pop to-visit) (max curr-max path-len))
        (let [neighbours (filter #(not (contains? visited (first %))) (adjacency curr-loc))
              new-visits (map (fn [[dest len]] (vector dest (conj visited dest) (+ len path-len))) neighbours)]
          (recur (into (pop to-visit) new-visits) curr-max)))
      curr-max)))

(def part2
  (let [grid (map seq input)
        start [(tools/find-index #(= \. %) (first grid)) 0]
        end [(tools/find-index #(= \. %) (last grid)) (dec (count grid))]
        nodes (into (find-intersections grid) [start end])
        reducer (fn [a curr-node]
                  (let [connections (find-connections grid curr-node nodes)]
                    (reduce (fn [acc [dest path-len]]
                              (assoc-in (assoc-in acc [curr-node dest] path-len) [dest curr-node] path-len)) a connections)))
        adjacency (reduce reducer {} nodes)]
    (longest-path2 start end adjacency)))

(defn -main [& args]
  (println (str part1 " " part2)))
