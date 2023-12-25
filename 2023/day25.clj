(ns clojure-aoc.core (:gen-class)
    (:require clojure.string))

(def input (-> (slurp "input.txt") (clojure.string/split #"\n")))

(defn parse-line [adjacency line]
  (let [[name c] (clojure.string/split line #": ")
        conns (clojure.string/split c #" ")
        inserted-name (assoc adjacency name (into (or (get adjacency name) #{}) conns))]
    (reduce #(assoc %1 %2 (conj (or (get %1 %2) #{}) name)) inserted-name conns)))

(defn shortest-path [start end adjacency]
  (loop [to-visit (conj clojure.lang.PersistentQueue/EMPTY [start []]) visited #{start}]
    (if-let [[curr-loc used-conns] (peek to-visit)]
      (if (= curr-loc end)
        used-conns
        (let [neighbours (filter #(not (contains? visited %)) (adjacency curr-loc))
              new-visits (map #(vector % (conj used-conns [curr-loc %])) neighbours)]
          (recur (into (pop to-visit) new-visits) (into visited neighbours))))
      nil)))

;; If the algorithm doesn't find a minimum cut, try increasing the number of nodes y can be.
;; This increases time usage but is more likely to be correct.
(defn most-used-connections [adjacency]
  (let [nodes (map first adjacency)
        paths (for [x nodes y (take 50 nodes) :while (not= x y)]
                (shortest-path x y adjacency))]
    (->> (map seq paths)
         (apply concat)
         frequencies)))

(defn visit-all [adjacency node]
  (loop [to-visit (conj clojure.lang.PersistentQueue/EMPTY node) visited #{node}]
    (if-let [curr-loc (peek to-visit)]
      (let [neighbours (filter #(not (contains? visited %)) (adjacency curr-loc))]
        (recur (into (pop to-visit) neighbours) (into visited neighbours)))
      visited)))

(defn disconnect [adjacency snips]
  (let [reducer (fn [acc [n1 n2]]
                  (let [interm (assoc acc n1 (disj (get acc n1) n2))]
                    (assoc interm n2 (disj (get acc n2) n1))))]
  (reduce reducer adjacency snips)))

(defn still-connected? [adjacency snips]
  (let [new-adjacency (disconnect adjacency snips)
        connected (visit-all new-adjacency (first (first adjacency)))]
    [(- (count adjacency) (count connected)) (count connected)]))

(defn all-distinct? [coll]
  (= (count coll) (count (distinct coll))))

(defn minimum-cut [adjacency top-conns]
  (let [cuts (for [x top-conns y top-conns z top-conns
                   :while (not= x y z)
                   :when (all-distinct? (flatten [x y z]))]
               [x y z])]
    (->> cuts
         (map #(still-connected? adjacency %))
         (filter #(not= (first %) 0))
         first)))

;; Takes about 5 minutes to run, but it's good enough.
;; Finds the shortest path between a bunch of nodes and notes the top 6 connections used.
;; Tries removing 3 of those connections until the graph is no longer connected.
(def part1
  (let [adjacency (reduce parse-line {} input)
      top-conns (->> (most-used-connections adjacency)
                 (sort-by second)
                 (take-last 6)
                 reverse
                 (map first))]
  (minimum-cut adjacency top-conns)))

(defn -main [& args]
  (println part1))
