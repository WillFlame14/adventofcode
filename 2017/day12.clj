(ns clojure-aoc.core (:gen-class)
    (:require [clojure-aoc.tools :as tools]
              clojure.string
              clojure.set))

(def input (slurp "input.txt"))

(defn parse [line]
  (let [[p _ & conn] (-> line
                         (clojure.string/replace #"," "")
                         (clojure.string/split #" "))]
    [(Integer/parseInt p) (map #(Integer/parseInt %) conn)]))

(def pipes
  (let [lines (clojure.string/split input #"\n")]
    (reduce #(let [[prog conn] (parse %2)] (assoc %1 prog conn)) {} lines)))

(defn connected [node visited]
  (let [neighbours (get pipes node)
        new_visited (clojure.set/union visited (set neighbours))]
    (->> neighbours
         (filter #(not (contains? visited %)))
         (map #(connected % new_visited))
         (reduce clojure.set/union new_visited))))

(def part1 (count (connected 0 #{0})))

(defn assign_groups [acc curr]
  (if (contains? acc curr)
    acc
    (let [group (connected curr #{curr})
          sym (gensym)]
      (reduce #(assoc %1 %2 sym) acc group))))

(def part2
  (->> (keys pipes)
       (reduce assign_groups {})
       vals
       distinct
       count))

(defn -main [& args]
  (println (str part1 " " part2)))

