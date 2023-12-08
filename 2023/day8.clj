(ns clojure-aoc.core (:gen-class)
  (:require [clojure-aoc.tools :as tools]
    clojure.string))

(def input (-> (slurp "input.txt") (clojure.string/split #"\n")))

(defn parse-connection [line]
  (let [[_ loc l-dest r-dest] (first (re-seq #"(\w+) = \((\w+), (\w+)\)" line))]
    [loc {\L l-dest \R r-dest}]))

(defn traverse [loc dir connections]
  ((connections loc) dir))

(def part1
  (let [dirs (seq (first input))
        connections (into {} (map parse-connection (drop 2 input)))]
    (loop [i 0 loc "AAA"]
      (if (= loc "ZZZ")
        i
        (let [new-loc (traverse loc (nth dirs (mod i (count dirs))) connections)]
          (recur (inc i) new-loc))))))

(defn get-cycles [starts dirs connections]
  (let [update (fn [[loc i]]
                 (let [new-loc (traverse loc (nth dirs i) connections)
                       new-i (mod (inc i) (count dirs))]
                   [new-loc new-i]))]
    (map #(tools/find-cycle [% 0] update identity) starts)))


;; First, find all the cycles formed by each ghost. Pretty sure that a cycle is guaranteed since the path length and combinations are finite.
;; Then, notice that in each cycle, only one Z node is reached by each ghost.
;; Next, notice that each ghost takes the same amount of time to reach the Z node again as it takes to reach the Z node the first time.
;; Thus, no offset is needed, and you can just take the LCM between the times to reach the Z nodes for every ghost.
(def part2
  (let [dirs (seq (first input))
        connections (into {} (map parse-connection (drop 2 input)))
        starts (->> (map first connections) (filter #(= (last %) \A)))
        cycles (get-cycles starts dirs connections)]
    (apply tools/lcm (map #(count (% :cycle)) cycles))))

(defn -main [& args]
  (str part1 " " part2))

(-main)
