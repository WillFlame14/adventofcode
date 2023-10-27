(ns clojure-aoc.core (:gen-class)
    (:require [clojure-aoc.tools :as tools]
              clojure.string))

(def input (slurp "input.txt"))

(defn spin [coll amt]
  (tools/rotate coll (- (count coll) amt)))

(defn exchange [coll a b]
  (tools/swap coll a b))

(defn partner [coll a b]
  (tools/swap coll (.indexOf coll a) (.indexOf coll b)))

(defn do_move [coll [move & parts]]
  (cond
    (= move \s) (spin coll (Integer/parseInt (apply str parts)))
    (= move \x) (let [[a b] (->> (clojure.string/split (apply str parts) #"/")
                                 (map #(Integer/parseInt %)))]
                  (exchange coll a b))
    (= move \p) (let [[a / b] parts] (partner coll a b))))

(defn dance [coll steps]
  (->> (reduce do_move (into (vector) coll) steps)
       (apply str)))

(def part1
  (let [steps (clojure.string/split input #",")]
    (dance "abcdefghijklmnop" steps)))

(def part2
  (let [steps (clojure.string/split input #",")
        {offset :offset cycle :cycle}
        (tools/find_cycle "abcdefghijklmnop" #(dance % steps) #(apply str %))]
    (nth cycle (mod (- 1000000000 offset) (count cycle)))))

  (defn -main [& args]
    (println (str part1 " " part2)))
