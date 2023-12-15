(ns clojure-aoc.core (:gen-class)
  (:require [clojure-aoc.tools :as tools]
    clojure.string))

(def input (-> (slurp "input.txt") (clojure.string/split #",")))

(defn hash-alg [line]
  (reduce #(-> %1 (+ (int %2)) (* 17) (mod 256)) 0 (seq line)))

(def part1 (->> input (map hash-alg) (apply +)))

(defn h-insert [hashmap key val]
  (let [key-hash-alg (hash-alg key)
        lenses (or (get hashmap key-hash-alg) [])
        lens [key (Integer/parseInt val)]
        new-hmap (if-let [ind (tools/find-index #(= (first %) key) lenses)]
                   (assoc lenses ind lens)
                   (conj lenses lens))]
    (assoc hashmap key-hash-alg new-hmap)))

(defn h-remove [hashmap key]
  (let [key-hash-alg (hash-alg key)
        lenses (get hashmap key-hash-alg)]
    (if-let [ind (tools/find-index #(= (first %) key) lenses)]
      (assoc hashmap key-hash-alg (tools/vec-remove ind lenses))
      hashmap)))

(defn focus-power-box [box-ind box]
  (->> (map second box)
    (map-indexed vector)
    (map (fn [[ind val]] (* box-ind (inc ind) val)))
    (apply +)))

(defn focus-power [hashmap]
  (->> hashmap
    (map (fn [[box-ind box]] (focus-power-box (inc box-ind) box)))
    (apply +)))

(defn execute [hashmap cmd]
  (let [[_ key type val] (re-matches #"(\w+)([-=])(\w+)?" cmd)]
    (cond
      (= type "=") (h-insert hashmap key val)
      (= type "-") (h-remove hashmap key))))

(def part2 (->> input (reduce execute {}) focus-power))

(defn -main [& args]
  (println (str part1 " " part2)))
