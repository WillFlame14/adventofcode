(ns clojure-aoc.core (:gen-class)
  (:require clojure.string))

(defn parse-set [str]
  (let [cols (clojure.string/split str #", ")
        reducer #(let [[amt type] (clojure.string/split %2 #" ")]
                   (assoc %1 type (Integer/parseInt amt)))]
    (reduce reducer {} cols)))

(defn parse [line]
  (let [[intro rem] (clojure.string/split line #": ")
        id (->> (clojure.string/split intro #" ") second Integer/parseInt)
        sets (clojure.string/split rem #"; ")]
    {:id id :sets (map parse-set sets)}))

(defn get-val [set colour]
  (or (set colour) 0))

(defn possible [set]
  (and
    (<= (get-val set "green") 13)
    (<= (get-val set "red") 12)
    (<= (get-val set "blue") 14)))

(def part1
  (let [input (slurp "input.txt")]
    (->> (clojure.string/split input #"\n")
      (map parse)
      (filter (fn [{sets :sets}] (every? possible sets)))
      (map :id)
      (apply +))))

(defn min-vals [sets]
  (let [max-col (fn [col] (apply max (map #(get-val % col) sets)))]
    (map max-col ["red" "green" "blue"])))

(defn power [game]
  (->> (game :sets)
    min-vals
    (apply *)))

(def part2
  (let [input (slurp "input.txt")]
    (->> (clojure.string/split input #"\n")
      (map (comp power parse))
      (apply +))))

(defn -main [& args]
  (str part1 " " part2))
