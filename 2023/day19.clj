(ns clojure-aoc.core (:gen-class)
  (:require [clojure-aoc.tools :as tools]
    clojure.string))

(def input (-> (slurp "input.txt") (clojure.string/split #"\n")))

(defn parse-rule [line]
  (if-let [[_ var op x res] (re-matches #"([xmas])([<>])(\d+):(\w+)" line)]
    [var op (Integer/parseInt x) res]
    [line]))

(defn parse-workflow [line]
  (let [[_ name parts] (re-matches #"(\w+)\{(.*)\}" line)
        rules (clojure.string/split parts #",")]
    [name (map parse-rule rules)]))

(defn parse-part [line]
  (let [[_ x m a s] (re-matches #"\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}" line)
        [x' m' a' s'] (map #(Integer/parseInt %) [x m a s])]
    {"x" x' "m" m' "a" a' "s" s'}))

(defn process-workflow [rules part]
  (loop [i 0]
    (when-let [rule (nth rules i)]
      (if (= (count rule) 1)
        (first rule)
        (let [[var op x res] rule]
          (if ((if (= op "<") < >) (part var) x)
            res
            (recur (inc i))))))))

(defn process [workflows part]
  (loop [name "in"]
    (let [rules (workflows name)
          next-w (process-workflow rules part)]
      (cond
        (= next-w "R") 0
        (= next-w "A") (->> (map second part) (apply +))
        :else (recur next-w)))))

(def part1
  (let [[w p] (tools/split-all #(= % "") input)
        workflows (into {} (map parse-workflow w))
        parts (map parse-part p)]
    (->> (map #(process workflows %) parts)
      (apply +))))

(def inds {"x" 0 "m" 1 "a" 2 "s" 3})

(defn restrict [[lower upper] o x]
  (let [[new-low new-up] (if (= o "<") [lower (dec x)] [(inc x) upper])
        [else-low else-up] (if (= o "<") [x upper] [lower x])
        valid? (< new-low new-up)]
    [valid? [new-low new-up] [else-low else-up]]))

(defn combs [ranges]
  (apply * (map (comp inc #(Math/abs %) #(apply - %)) ranges)))

(defn accepted [workflows name bounds]
  (cond
    (= name "A") (combs bounds)
    (= name "R") 0
    :else (let [rules (workflows name)
                reducer (fn [[sum bounds] rule]
                          (if (= (count rule) 1)
                            [(+ sum (accepted workflows (first rule) bounds)) bounds]
                            (let [[var op x res] rule
                                  [valid? new-bounds else-bounds] (restrict (bounds (inds var)) op x)
                                  next-bounds (assoc bounds (inds var) new-bounds)
                                  next-else-bounds  (assoc bounds (inds var) else-bounds)]
                              [(+ sum (if valid? (accepted workflows res next-bounds) 0)) next-else-bounds])))]
            (first (reduce reducer [0 bounds] rules)))))

(def part2
  (let [[w _] (tools/split-all #(= % "") input)
        workflows (into {} (map parse-workflow w))]
    (accepted workflows "in" [[1 4000] [1 4000] [1 4000] [1 4000]])))

(defn -main [& args]
  (println (str part1 " " part2)))
