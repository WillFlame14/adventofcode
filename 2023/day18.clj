(ns clojure-aoc.core (:gen-class)
  (:require [clojure-aoc.grids :as grids]
    clojure.string))

(def input (-> (slurp "input.txt") (clojure.string/split #"\n")))

(def dirs {"R" :right "D" :down "L" :left "U" :up})

(defn parse-vertices1 [loc line]
  (let [[_ d a _] (re-matches #"(.) (\d+) \(#(\w+)\)" line)
        dir (dirs d)
        amt (Integer/parseInt a)]
    (mapv + loc (map #(* % amt) (grids/grid-dirs dir)))))

(defn parse-vertices2 [loc line]
  (let [[_ a d] (re-matches #". \d+ \(#(\w+)([0-3])\)" line)
        dir (second (nth (seq dirs) (Integer/parseInt d)))
        amt (Integer/parseInt a 16)]
    (mapv + loc (map #(* % amt) (grids/grid-dirs dir)))))

(defn calc-filled [vertices]
  (->> (partition 2 vertices)
    (map #(inc (Math/abs (apply - %))))
    (apply +)))

(defn xor-vertices [old-vertices curr-vertices]
  (->> (concat old-vertices curr-vertices)
    frequencies
    (filter #(= 1 (second %)))
    (map first)
    sort))

(defn overlap [[a b] [c d]]
  (max 0 (inc (- (min b d) (max a c)))))

(defn overlap-vertices [old-vertices new-vertices]
  (->> (for [x (partition 2 old-vertices)
             y (partition 2 new-vertices)]
         (overlap x y))
    (apply +)))

(defn enclosed [path]
  (let [grouped (->> (group-by second path) (map (fn [[y pts]] [y (map first pts)])) (sort-by first))
        initial (conj (first grouped) 0)
        reducer' (fn [[old-index old-vertices old-sum] [curr-index curr-vertices]]
                   (let [old-filled (calc-filled old-vertices)
                         new-vertices (xor-vertices old-vertices curr-vertices)
                         last-sum (-> (- curr-index old-index)
                                    inc
                                    (* old-filled)
                                    (- (overlap-vertices old-vertices new-vertices)))]
                     [curr-index new-vertices (+ old-sum last-sum)]))]
    (last (reduce reducer' initial (next grouped)))))

(def part1
  (let [path (butlast (reduce #(conj %1 (parse-vertices1 (last %1) %2)) [[0 0]] input))]
    (enclosed path)))

(def part2
  (let [path (butlast (reduce #(conj %1 (parse-vertices2 (last %1) %2)) [[0 0]] input))]
    (enclosed path)))

(defn -main [& args]
  (println (str part1 " " part2)))
