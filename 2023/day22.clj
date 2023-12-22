(ns clojure-aoc.core (:gen-class)
  (:require [clojure-aoc.grids :as grids]
    clojure.set
    clojure.string))

(def input (-> (slurp "input.txt") (clojure.string/split #"\n")))

(defn parse-brick [line]
  (let [[x1 y1 z1 x2 y2 z2] (->> (re-matches #"(\d+),(\d+),(\d+)~(\d+),(\d+),(\d+)" line) next (map #(Integer/parseInt %)))]
    [(gensym) [[x1 x2] [y1 y2] [z1 z2]]]))

(defn fall-brick [fallen [id [[x1 x2] [y1 y2] [z1 z2]]]]
  (let [xy-coords (for [x (range x1 (inc x2)) y (range y1 (inc y2))] [x y])
        final-z (->> xy-coords (map #(->> (grids/at fallen %) keys (apply max))) (apply max) inc)
        new-fallen (if (= z1 z2)
                     (reduce (fn [acc [x y]] (assoc-in acc [y x final-z] id)) fallen xy-coords)
                     (reduce (fn [acc z] (assoc-in acc [y1 x1 z] id)) fallen (range final-z (+ final-z (Math/abs (- z2 z1)) 1))))
        new-coords [[x1 x2] [y1 y2] [final-z (+ final-z (Math/abs (- z2 z1)))]]]
    [new-fallen [id new-coords]]))

(defn find-supporting [fallen [[x1 x2] [y1 y2] [z1 z2]]]
  (let [xy-coords (for [x (range x1 (inc x2)) y (range y1 (inc y2))] [y x (inc (max z1 z2))])]
    (->> xy-coords
      (map #(get-in fallen %))
      (filter (complement nil?))
      distinct)))

(defn find-supported [fallen [[x1 x2] [y1 y2] [z1 z2]]]
  (let [xy-coords (for [x (range x1 (inc x2)) y (range y1 (inc y2))] [x y])]
    (->> xy-coords
      (map (fn [[x y]] (get-in fallen [y x (dec (min z1 z2))])))
      (filter (complement nil?))
      distinct)))

(defn multi-support? [fallen bricks id]
  (let [supporting (find-supported fallen (bricks id))]
    (> (count supporting) 1)))

(defn safe-disintegrate [fallen bricks [_ coords]]
  (let [supported-bricks (find-supporting fallen coords)]
    (every? #(multi-support? fallen bricks %) supported-bricks)))

(def part1
  (let [bricks (->> (map parse-brick input) (sort-by (fn [[_ [_ _ [z1 z2]]]] (min z1 z2))))
        ground (vec (repeat 10 (vec (repeat 10 {0 :ground}))))
        reducer (fn [[fallen final-bricks] brick]
                  (let [[new-fallen new-brick] (fall-brick fallen brick)]
                    [new-fallen (conj final-bricks new-brick)]))
        [final-fallen final-bricks] (reduce reducer [ground {}] bricks)]
    (->> final-bricks
      (filter #(safe-disintegrate final-fallen final-bricks %))
      count)))

(defn get-supported [fallen bricks]
  (reduce (fn [acc [id coords]] (assoc acc id (find-supported fallen coords))) {} bricks))

(defn disintegrate-fall [bricks supported-by id]
  (loop [disintegrated #{id}]
    (let [new-disintegrated (->> (map first bricks)
                              (filter (fn [id] (and (not (contains? disintegrated id))
                                                 (every? #(contains? disintegrated %) (supported-by id))))))]
      (if (empty? new-disintegrated)
        disintegrated
        (recur (into disintegrated new-disintegrated))))))

(def part2
  (let [bricks (->> (map parse-brick input) (sort-by (fn [[_ [_ _ [z1 z2]]]] (min z1 z2))))
        ground (vec (repeat 10 (vec (repeat 10 {0 :ground}))))
        reducer (fn [[fallen final-bricks] brick]
                  (let [[new-fallen new-brick] (fall-brick fallen brick)]
                    [new-fallen (conj final-bricks new-brick)]))
        [final-fallen final-bricks] (reduce reducer [ground {}] bricks)
        supported-by (get-supported final-fallen final-bricks)]
    (->> final-bricks
      (map #(disintegrate-fall final-bricks supported-by (first %)))
      (map (comp dec count))
      (apply +))))

(defn -main [& args]
  (println (str part1 " " part2)))
