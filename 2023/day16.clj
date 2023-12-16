(ns clojure-aoc.core (:gen-class)
  (:require [clojure-aoc.grids :as grids]
    clojure.string))

(def input (-> (slurp "input.txt") (clojure.string/split #"\n")))

(def mirrors
  {\\ {:left :up
       :down :right
       :up :left
       :right :down}
   \/ {:left :down
       :down :left
       :up :right
       :right :up}})

(defn traverse [grid start]
  (loop [to-visit [start] visited #{}]
    (if (empty? to-visit)
      visited
      (let [[[cx cy] cdir :as curr] (peek to-visit)
            char (grids/at grid [cx cy])
            new-locs (cond
                       (= char \.) [[(map + [cx cy] (grids/grid-dirs cdir)) cdir]]
                       (some #(= % char) "\\/") (let [new-dir (get-in mirrors [char cdir])]
                                                  [[(map + [cx cy] (grids/grid-dirs new-dir)) new-dir]])
                       (some #(= % char) "|-") (let [split? (if (= char \|)
                                                              (some #(= % cdir) [:left :right])
                                                              (some #(= % cdir) [:up :down]))
                                                     new-dirs (if split?
                                                                (if (= char \|) [:up :down] [:left :right])
                                                                [cdir])]
                                                 (map #(vec [(map + [cx cy] (grids/grid-dirs %)) %]) new-dirs)))
            filtered-locs (filter (fn [[loc _ :as test]] (and
                                                           (grids/in-bounds grid loc)
                                                           (not (contains? visited test)))) new-locs)]
        (recur (into (pop to-visit) filtered-locs) (conj visited curr))))))

(defn energized [grid start]
  (->> (traverse grid start)
    (map first)
    distinct
    count))

(def part1
  (let [grid (map seq input)]
    (energized grid [[0 0] :right])))

(defn starts [grid]
  (let [ylen (count grid)
        xlen (count (nth grid 0))]
    (concat
      (map #(vector [0 %] :right) (range 0 ylen))
      (map #(vector [% 0] :down) (range 0 xlen))
      (map #(vector [(dec xlen) %] :left) (range 0 ylen))
      (map #(vector [% (dec ylen)] :up) (range 0 xlen)))))

;; There is a faster solution involving caching.. but this only takes around 80 seconds to run.
(def part2
  (let [grid (map seq input)]
    (->> (starts grid)
      (map #(energized grid %))
      (apply max))))

(defn -main [& args]
  (println (str part1 " " part2)))
