(ns clojure-aoc.core (:gen-class)
    (:require [clojure-aoc.tools :as tools]
              clojure.string))

(def input (slurp "input.txt"))

(def grid (clojure.string/split input #"\n"))

(def opposite_dir
  {:down  :up
   :up    :down
   :left  :right
   :right :left})

(defn connections [[x y]]
  (->> (keys tools/grid_dir)
       (filter #(let [[x' y'] (map + [x y] (tools/grid_dir %))]
                  (and (< -1 x' (count (first grid)))
                       (< -1 y' (count grid))
                       (not= (tools/at grid [x' y']) \space))))))

(defn trace_path [start]
  (loop [[x y] start  dir :down  path ""  steps 0]
    (let [loc (tools/at grid [x y])]
      (if (= loc \space)
        [path steps]
        (let [next_dir (if (= loc \+)
                         (->> (connections [x y]) 
                              (filter #(not= % (opposite_dir dir)))
                              first)
                         dir)
              next_loc (map + [x y] (tools/grid_dir next_dir))
              next_path (str path (re-matches #"\w" (str loc)))]
          (recur next_loc next_dir next_path (inc steps)))))))

(def start
  [(tools/findIndex (first grid) #(= % \|)) 0])

(defn -main [& args]
  (println (trace_path start)))
