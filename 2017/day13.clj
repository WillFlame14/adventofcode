(ns clojure-aoc.core (:gen-class)
    (:require [clojure-aoc.tools :as tools]
              clojure.string
              clojure.set))

(def input (slurp "input.txt"))

(defn parse [line]
  (let [[_ depth range] (re-matches #"(\d+): (\d+)" line)]
    [(Integer/parseInt depth) {:range (Integer/parseInt range)
                               :loc 1
                               :dir :down}]))

(def initial_firewall
  (into {} (map parse (clojure.string/split input #"\n"))))

(defn step_layer [layer]
  (let [{range :range
         loc :loc
         dir :dir} layer
        ndir (if (and (= dir :down) (= loc range))
               :up
               (if (and (= dir :up) (= loc 1)) :down dir))]
    (assoc layer
           :loc ((if (= ndir :up) dec inc) loc)
           :dir ndir)))

(defn step_wall [firewall]
  (reduce #(let [[depth layer] %2]
             (assoc %1 depth (step_layer layer))) {} firewall))

(defn severity [delay history]
  (reduce #(let [[depth] %2
                 {loc :loc range :range} (get (nth history (+ depth delay)) depth)
                 curr_severity (if (= loc 1) (* depth range) 0)]
             (+ %1 curr_severity))
          0 initial_firewall))

(def history (iterate step_wall initial_firewall))

(def part1 (severity 0 history))

(defn bypass_guard [delay [depth layer]]
  (let [{range :range} layer]
    (not= (mod (+ delay depth) (* 2 (dec range))) 0)))

(defn uncaught [delay]
  (every? #(bypass_guard delay %) initial_firewall))

(def part2 (first (filter uncaught (range))))

(defn -main [& args]
  (time (println (str part1 " " part2))))
