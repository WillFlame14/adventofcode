(ns clojure-aoc.core (:gen-class)
    (:require [clojure.core.matrix :as matrix]
              [clojure.core.matrix.linear :as linear]
              clojure.string))

(def input (-> (slurp "input.txt") (clojure.string/split #"\n")))

(defn parse-hailstone [line]
  (->> (re-matches #"(\d+), (\d+), (\d+) @ *(-?\d+), *(-?\d+), *(-?\d+)" line)
                   next
                   (map #(Long/parseLong %))))

(defn intersect [[x1 y1 z1 vx1 vy1 vz1] [x2 y2 z2 vx2 vy2 vz2]]
  (let [denom (- (/ (* vy2 vx1) vx2) vy1)]
    (if (zero? denom)
      nil
      (let [t1 (/ (- y1 y2 (* (/ vy2 vx2) (- x1 x2))) (- (/ (* vy2 vx1) vx2) vy1))
            x' (+ (* vx1 t1) x1)
            y' (+ (* vy1 t1) y1)
            t2 (/ (+ (* vx1 t1) x1 (- x2)) vx2)]
        [t1 t2 x' y']))))

(def part1
  (let [hailstones (map parse-hailstone input)
        min-test 200000000000000
        max-test 400000000000000]
    (->> (for [a hailstones b hailstones :while (not= a b)]
           (intersect a b))
         (filter (complement nil?))
         (filter (fn [[t1 t2 x y]] (and (pos? t1) (pos? t2) (< min-test x max-test) (< min-test y max-test))))
         count)))

;; For part 2, note that we have n+6 unknowns and 3n equations. Thus, we only need n = 3 to solve.
;; We can get vector equations p0 + t[i]*v0 == p[i] + t[i]*v[i] ==> (p0 - p[i]) == - t[i] * (v0 - v[i]) ==>
;; (p0 - p[i]) x (v0 - v[i]) == 0, after right crossing both sides by (v0 - v[i]) since the vectors are parallel.
;; This is still bilinear, but if we pick two i's then they have a common p0 x v0 term which can then be removed.
;; We thus get p0 x (v[i] - v[j]) + v0 x (p[i] - p[j]) - (p[i] x v[i] - p[j] x v[j]) == 0 for hailstones i, j,
;; and can pick two pairs to solve the system of 6 linear equations.

(defn symbolic-cross [[p1 v1] [p2 v2]]
  (let [[p12x p12y p12z] (matrix/sub p1 p2)
        [v12x v12y v12z] (matrix/sub v1 v2)]
  [[0        v12z     (- v12y) 0        p12z     (- p12y)]
   [(- v12z) 0        v12x     (- p12z) 0        p12x]
   [v12y     (- v12x) 0        p12y     (- p12x) 0]]))

(defn augh [[p1 v1] [p2 v2] [p3 v3]]
  (let [b1 (matrix/sub (matrix/cross p1 v1) (matrix/cross p2 v2))
        b2 (matrix/sub (matrix/cross p1 v1) (matrix/cross p3 v3))
        a (concat (symbolic-cross [p1 v1] [p2 v2]) (symbolic-cross [p1 v1] [p3 v3]))
        b (concat b1 b2)]
    (matrix/mmul (linear/solve a) b)))

(def part2
  (let [hailstones (map parse-hailstone input)
      [x y z _ _ _] (apply augh (map #(partition 3 %) (take 3 hailstones)))]
  (long (+ x y z))))

(defn -main [& args]
  (println part1 " " part2))
