(ns clojure-aoc.core (:gen-class)
  (:require [clojure-aoc.tools :as tools]
    [clojure-aoc.grids :as grids]
    clojure.set
    clojure.string))

(def input (-> (slurp "input.txt") (clojure.string/split #"\n")))

(def pipes
  {\| [:up :down]
   \- [:left :right]
   \L [:up :right]
   \J [:up :left]
   \7 [:down :left]
   \F [:down :right]
   \. []})

(defn find-start [grid]
  (first (for [y (range (count grid))
               x (range (count (nth grid y)))
               :when (= (tools/at grid [x y]) \S)]
           `(~x ~y))))



(defn find-neighbours [grid loc]
  (->> tools/grid-dirs
    (map (fn [[dir d]] [(map + d loc) dir]))
    (filter (fn [[new-loc dir]]
              (and
                (tools/in-bounds grid new-loc)
                (let [pipe (tools/at grid new-loc)]
                  (some #(= % (grids/opposite-dir dir)) (pipes pipe))))))))

(defn traverse [grid [loc dir]]
  (let [pipe (tools/at grid loc)
        next-dir (first (filterv #(not= % (grids/opposite-dir dir)) (pipes pipe)))
        next-loc (mapv + loc (tools/grid-dirs next-dir))]
    [next-loc next-dir]))

(defn find-loop [grid]
  (let [start (find-start grid)
        start-next (first (find-neighbours grid start))]
    (loop [path [] [loc _ :as curr] start-next]
      (if (= loc start)
        (conj path curr)
        (recur (conj path curr) (traverse grid curr))))))

(def part1
  (let [grid (map seq input)
        pipe-loop (tools/debug (find-loop grid))]
    (quot (count pipe-loop) 2)))

(defn flood
  "Returns the set of all connected points, given a grid and a seq of walls."
  [grid initial-loc walls]
  (loop [to-visit [initial-loc] visited #{}]
    (if-let [curr (peek to-visit)]
      (let [neighbours (->> (tools/neighbours grid curr)
                         (filter (fn [new-loc]
                                   (and
                                     (not (some #(contains? % new-loc) [walls visited]))
                                     (every? #(not= % new-loc) to-visit)))))
            new-visit (into (pop to-visit) neighbours)
            new-visited (conj visited (apply list curr))]
        (recur new-visit new-visited))
      visited)))

(defn turning-out?
  "Returns whether the path turns outwards (e.g. enclosing ccw, moving right and encounting a 7 is an outward turn). "
  [dir pipe cw?]
  (let [other-dir (first (filterv #(not= % (grids/opposite-dir dir)) (pipes pipe)))]
    (= (tools/turn dir (if cw? :left :right)) other-dir)))

;; Try to turn right (or left) at every step and dfs on the node to catch all connected squares.
;; If the path curves outwards, you also need to move forward and dfs on that node (catches edge cases).
(defn enclosed [grid cw?]
  (let [pipe-loop (find-loop grid)
        loop-locs (into #{} (map first pipe-loop))
        rotate-dir (if cw? :right :left)
        reducer (fn [visited [loc dir]]
                  (let [inside (map + loc (tools/grid-dirs (tools/turn dir rotate-dir)))
                        pipe (tools/at grid loc)
                        insides (if (and (some #(= % pipe) (seq "7JLF")) (turning-out? dir pipe cw?))
                                  [inside (map + loc (tools/grid-dirs dir))]
                                  [inside])
                        valid-insides (filter #(and
                                                 (tools/in-bounds grid %)
                                                 (not (contains? loop-locs %))
                                                 (not (contains? visited %))) insides)
                        flooded (map #(flood grid % loop-locs) valid-insides)]
                    (reduce clojure.set/union visited flooded)))]
    (reduce reducer #{} pipe-loop)))

(def part2
  (let [grid (map seq input)]
    (->> (map #(enclosed grid %) [true false])
      (filter #(not (contains? % [0 0])))
      first
      count)))


(defn -main [& args]
  (str part1 " " part2))

;; I leave this behind as a memory of my alternate solution that died to corners. RIP

; (defn inside [locs grid ind vert?]
;   (println (str "looking in " ind))
;   (loop [i 0 in? false total 0]
;     (if (= i (if vert? (count grid) (count (nth grid ind))))
;       total
;       (let [pipe? (contains? locs i)
;             char (tools/at grid (if vert? [ind i] [i ind]))
;             new-in? (if (and pipe? (= char (if vert? \- \|))) (not in?) in?)
;             new-total (if (and (not pipe?) in?) (inc total) total)]
;         (if (and pipe? (= char (if vert? \- \|))) (println (str "toggle " i)) ())
;         (if (and (not pipe?) in?) (println i) ())
;         (recur (inc i) new-in? new-total)))))
