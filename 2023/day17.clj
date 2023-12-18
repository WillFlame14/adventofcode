(ns clojure-aoc.core (:gen-class)
  (:require [clojure-aoc.grids :as grids]
    [clojure.data.priority-map :refer [priority-map]]
    clojure.string))

(def input (-> (slurp "input.txt") (clojure.string/split #"\n")))

;; We can make a small optimizaton in our dijkstra by not re-visiting locations with a smaller streak and heat value,
;; since that path is guaranteed to be better.

(defn traverse1 [grid]
  (let [end (map (comp dec count) [(nth grid 0) grid])]
    (loop [to-visit (priority-map [[0 0] :down 0 []] 0) best {[0 0] {:down {0 0}}} visited #{}]
      (when-let [[[loc dir streak path] heat] (peek to-visit)]
        (if (= loc end)
          (do (println path) heat)
          (let [neighbours (->> grids/grid-dirs
                             (filter (fn [[d _]] (every? #(not= d %) [(grids/opposite-dir dir) (if (= streak 3) dir nil)])))
                             (map (fn [[d c]] (vector (map + loc c) d)))
                             (filter (fn [[l _]] (grids/in-bounds grid l)))
                             (map (fn [[l d]] (vector [l d (if (= dir d) (inc streak) 1) (conj path l)] (+ heat (grids/at grid l)))))
                             (filter (fn [[[l d s _] h]]
                                       (and (not (contains? visited [l d s]))
                                         (if-let [loc-best (get-in best [l d])]
                                           (not (some (fn [[best-s best-h]] (and (<= best-s s) (<= best-h h))) loc-best))
                                           true)))))
                new-visit (into (pop to-visit) neighbours)
                new-best (reduce (fn [acc [[l d s _] h]] (assoc-in acc [l d s] h)) best neighbours)
                new-visited (conj visited [loc dir streak])]
            (recur new-visit new-best new-visited)))))))

(defn traverse2 [grid]
  (let [end (map (comp dec count) [(nth grid 0) grid])]
    (loop [to-visit (priority-map [[0 0] :down 0 []] 0) best {[0 0] {:down {0 0}}} visited #{}]
      (when-let [[[loc dir streak path] heat] (peek to-visit)]
        (if (and (= loc end) (> streak 3))
          (do (println path) heat)
          (let [neighbours (->> grids/grid-dirs
                             (filter (fn [[d _]]
                                       (cond
                                         (< 0 streak 4) (= d dir)
                                         (= streak 10) (every? #(not= % d) [(grids/opposite-dir dir) dir])
                                         :else (not= d (grids/opposite-dir dir)))))
                             (map (fn [[d c]] (vector (map + loc c) d)))
                             (filter (fn [[l _]] (grids/in-bounds grid l)))
                             (map (fn [[l d]] (vector [l d (if (= dir d) (inc streak) 1) (conj path l)] (+ heat (grids/at grid l)))))
                             (filter (fn [[[l d s _] h]]
                                       (and (not (contains? visited [l d s]))
                                         (or (< s 4)
                                           (if-let [loc-best (get-in best [l d])]
                                             (not (some (fn [[best-s best-h]] (and (<= best-s s) (<= best-h h))) loc-best))
                                             true))))))
                new-visit (into (pop to-visit) neighbours)
                new-best (reduce (fn [acc [[l d s _] h]] (assoc-in acc [l d s] h)) best (filter (fn [[[_ _ s _] _]] (> s 3)) neighbours))
                new-visited (conj visited [loc dir streak])]
            (recur new-visit new-best new-visited)))))))

(def part1
  (let [grid (->> (map seq input)
               (map (fn [line] (map #(- (int %) (int \0)) line))))]
    (traverse1 grid)))

(def part2
  (let [grid (->> (map seq input)
               (map (fn [line] (map #(- (int %) (int \0)) line))))]
    (traverse2 grid)))

(defn -main [& args]
  (println (str part1 " " part2)))
