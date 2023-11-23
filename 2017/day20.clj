(ns clojure-aoc.core (:gen-class)
    (:require [clojure-aoc.tools :as tools]
              clojure.string))

(def input (slurp "input.txt"))

(defn parse [line]
  (let [parts (clojure.string/split line #", ")
        parse_comps #(->> (re-matches #".=<(-?\d+),(-?\d+),(-?\d+)>" %)
                          rest
                          (map (fn [x] (Integer/parseInt x))))]
    (map parse_comps parts)))

(defn update-particle [[p v a]]
  (let [new_v (map + v a)
        new_p (map + p new_v)]
    [new_p new_v a]))

(def part1
  (let [particles (->> (clojure.string/split input #"\n") (map parse))
        states (iterate #(map update-particle %) particles)]
    (->> (nth states 1000)
         (map-indexed vector)
         (apply min-key #(apply + (map (fn [x] (Math/abs x)) (first (second %)))))
         first)))

(defn quad-roots [a b c]
  (cond
    (= a b c 0) :all
    (= a b 0) nil
    (= a 0) (list (- (/ c b)))
    :else (let [discrim (-> (Math/pow b 2) (- (* 4 a c)))]
            (cond
              (< discrim 0) nil
              (= discrim 0.0) (list (-> (- b) (/ (* 2 a))))
              :else (map #(-> (- b) (% (Math/sqrt discrim)) (/ (* 2 a))) [+ -])))))

(defn kin-form [[p v a]]
  (tools/transpose [(map #(/ % 2) a) (map #(+ %1 (/ %2 2)) v a) p]))

(defn intersect [p1 p2]
  (let [[k1 k2] (map kin-form [p1 p2])
        [xt yt zt] (->> (tools/transpose [k1 k2])
                        (map #(apply map - %))
                        (map #(apply quad-roots %))
                        (map #(if (= % :all) :all (vec %))))
        synced? (fn [ts t] (cond
                             (nil? ts) false
                             (= ts :all) true
                             :else (some #(== % t) ts)))
        test (or (first (filter #(not= :all %) [xt yt zt])) xt)]
    (cond
      (nil? test) nil
      (= test :all) '(0)
      :else (let [times (filter (fn [t] (every? #(synced? % t) [yt zt])) test)]
              (if (empty? times)
                nil
                (apply min times))))))

(defn get-collisions [particles]
  (let [len (count particles)]
    (->> (for [x (range len) y (range len)
               :while (< y x)
               :let [time (intersect (nth particles x) (nth particles y))]
               :when (and (not (nil? time))
                          (== (int time) time))]
           {:particles [x y] :time time}))))

(defn reducer [[particles time colliding] collision]
  (let [{col-ps :particles col-t :time} collision
        collided? (every? #(contains? particles %) col-ps)]
    (if collided?
      (if (== col-t time)
        [particles time (if collided? (into colliding col-ps) colliding)]
        (let [new_particles (apply disj particles colliding)]
          [new_particles col-t (into #{} col-ps)]))
      [particles time colliding])))

(defn resolve-collisions [particles]
  (let [collisions (get-collisions particles)
        sorted (sort-by :time collisions)
        particle-set (into #{} (range (count particles)))
        [rem-ps _ final-collide] (reduce reducer [particle-set 0 #{}] sorted)]
    (apply disj rem-ps final-collide)))

;; I couldn't figure out a bug in my solution (spoiler: using = instead of ==)
;; so I wrote a brute-force solution to help check my work.
;; I can't believe this is the intended solution.
(defn part2-hack [particles]
  (loop [i 0 ps particles]
    (if (< i 100)
      (let [new-ps (->> (map update-particle ps)
                        (group-by first)
                        (filter #(= (count (second %)) 1))
                        (map #(reduce into (vector) (second %))))]
        (recur (inc i) new-ps))
      (count ps))))

(def part2
  (let [particles (->> (clojure.string/split input #"\n") (map parse))
        remaining (resolve-collisions particles)]
    (count remaining)))

(defn -main []
  (println (str part1 " " part2)))
