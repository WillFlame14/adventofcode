(ns clojure-aoc.core (:gen-class)
  (:require clojure.string))

(defn parse-card [line]
  (let [[w c] (clojure.string/split line #" \| ")
        win-str (->> (drop-while #(not= % \:) w)
                  (drop-while #(not (Character/isDigit %)))
                  (apply str))
        wins (map #(Integer/parseInt %) (clojure.string/split win-str #" +"))
        chosen (->> (clojure.string/split c #" +")
                 (filter #(not= % ""))
                 (map #(Integer/parseInt %)))]
    [wins chosen]))

(defn num-wins [wins chosen]
  (let [wins-set (into #{} wins)]
    (count (filter #(contains? wins-set %) chosen))))

(defn points [wins chosen]
  (let [nwins (num-wins wins chosen)]
    (if (zero? nwins)
      0
      (int (Math/pow 2 (dec nwins))))))

(def part1
  (->> (clojure.string/split (slurp "input.txt") #"\n")
    (map parse-card)
    (map #(apply points %))
    (apply +)))

(defn reducer [[acc extras] [wins chosen]]
  (let [nwins (num-wins wins chosen)
        upd-extras (->> extras
                     (map (fn [[ts rng]] [ts (dec rng)]))
                     (filter (fn [[_ rng]] (not= rng 0))))
        ncopies (->> (map first extras)(apply +) inc)
        new-extras (if (zero? nwins)
                     upd-extras
                     (conj upd-extras [ncopies nwins]))]
    [(conj acc ncopies) new-extras]))

(def part2
  (->> (clojure.string/split (slurp "input.txt") #"\n")
    (map parse-card)
    (reduce reducer [[] []])
    first
    (apply +)))

(defn -main [& args]
  (str part1 " " part2))
