(ns clojure-aoc.core (:gen-class)
  (:require [clojure-aoc.tools :as tools]
            clojure.string))

(defn parse [line]
  (let [parts (clojure.string/split line #" ")
        name (first parts)
        entry {:weight (->> (second parts)
                            butlast
                            rest
                            (apply str)
                            Integer/parseInt)}]
    (if (= (count parts) 2)
      `(~name ~entry)
      (let [above (map #(clojure.string/replace % #"," "") (drop 3 parts))]
        `(~name ~(assoc entry :above above))))))

(def programs (->> (clojure.string/split (slurp "input.txt") #"\n")
                (map #(parse %))
                (reduce (fn [acc [name entry]] (assoc acc name entry )) {})))

(defn rebalance [name]
  (let [program (get programs name)
        above (get program :above)
        weight (get program :weight)]
    (if (nil? above)
      `(false ~weight)
      (let [results (map rebalance above)
            found (first (filter #(first %) results))]
        (if (nil? found)
          (let [weights (map second results)]
            (if (apply = weights)
              `(false ~(+ weight (reduce + weights)))
              (let [mode (tools/get_mode weights)
                    index (tools/findIndex weights #(not= mode %))
                    diff (- mode (nth weights index))
                    faulty (get programs (nth above index))
                    correct_weight (+ diff (get faulty :weight))]
                `(true ~correct_weight))))
          found)))))

  (defn -main
    [& args]
    (let [part1 "dtacyn" ;; just use inspection
          part2 (second (rebalance "dtacyn"))]
      (println (str part1 " " part2))))
