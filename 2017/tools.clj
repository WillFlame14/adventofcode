(ns clojure-aoc.tools)

(defn find-index [coll pred]
  (loop [i 0]
    (when (< i (count coll))
      (if (pred (nth coll i))
        i
        (recur (inc i))))))

(defn insert-at [coll value index]
  (let [[l r] (split-at index coll)]
    (concat l [value] r)))

(defn rotate [coll offset]
  (let [[l r] (split-at offset coll)]
    (concat r l)))

(defn swap [coll i j]
  (let [vec (into (vector) coll)]
    (-> vec (assoc i (vec j)) (assoc j (vec i)))))

(defn find-cycle [item f hash]
  (loop [i 1 curr (f item) visited {(hash item) 0}]
    (let [index (get visited curr)]
      (if (nil? index)
        (recur (inc i) (f curr) (assoc visited (hash curr) i))
        {:offset index
         :cycle (->> (filter #(>= (second %) index) visited)
                     (sort-by second <)
                     (map first))}))))

(defn get-mode [coll]
  (first (apply (partial max-key second) (seq (frequencies coll)))))

(def grid-dirs
  {:up [0 -1]
   :right [1 0]
   :down [0 1]
   :left [-1 0]})

(defn at [grid [x y]]
  (nth (nth grid y) x))

(defn transpose [m] 
  (apply mapv vector m))

(defn distance [p1 p2]
  (->> (transpose [p1 p2])
       (map #(Math/pow (apply - %) 2))
       (apply +)
       Math/sqrt))

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

(defn matrix-rotate-cw [matrix]
  (->> (transpose matrix) (map reverse)))

(defn matrix-flip [matrix]
  (map reverse matrix))

(defn square-partition [size grid]
  (->> grid
       (partition size)
       (map #(->> (transpose %)
                  (partition size)
                  (map transpose)))
       (apply concat)))
