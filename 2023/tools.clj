(ns clojure-aoc.tools)

(defmacro debug [coll]
  `(do (println ~coll) ~coll))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll] (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defn find-index [pred coll]
  (->> (map-indexed vector coll)
    (filter #(pred (second %)))
    first
    first))

(defn insert-at [coll value index]
  (let [[l r] (split-at index coll)]
    (concat l [value] r)))

(defn vec-remove
  "remove elem in coll"
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn rotate [coll offset]
  (let [[l r] (split-at offset coll)]
    (concat r l)))

(defn swap [coll i j]
  (let [vec (into (vector) coll)]
    (-> vec (assoc i (vec j)) (assoc j (vec i)))))

(defn find-cycle [item f hash]
  (loop [i 1 curr (f item) visited {(hash item) 0}]
    (let [index (get visited (hash curr))]
      (if (nil? index)
        (recur (inc i) (f curr) (assoc visited (hash curr) i))
        {:offset index
         :cycle (->> (filter #(>= (second %) index) visited)
                  (sort-by second <)
                  (map first))}))))

(defn get-mode [coll]
  (first (apply (partial max-key second) (seq (frequencies coll)))))

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

(defn partition-with
  [f coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (let [run (cons (first s) (take-while (complement f) (next s)))]
        (cons run (partition-with f (seq (drop (count run) s))))))))

(defn split-all [pred items]
  (when (seq items)
    (apply conj (reduce (fn [[acc curr] x]
                          (if (pred x)
                            [(conj acc curr) []]
                            [acc (conj curr x)]))
                  [[] []] items))))

(defn split-including [pred items]
  (when (seq items)
    (butlast (apply conj (reduce (fn [[acc curr with?] x]
                                   (if with?
                                     (if (pred x)
                                       [acc (conj curr x) true]
                                       [(conj acc curr) [x] false])
                                     (if (pred x)
                                       [(conj acc curr) [x] true]
                                       [acc (conj curr x) false])
                                     ))
                           [[] [] false] items)))))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm_helper [a b]
  (/ (* a b) (gcd a b)))

(defn lcm [& vals]
  (reduce lcm_helper vals))
