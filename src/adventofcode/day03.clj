(ns adventofcode.day03
  (:require [clojure.set]))

(defn- calculate-value [v]
  (let [ascii-value (int v)]
    (if (< ascii-value 97)
      (- ascii-value 38)
      (- ascii-value 96))))

(defn- parse-group [input]
  (first (apply clojure.set/intersection (map set input))))

(defn strategy1 [input]
  (map #(let [m (/ (count %) 2)
              first-part (subs % 0 m)
              second-part (subs % m (count %))]
          [first-part second-part])
       input))

(defn strategy2 [input]
  (partition 3 input))

(defn day03 [f input]
  (->> (clojure.string/split-lines input)
       f
       (map parse-group)
       (map calculate-value)
       (apply +)))
