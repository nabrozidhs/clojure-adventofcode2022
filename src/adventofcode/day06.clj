(ns adventofcode.day06)

(defn find-marker [n input]
  (->> (partition n 1 input)
       (map vector (range))
       (filter #(= (count (set (get % 1))) n))
       first
       first
       (+ n)))

(def strategy1 4)

(def strategy2 14)

(defn day06 [n input]
  (find-marker n input))
