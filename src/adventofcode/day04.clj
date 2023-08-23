(ns adventofcode.day04)

(defn- parse-line [input]
  (->> (clojure.string/split input #",")
       (map #(clojure.string/split % #"-"))
       flatten
       (map parse-long)))

(defn strategy1 [start1 end1 start2 end2]
  (or (and (<= start1 start2) (>= end1 end2))
      (and (<= start2 start1) (>= end2 end1))))

(defn strategy2 [start1 end1 start2 end2]
  (not (or (> start1 end2)
           (> start2 end1))))

(defn day04 [f input]
  (->> (clojure.string/split-lines input)
       (map parse-line)
       (filter #(apply f %))
       count))
