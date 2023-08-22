(ns adventofcode.day01)

(defn day01
  ([input] (day01 input 1))
  ([input n]
   (->> (clojure.string/split input #"\n\n")
        (map #(->> (clojure.string/split-lines %)
                   (map parse-long)
                   (apply +)))
        sort
        (take-last n)
        (apply +))))
