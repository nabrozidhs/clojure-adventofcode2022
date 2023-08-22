(ns adventofcode.day02)

(def win-map
  {:rock     :scissors
   :paper    :rock
   :scissors :paper})

(defn- result [left right]
  (cond
    (= left right) :draw
    (= (left win-map) right) :left
    :else :right))

(defn- calculate-score [left right]
  (+ (case (result left right)
       :draw 3
       :right 6
       :left 0)
     (case right
       :rock 1
       :paper 2
       :scissors 3)))

(defn- parse-hand [input]
  (case input
    ("A" "X") :rock
    ("B" "Y") :paper
    ("C" "Z") :scissors))

(defn- parse-action [action]
  (case action
    "Y" :tie
    "X" :lose
    "Z" :win))

(defn- find-hand [result opponent-hand]
  (case result
    :tie opponent-hand
    :lose (opponent-hand win-map)
    :win ((opponent-hand win-map) win-map)))

(defn- parse-line [f input]
  (->> (clojure.string/split input #" ")
       f
       (apply calculate-score)))

(defn strategy1 [input]
  (map parse-hand input))

(defn strategy2 [input]
  (let [left (parse-hand (first input))
        right (find-hand (parse-action (last input)) left)]
    [left right]))

(defn day02 [f input]
  (->> (clojure.string/split-lines input)
       (map #(parse-line f %))
       (apply +)))
