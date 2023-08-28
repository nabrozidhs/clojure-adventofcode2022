(ns adventofcode.day08)

(defn- parse-line [line]
  (map #(parse-long (str %)) line))

(defn build-map [lines]
  (loop [y 0
         result {}]
    (if (>= y (count lines))
      {:board result, :height (count lines), :width (count (first lines))}
      (let [line (lines y)]
        (recur (inc y) (merge result (zipmap (map list (range (count line)) (repeat y))
                                             (parse-line line))))))))

(defn visible? [board height width x y]
  (let [v (board (list x y))
        row-start (map list (range x) (repeat y))
        row-end (map list (range (inc x) width) (repeat y))
        column-start (map list (repeat x) (range y))
        column-end (map list (repeat x) (range (inc y) height))]
    (->> (list row-start row-end column-start column-end)
         (some #(every? (fn [z] (> v (board z))) %)))))

(defn scenic-score [board height width x y]
  (let [v (board (list x y))
        row-start (map list (reverse (range x)) (repeat y))
        row-end (map list (range (inc x) width) (repeat y))
        column-start (map list (repeat x) (reverse (range y)))
        column-end (map list (repeat x) (range (inc y) height))]
    (->> (list row-start row-end column-start column-end)
         (map #(apply + (let [[in-view outside] (split-with (fn [z] (> v (board z))) %)]
                          (list (count in-view) (if (empty? outside) 0 1)))))
         (apply *))))

(defn strategy1 [{:keys [board height width]}]
  (->> (for [x (range width)
             y (range height)]
         (visible? board height width x y))
       (filter identity)
       count))

(defn strategy2 [{:keys [board height width]}]
  (->> (for [x (range 1 (dec width))
             y (range 1 (dec height))]
         (scenic-score board height width x y))
       (apply max)))

(defn day08 [f input]
  (->> (clojure.string/split-lines input)
       build-map
       f))
