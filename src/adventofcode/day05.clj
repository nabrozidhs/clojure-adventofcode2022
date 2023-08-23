(ns adventofcode.day05)

(defn- parse-stacks [stacks]
  (let [num-stacks (/ (inc (count (first stacks))) 4)
        num-rows (dec (count stacks))]
    (loop [y (dec num-rows)
           x 1
           result (zipmap (range 1 (inc num-stacks)) (repeat []))]
      (cond
        (< y 0) result
        (> x num-stacks) (recur (dec y) 1 result)
        :else (let [element (get (stacks y) (inc (* (dec x) 4)))]
                (if (= element \space)
                  (recur y (inc x) result)
                  (recur y (inc x) (assoc result x (conj (result x) element)))))))))

(defn- parse-command [command]
  (let [m (re-matcher #"(\d+)" command)]
    {:count (parse-long (first (re-find m)))
     :from  (parse-long (first (re-find m)))
     :to    (parse-long (first (re-find m)))}))

(defn- parse-groups [stacks commands]
  {:stacks   (parse-stacks (clojure.string/split-lines stacks))
   :commands (map parse-command (clojure.string/split-lines commands))})

(defn- evaluate [f {:keys [stacks commands]}]
  (reduce f stacks commands))

(defn- calculate-result [stack]
  (apply str (map #(peek (stack %)) (range 1 (inc (count stack))))))

(defn strategy1 [stack command]
  (let [from (:from command)
        to (:to command)]
    (loop [i (:count command)
           result stack]
      (if (= i 0)
        result
        (recur (dec i)
               (assoc result from (pop (result from))
                      to (conj (result to) (peek (result from)))))))))

(defn strategy2 [stack command]
  (let [from (:from command)
        to (:to command)]
    (loop [result stack
           temp-stack (seq (subvec (stack from)
                                   (- (count (stack from)) (:count command))))]
      (if (empty? temp-stack)
        result
        (recur (assoc result to (conj (result to) (first temp-stack))
                      from (pop (result from)))
               (rest temp-stack))))))

(defn day05 [f input]
  (->> (clojure.string/split input #"\n\n")
       (apply parse-groups)
       (evaluate f)
       calculate-result))
