(ns adventofcode.day07)

(defn- parse [input]
  (loop [result {}
         dir '()
         lines (drop 1 input)]
    (if (empty? lines)
      result
      (let [line (first lines)
            split (clojure.string/split line #" ")]
        (cond
          (= "$ cd .." line) (recur result (rest dir) (rest lines))
          (clojure.string/starts-with? line "$ cd") (recur result (cons (last split) dir) (rest lines))
          (= "$ ls" line) (recur result dir (rest lines))
          (clojure.string/starts-with? line "dir") (recur (assoc result dir
                                                                 (assoc (result dir {}) :directories
                                                                        (cons (last split) (:directories (result dir {}) '()))))
                                                          dir
                                                          (rest lines))
          :else (recur (assoc result dir
                              (assoc (result dir {}) :files
                                     (assoc (:files (result dir) {}) (second split) (parse-long (first split)))))
                       dir
                       (rest lines)))))))

(defn calculate-directory-sizes [input dir]
  (let [v (input dir)]
    (+ (apply + (vals (:files v {})))
       (apply + (map #(calculate-directory-sizes input (cons % dir)) (:directories v '()))))))

(defn strategy1 [sizes]
  (apply + (filter #(>= 100000 %) sizes)))

(defn strategy2 [sizes]
  (let [unused (- 70000000 (apply max sizes))
        required (- 30000000 unused)]
    (->> (drop-while #(> required %) sizes)
         first)))

(defn day07 [f input]
  (->> (clojure.string/split-lines input)
       parse
       ((fn [tree] (map #(calculate-directory-sizes tree %) (keys tree))))
       sort
       f))
