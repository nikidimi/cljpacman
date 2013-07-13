(ns cljpacman.level
  (:gen-class))

(def levels-path "levels/")

(def level (create-struct :size :player-start-position :enemies-start-positions :coins-positions :field))

(defn line-to-integer-seq
  [line-string]
  (map #(Integer. %) (clojure.string/split line-string #" ")))

(defn get-all-levels
  "Get all files from the levels directory"
  []
  (->>
    (clojure.java.io/file "levels/")
    (file-seq)
    (filter #(.isFile %))
    (map #(.getName %))))

(defn parse-line
  [game-field line-string]
  (let [line (line-to-integer-seq line-string)]
    (reduce (fn 
              [curr-game-field x]
              (assoc curr-game-field
                     x
                     (reduce (fn 
                               [curr-line y]
                               (assoc curr-line y true))
                             (get curr-game-field x)
                             (range (second line) (+ 1 (second (rest (rest line))))))))
            game-field
            (range (first line) (+ 1 (first (rest (rest line))))))))



(defn fill-vector
  "Fills a vector with false values"
  [vector-size]
  (let [width (first vector-size)
        height (second vector-size)]
    (apply vector (take (+ width 1) 
                        (repeat (apply vector (take (+  height 1) (repeat false))))))))
(defn generate-field
  [field-size file-data]
  (reduce parse-line (fill-vector field-size) file-data))

(defn group-by-pairs
  [positions-list]
  (if (empty? positions-list)
    '()
    (cons (list (first positions-list) (second positions-list)) (group-by-pairs (rest (rest positions-list))))))

(defn parse-enemy-start-positions
  [file-data]
  (group-by-pairs (line-to-integer-seq file-data)))

(defn read-level 
  [level-name]
  (let [file-data (clojure.string/split (slurp (str levels-path level-name)) #"\n")
        field-size (line-to-integer-seq (first file-data))]
    (struct-map level 
                :size field-size 
                :player-start-position (line-to-integer-seq (second file-data))
                :enemies-start-positions (parse-enemy-start-positions (first (rest (rest file-data))))
                :coins-positions (parse-enemy-start-positions (first (rest (rest (rest file-data)))))
                :field (generate-field field-size (rest (rest (rest (rest file-data))))))))
