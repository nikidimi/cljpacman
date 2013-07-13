(ns cljpacman.engine
  (:gen-class))

(def movable-item (create-struct :position :size :images :move-direction :last-data :move-fun))

(def game-state (create-struct :player :enemies :coins))

(defn matrix-get
  [matrix x y]
  (let [value (get (get matrix x) y)]
    (if (nil? value)
      true
      value)))

(defn is-legal-position?
  [field position size]
  (and
    (matrix-get field (first position) (second position))
    (matrix-get field (+ (first size) (first position)) (second position))
    (matrix-get field (first position) (+ (second size) (second position)))
    (matrix-get field (+ (first size) (first position)) (+ (second size) (second position)))))

(defn calc-coordinates
  [field position move-direction]
  (case move-direction
    0 (list (mod (+ (first position) 0) (count field)) (mod (+ (second position) -1) (count (first field))))
    1 (list (mod (+ (first position) 1) (count field)) (mod (+ (second position) 0) (count (first field))))
    2 (list (mod (+ (first position) 0) (count field)) (mod (+ (second position) 1) (count (first field))))
    3 (list (mod (+ (first position) -1) (count field)) (mod (+ (second position) 0) (count (first field))))))

(defn empty-move
  [keyboard-control field position size old-direction]
  (list old-direction position))

(defn simple-move
  [keyboard-control field position size old-direction]
  (let [new-position (calc-coordinates field position old-direction)]
    (if (is-legal-position? field new-position size)
      (list old-direction new-position)
      (list old-direction position))))


(defn player-move
  [keyboard-control field position size old-direction]
  (if (< keyboard-control 5)
    (let [new-position (calc-coordinates field position keyboard-control)]
      (if (is-legal-position? field new-position size)
        (list keyboard-control new-position)
        (simple-move keyboard-control field position size old-direction)))
    (simple-move keyboard-control field position size old-direction)))


(defn enemy-move
  [keyboard-control field position size old-direction]
  (let [opposite-direction (mod (+ 2 old-direction) 4)
        possible-moves (->>
                         (filter #(not (= % opposite-direction)) '(0 1 2 3))
                         (map #(list % (calc-coordinates field position %)))
                         (filter #(is-legal-position? field (second %) size)))]
    (if (empty? possible-moves)
      (if (is-legal-position?  field (calc-coordinates field position opposite-direction) size )
        (list opposite-direction (calc-coordinates field position opposite-direction))
        (list 0 position))
      (nth possible-moves (rand-int (count possible-moves))))))




(defn generate-enemy
  [enemy-start-position]
  (struct-map movable-item
              :move-direction 1
              :position enemy-start-position
              :size [40 40]
              :images (list (javax.imageio.ImageIO/read (clojure.java.io/as-file "images/ghost.png")))
              :move-fun enemy-move))


(defn generate-player
  [player-start-position]
  (struct-map movable-item
              :move-direction 1
              :position player-start-position
              :size [40 40]
              :images (list (javax.imageio.ImageIO/read (clojure.java.io/as-file "images/pacman.png")))
              :move-fun player-move))

(defn generate-coin
  [coin-position]
  (struct-map movable-item
              :move-direction 1
              :position coin-position
              :size [10 10]
              :images (list (javax.imageio.ImageIO/read (clojure.java.io/as-file "images/coin.png")))
              :move-fun empty-move))

(defn get-all-items 
  [game-state]
  (->>
    (get game-state :enemies)
    (concat (get game-state :coins))
    (cons (get game-state :player))))

(defn get-all-movable-items 
  [game-state]
  (->>
    (get game-state :enemies)
    (concat (get game-state :coins))
    (cons (get game-state :player))))

(defn does-collide?
  [first-item second-item]
  (let [first-position (get first-item :position)
        first-size (get first-item :size)
        second-position (get second-item :position)
        second-size (get second-item :size)]
    (and 
      (< (first first-position) (+ (first second-position) (first second-size)))
      (> (+ (first first-position) (first first-size)) (first second-position))
      (< (second first-position) (+ (second second-position) (second second-size)))
      (> (+ (second first-position) (second first-size)) (second second-position)))))

(defn check-collisions
  [current-state]
  (if (some #(does-collide? (get current-state :player) %) (get current-state :enemies))
    false
    (let [new-coins (filter #(not (does-collide? (get current-state :player) %)) (get current-state :coins))]
      (if (empty? (get current-state :coins))
        true
        (assoc current-state :coins new-coins)))))


(defn do-move
  [keyboard-control movable-item field]
  (let [move-result ((get movable-item :move-fun)
                     keyboard-control
                     field
                     (get movable-item :position)
                     (get movable-item :size)
                     (get movable-item :move-direction))]
    (->
      (assoc movable-item :position (second move-result))
      (assoc :move-direction (first move-result)))))

(defn do-all-moves
  [keyboard-control current-state field]
  (->
    (assoc current-state :player (do-move keyboard-control (get current-state :player) field))
    (assoc :enemies (map #(do-move keyboard-control % field) (get current-state :enemies)))
    (check-collisions)))

(defn get-position
  [movable-item]
  (get movable-item :position))

(defn generate-state
  [field]
  (struct-map game-state
              :player (generate-player (get field :player-start-position))
              :enemies (map generate-enemy (get field :enemies-start-positions))
              :coins (map generate-coin (get field :coins-positions))))


