(ns cljpacman.engine-test
  (:use clojure.test
        cljpacman.level
        cljpacman.engine))

(deftest matrix-get-test
  (let [matrix [[1 2] [3 4] [5 6]]]
    (is (= (matrix-get matrix 0 0) 1))
    (is (= (matrix-get matrix 1 1) 4))
    (is (= (matrix-get matrix 2 1) 6))
    (is (= (matrix-get matrix 1 0) 3))
    (is (true? (matrix-get matrix 5 0)))
    (is (true? (matrix-get matrix 0 5)))))

(deftest legal-positions-test
  (let [level (read-level "../test/levels/level1")
        field (get level :field)]
    (is (true? (is-legal-position? field [10 80] [40 40])))
    (is (false? (is-legal-position? field [10 81] [40 40])))
    (is (false? (is-legal-position? field [10 80] [40 41])))
    (is (true? (is-legal-position? field [380 800] [40 40])))))

(deftest calc-coordinates-test
  (let [level (read-level "../test/levels/level1")
        field (get level :field)]
    (is (= (calc-coordinates field [20 20] 0) [20 19]))
    (is (= (calc-coordinates field [20 20] 1) [21 20]))
    (is (= (calc-coordinates field [20 20] 2) [20 21]))
    (is (= (calc-coordinates field [20 20] 3) [19 20]))
    (is (= (calc-coordinates field [800 20] 1) [0 20]))
    (is (= (calc-coordinates field [20 800] 2) [20 0]))))

(deftest empty-move-test
  (let [level (read-level "../test/levels/level1")
        field (get level :field)]
    (is (= (empty-move 5 field [20 20] [40 40] 2) [2 [20 20]]))))


(deftest simple-move-test
  (let [level (read-level "../test/levels/level1")
        field (get level :field)]
    (is (= (simple-move 5 field [10 80] [40 40] 1) [1 [11 80]]))
    (is (= (simple-move 5 field [10 80] [40 40] 2) [2 [10 80]])))) 

(deftest player-move-test
  (let [level (read-level "../test/levels/level1")
        field (get level :field)]
    (is (= (player-move 5 field [10 80] [40 40] 1) [1 [11 80]]))
    (is (= (player-move 5 field [10 80] [40 40] 2) [2 [10 80]]))
    (is (= (player-move 1 field [10 80] [40 40] 1) [1 [11 80]]))
    (is (= (player-move 2 field [10 80] [40 40] 1) [1 [11 80]]))
    (is (= (player-move 3 field [10 80] [40 40] 1) [3 [9 80]]))))
    

(deftest enemy-move-test
  (let [level (read-level "../test/levels/level1")
        field (get level :field)]
    (is (= (enemy-move 5 field [10 80] [40 40] 1) [1 [11 80]]))
    (let [enemy-move-result (enemy-move 5 field [10 80] [40 40] 2)]
      (is (or (= enemy-move-result [1 [11 80]])
              (= enemy-move-result [3 [9 80]]))))
    (let [enemy-move-result-intersection (enemy-move 5 field [380 380] [40 40] 2)]
      (is (or (= enemy-move-result-intersection [1 [381 380]])
              (= enemy-move-result-intersection [3 [379 380]])
              (= enemy-move-result-intersection [2 [380 381]]))))))

(deftest does-collide-test
 (is (true? (does-collide? {:position [20 20] :size [10 10]} {:position [25 25] :size [10 10]})))
 (is (true? (does-collide? {:position [20 20] :size [10 10]} {:position [15 15] :size [10 10]})))
 (is (false? (does-collide? {:position [20 20] :size [10 10]} {:position [10 15] :size [10 10]})))
 (is (false? (does-collide? {:position [20 20] :size [10 10]} {:position [35 35] :size [10 10]})))
 (is (false? (does-collide? {:position [20 20] :size [10 10]} {:position [30 30] :size [10 10]}))))

(deftest do-check-collisions-lose-test
  (let [level (read-level "../test/levels/level1")
        field (get level :field)
        game-state (generate-state level)
        player (get game-state :player)
        new-player-state (assoc player :position [10 80])
        new-game-state (assoc game-state :player new-player-state)]
    (is (map? (check-collisions game-state)))
    (is (false? (check-collisions new-game-state)))))

(deftest do-check-collisions-win-test
  (let [level (read-level "../test/levels/level1")
        field (get level :field)
        game-state (generate-state level)
        new-game-state (assoc game-state :coins [])]
    (is (map? (check-collisions game-state)))
    (is (true? (check-collisions new-game-state)))))


(deftest do-move-test
  (let [level (read-level "../test/levels/level1")
        field (get level :field)
        game-state (generate-state level)
        player (get game-state :player)]
    (is (= (get (do-move 0 player field) :position) [380 379]))
    (is (= (get (do-move 1 player field) :position) [381 380]))
    (is (= (get (do-move 2 player field) :position) [380 381]))
    (is (= (get (do-move 3 player field) :position) [379 380]))))


(deftest generate-state-test
  (let [level (read-level "../test/levels/level1")
        game-state (generate-state level)]
    (let [player-state (get game-state :player)]
      (is (= (get player-state :position) [380 380]))
      (is (= (get player-state :size) [40 40]))
      (is (= (get player-state :move-direction) 1))
      (is (not (empty? (get player-state :images)))))
    (let [enemies-state (get game-state :enemies)]
      (is (= (count enemies-state) 5))
      (is (= (get (first enemies-state) :position)) [10 180])
      (is (= (get (first enemies-state) :size)) [40 40])
      (is (= (get (first enemies-state) :move-direction)) 1)
      (is (not (empty? (get (first enemies-state) :images)))
      (is (= (get (last enemies-state) :position)) [60 680]))
      (is (= (get (last enemies-state) :size)) [40 40])
      (is (= (get (last enemies-state) :move-direction)) 1)
      (is (not (empty? (get (last enemies-state) :images)))))
    (let [coins-state (get game-state :coins)]
      (is (= (count coins-state) 21))
      (is (= (get (first coins-state) :position)) [10 100])
      (is (= (get (first coins-state) :size)) [10 10])
      (is (not (empty? (get (first coins-state) :images)))
      (is (= (get (last coins-state) :position)) [200 100]))
      (is (= (get (last coins-state) :size)) [10 10])
      (is (not (empty? (get (last coins-state) :images)))))))

