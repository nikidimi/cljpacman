(ns cljpacman.level-test
  (:use clojure.test
        cljpacman.level))

(deftest fill-vector-test
  (let [empty-game-field (fill-vector [100 100])]
    (is (false? (get (get empty-game-field 100) 100)))
    (is (false? (get (get empty-game-field 0) 0)))
    (is (false? (get (get empty-game-field 0) 100)))
    (is (false? (get (get empty-game-field 100) 0)))
    (is (nil? (get (get empty-game-field 101) 0)))
    (is (nil? (get (get empty-game-field 0) 101)))))

(deftest parse-line-test
  (let [game-field (parse-line (fill-vector [100 100]) "20 30 40 50")]
    (is (true? (get (get game-field 20) 30)))
    (is (true? (get (get game-field 40) 30)))
    (is (true? (get (get game-field 20) 50)))
    (is (true? (get (get game-field 40) 50)))
    (is (false? (get (get game-field 19) 30)))
    (is (nil? (get (get game-field 101) 0)))
    (is (nil? (get (get game-field 0) 101)))))


(deftest generate-field-test
  (let [game-field (generate-field [100 100] ["20 30 40 40" "20 40 40 50"])]
    (is (true? (get (get game-field 20) 30)))
    (is (true? (get (get game-field 40) 30)))
    (is (true? (get (get game-field 20) 50)))
    (is (true? (get (get game-field 40) 50)))
    (is (false? (get (get game-field 19) 30)))
    (is (nil? (get (get game-field 101) 0)))
    (is (nil? (get (get game-field 0) 101)))))


(deftest read-file-test
  (let [level (read-level "../test/levels/level1")
        game-field (get level :field)]
    (is (= (get level :size) [800 800]))
    (is (= (get level :player-start-position [380 380])))
    (is (= (get level :enemies-start-positions [[10 80] [10 380] [10 680] [600 80] [600 680]])))
    (is (= (count (get level :coins-positions)) 21))
    (is (= (first (get level :coins-positions)) [10 100]))
    (is (= (last (get level :coins-positions)) [200 100]))
    (is (true? (get (get game-field 10) 390)))
    (is (true? (get (get game-field 10) 90)))
    (is (true? (get (get game-field 10) 690)))
    (is (true? (get (get game-field 380) 0)))
    (is (false? (get (get game-field 0) 130)))
    (is (nil? (get (get game-field 801) 0)))
    (is (nil? (get (get game-field 0) 801)))))

(deftest line-to-integer-seq-test
  (is (= (line-to-integer-seq "20 30 30 40 50 60") '(20 30 30 40 50 60))))

(deftest group-by-pairs-test
  (is (= (group-by-pairs [20 30 30 40 50 60]) [[20 30] [30 40] [50 60]])))

(deftest enemy-positions-test
  (is (= (parse-enemy-start-positions "20 30 30 40 50 60") [[20 30] [30 40] [50 60]])))




