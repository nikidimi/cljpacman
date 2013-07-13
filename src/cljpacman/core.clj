(ns cljpacman.core
  (:use seesaw.core)
  (:use seesaw.graphics)
  (:use seesaw.keymap)
  (:use cljpacman.level)
  (:use cljpacman.engine))


(defn iterate-column
  [g x y column]
  (if (not (empty? column))
    (do 
      (if (false? (peek column)) 
        (draw g (line x y x y) (style :foreground :blue)))
      (iterate-column g x (- y 1) (pop column)))))

(defn iterate-field
  [g x field]
  (if (not (empty? field))
    (do
      (iterate-column g x (count (peek field)) (peek field))
      (iterate-field g (- x 1) (pop field)))))

(defn paint
  [c g background-image game-state field]
  (do
    (.drawImage g background-image 0 0 (count field) (count (peek field)) nil)
    (dorun (map #(.drawImage g (first (get % :images)) (first (get % :position)) (second (get % :position)) (first (get % :size)) (second (get % :size)) nil) game-state))))


(defn draw-background 
  [field]
  (let [image (java.awt.image.BufferedImage. (count field) (count (peek field)) java.awt.image.BufferedImage/TYPE_INT_ARGB)
        graphics (.createGraphics image)]
    (iterate-field graphics (count field) field)
    (.dispose graphics)
    image))

(defn game-loop
  [frame keyboard-control-atom background-image field initial-state]
  (future
    (loop [current-state initial-state]
      (case current-state
        true (alert "You win!")
        false (alert "You lose!")
        (let [canvas (select frame [:#canvas])]
          (do 
            (config! canvas :paint (fn
                                     [c g]
                                     (paint c g background-image (get-all-items current-state) field)))
            (repaint! canvas)
            (Thread/sleep 5)
            (recur (do-all-moves @keyboard-control-atom current-state field))))))))

(defn handle-key-press
  [keyboard-control-atom e]
  (let [key-code (.getKeyCode e)]
    (if (and (>= key-code 37) (<= key-code 40))
      (swap! keyboard-control-atom (fn [x] (mod (- key-code 34) 4))))))

(defn handle-key-release
  [keyboard-control-atom e]
  (swap! keyboard-control-atom (fn [x] 5)))



(defn show-level
  [level-name]
  (let [level (read-level level-name)
        game-state (generate-state level)
        keyboard-control-atom (atom 5)
        background-image (draw-background (get level :field))]
    (invoke-later
      (let [level-frame (frame :title "Hello",
                               :size [(first (get level :size)) :by (second (get level :size))]
                               :content
                               (vertical-panel
                                 :items [
                                         (canvas :id :canvas 
                                                 :background "#000000" 
                                                 :paint nil)]) 
                               :on-close :dispose)]
        (do
          (listen level-frame :key-pressed (fn [e] (handle-key-press keyboard-control-atom e)))
          (listen level-frame :key-released (fn [e] (handle-key-release keyboard-control-atom e)))
          (game-loop level-frame keyboard-control-atom background-image (get level :field) game-state)
          (show! level-frame))))))



  ;(read-level level-name))

(def levels-list (listbox :size [300 :by 300] :model (get-all-levels)))

(defn -main
  [& args]
  (invoke-later
    (-> (frame :title "Hello",
               :size [400 :by 400]
               :content 
                (vertical-panel
                  :items [
                          (label :text "Select level" :size [400 :by 30] :v-text-position :center)
                          levels-list
                          (button :text "Play!" :listen [:action (fn
                                                                   [x]
                                                                   (show-level (selection levels-list)))])])
               :on-close :exit)
        show!)))

