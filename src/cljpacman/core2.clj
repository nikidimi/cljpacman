(ns cljpacman.core
  (:use seesaw.core)
  (:use seesaw.graphics)
  (:use cljpacman.level)
  (:use cljpacman.engine))



(defn iterate-column
  [g x y column]
  (if (not (empty? column))
    (if (false? (peek column))
      (println x y))
      ;(draw g (line x y x y) (style :foreground :blue)))
    (iterate-column g x (- y 1) (pop column))))

(defn iterate-field
  [g x field]
  (if (not (empty? field))
    (do
      (iterate-column g x (count (peek field)) (peek field))
      (iterate-field g (- x 1) (pop field)))))

(defn paint
  [c g field]
  (iterate-field g (count field) field))


(defn show-level
  [level-name]
  (let [level (read-level level-name)]
    (invoke-later
      (-> (frame :title "Hello",
                 :size [(first (get level :size)) :by (second (get level :size))]
                 :content
                 (vertical-panel
                   :items [
                           (canvas :id :canvas :background "#BBBBDD" :paint (fn
                                                                              [c g]
                                                                              (paint c g (get level :field))))])
                 :on-close :exit)
          show!))))



  ;(read-level level-name))

(def levels-list (listbox :size [300 :by 300] :model (get-all-levels)))

(defn -main
  "I don't do a whole lot ... yet."
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

