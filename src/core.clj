(ns cleanbot.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.math :refer :all]))

(declare cleanbot-game main-screen)

(def col# 22) ;; Number of columns
(def row# 22) ;; Number of rows
(def tile-size 50) ;; Pixel length of a single tile edge
(def time-quantum 0.06) ;; Time interval of a single move (in seconds)
(def unvisited-val 0)
(def visited-val 2)
(def obs-val 1)
(def init-pos [1 1]) ;; Initial position
(def obs# 100) ;; Number of randomized obstacles
(def obs-list
   (partition 2
              (interleave
                (repeatedly obs#  #(inc (rand-int (- col# 2)))) (repeatedly obs#  #(inc (rand-int (- row# 2))))
              )
   )
)

;; Sample Map (col = 20 row = 10)
;; (def obs# 13)
;; (def obs-list [[6 4] [6 5] [5 4] [5 5] [11 5] [11 6]
;;                [12 3] [12 6] [13 3] [13 6] [14 3] [14 4] [14 5]])

(defn- map-covered? [map-vector]
  (not (some true? (map (partial some #(= unvisited-val %)) (map vec map-vector))))
  )

(defn- tile-value [pos full-map direction]
  (let [tile-post (case direction
                    :up    [(first pos) (inc (second pos))]
                    :down  [(first pos) (dec (second pos))]
                    :left  [(dec (first pos)) (second pos)]
                    :right [(inc (first pos)) (second pos)])
        ]
    (aget full-map (- (dec row#) (second tile-post)) (first tile-post))
  )
)

(defn- obstacle? [pos full-map direction]
  (if (= obs-val (tile-value pos full-map direction))
    true
    false
    )
  )

(defn- unvisited? [pos full-map direction]
  (if (= unvisited-val (tile-value pos full-map direction))
    true
    false
    )
  )

(defn- visited-tile? [map-vector pos direction]
  (if (< obs-val (tile-value pos map-vector direction)) ;; WARN
    true
    false
    )
  )

(defn- exists? [sweep-vector-list sweep-vector]
  (some #(= sweep-vector %) sweep-vector-list)
  )

(defn- check-state [{:keys [robot? direction pos map-vector] :as entity}]
  (if robot?
    (case direction
      :up (if (obstacle? pos map-vector :up)
            (assoc entity :unvisited-up-tiles-remain? false)
            entity)
      :down (if (obstacle? pos map-vector :down)
              (assoc entity :unvisited-down-tiles-remain? false)
              entity)
      :left (assoc entity :unvisited-down-tiles-remain? (unvisited? pos map-vector :down)
                            :unvisited-up-tiles-remain? (unvisited? pos map-vector :up))
      :right  (assoc entity :unvisited-down-tiles-remain? (unvisited? pos map-vector :down)
                            :unvisited-up-tiles-remain? (unvisited? pos map-vector :up))
    )
    entity
  )
)

(defn- visit-tile [current_pos {:keys [robot? back? direction pos map-vector cell-label] :as entity}]
    (cond
    robot? (assoc entity
             :map-vector (aset map-vector (- (dec row#) (second pos)) (first pos) cell-label)
             )
    back?  (if (= pos current_pos)
             (texture! entity :set-texture (texture! (texture "clean_tile_wood.jpg") :get-texture))
             )
    :else entity
    )
  entity
)

(defn- visit-count [pos full-map {:keys [text? visit-counter revisit-counter] :as entity}]
  (if text?
    (if (= visited-val (aget full-map (- (dec row#) (second pos)) (first pos)))
      (assoc entity :revisit-counter (inc revisit-counter))
      (if (= unvisited-val (aget full-map (- (dec row#) (second pos)) (first pos)))
       (assoc entity :visit-counter (inc visit-counter))
        entity)
    )
    entity
  )
)

(defn- get-sweep-vec [map-vector pos direction]
  [(tile-value pos map-vector direction) (case direction :right (inc (first pos)) :left (dec (first pos))) (second pos)]
  )

(defn- unvisited-left? [sweep-vector]
  (some #(= unvisited-val %) (map first sweep-vector))
  )

(defn- go-to-cell [{:keys [pos map-vector back-loop-list] :as entity} unvisited-cell-list]
 (let [map-vec (to-array-2d (map vec map-vector))]
    (def target-pos (drop 1 (last unvisited-cell-list)))
    (def step (atom 100))
    (aset map-vec (- (dec row#) (second pos)) (first pos) @step)
    (while (= (aget map-vec (- (dec row#) (second target-pos)) (first target-pos)) visited-val)
      (do
        (doseq [idx1 (range row#) idx2 (range col#)]
          (if (= (aget map-vec (- (dec row#) idx1) idx2) @step)
            (do
              (if (= (aget map-vec (- (dec row#) (inc idx1)) idx2) visited-val) (aset map-vec (- (dec row#) (inc idx1)) idx2 (inc @step)))
              (if (= (aget map-vec (- (dec row#) (dec idx1)) idx2) visited-val) (aset map-vec (- (dec row#) (dec idx1)) idx2 (inc @step)))
              (if (= (aget map-vec (- (dec row#) idx1) (inc idx2)) visited-val) (aset map-vec (- (dec row#) idx1) (inc idx2) (inc @step)))
              (if (= (aget map-vec (- (dec row#) idx1) (dec idx2)) visited-val) (aset map-vec (- (dec row#) idx1) (dec idx2) (inc @step)))
              )
            )
          )
        (swap! step inc)
        )
      )
     (def xstep (atom (first target-pos)))
     (def ystep (atom (second target-pos)))
     (def move-list (atom []))
     (swap! step dec)
     (while (> @step 99)
       (do
         (cond
              (= (aget map-vec (- (dec row#) (inc @ystep)) @xstep) @step) (do (swap! ystep inc) (swap! move-list (partial cons :down)))
              (= (aget map-vec (- (dec row#) (dec @ystep)) @xstep) @step) (do (swap! ystep dec) (swap! move-list (partial cons :up)))
              (= (aget map-vec (- (dec row#) @ystep) (inc @xstep)) @step) (do (swap! xstep inc) (swap! move-list (partial cons :left)))
              (= (aget map-vec (- (dec row#) @ystep) (dec @xstep)) @step) (do (swap! xstep dec) (swap! move-list (partial cons :right)))
          )
         (swap! step dec)
        )
      )
    (assoc entity :sweep-mode :back-loop :back-loop-list @move-list :unvisited-cells unvisited-cell-list)
    )
  )

(defn- get-unvisited-cell-pos [{:keys [pos map-vector sweep-left sweep-right unvisited-cells] :as entity}] ;; TODO sağ ve sol sweepleri birleştir !! sonra ziyaret ettikçe sil.
  (apply
    (partial conj
             (apply (partial conj unvisited-cells)
                    (map (fn [vec] [(first vec) (dec (second vec)) (last vec)])
                         (filter #(= unvisited-val (first %)) (map first (partition-by first sweep-right))))))
    (into [] (map (fn [vec] [(first vec) (inc (second vec)) (last vec)])
                           (filter #(= unvisited-val (first %)) (map first (partition-by first sweep-left))))))
)

(defn- clear-unvisited-cell-list [full-map cell-list]
  (into []
    (cons [:bottom]
    (filterv
          #(or (unvisited? [(second %) (last %)] full-map :left) (unvisited? [(second %) (last %)] full-map :right))
          (drop 1 cell-list)
  )))
)

(defn- cell-decomposition [{:keys [robot? sweep-right sweep-left unvisited-cells] :as entity}]
  (if robot?
    (if (and (not (:unvisited-up-tiles-remain? entity)) (not (:unvisited-down-tiles-remain? entity)))
      (if (unvisited-left? sweep-left)
        (assoc entity :sweep-mode :left :unvisited-cells (get-unvisited-cell-pos entity))
        (if (unvisited-left? sweep-right)
          (assoc entity :sweep-mode :right :unvisited-cells (get-unvisited-cell-pos entity))
          (if (= :bottom (last (flatten (clear-unvisited-cell-list (:map-vector entity) unvisited-cells))))
            (assoc entity :sweep-mode :right)
            (go-to-cell entity (clear-unvisited-cell-list (:map-vector entity) unvisited-cells)))))
      entity)
    entity)
  )

(defn- move-robot [direction {:keys [robot? pos map-vector sweep-right sweep-left] :as entity}]
 (if robot?
   (let [
       new-x  (case direction
                  :right (+ (:x entity) tile-size)
                  :left (- (:x entity) tile-size)
                  :up (:x entity)
                  :down (:x entity))
       new-y  (case direction
                   :right (:y entity)
                   :left (:y entity)
                   :up (+ (:y entity) tile-size)
                   :down (- (:y entity) tile-size))
       new-texture (case direction
                     :right 270
                     :left 90
                     :up 0
                     :down 180
                     )
       new-pos (case direction
                    :up    [(first pos) (inc (second pos))]
                    :down  [(first pos) (dec (second pos))]
                    :left  [(dec (first pos)) (second pos)]
                    :right [(inc (first pos)) (second pos)])
       new-sweep-right (case direction
                          :up (if (exists? sweep-right (get-sweep-vec map-vector new-pos :right))
                                     sweep-right
                                     (conj sweep-right (get-sweep-vec map-vector new-pos :right)))
                          :down (if (exists? sweep-right (get-sweep-vec map-vector new-pos :right))
                                  sweep-right
                                   (into [] (cons (get-sweep-vec map-vector new-pos :right) sweep-right)))
                          :right [(get-sweep-vec map-vector new-pos :right)]
                          :left [(get-sweep-vec map-vector new-pos :right)])
       new-sweep-left (case direction
                          :up (if (exists? sweep-left (get-sweep-vec map-vector new-pos :left))
                                     sweep-left
                                      (conj sweep-left (get-sweep-vec map-vector new-pos :left)))
                          :down (if (exists? sweep-left (get-sweep-vec map-vector new-pos :left))
                                  sweep-left
                                  (into [] (cons (get-sweep-vec map-vector new-pos :left) sweep-left)))
                          :right [(get-sweep-vec map-vector new-pos :left)]
                          :left [(get-sweep-vec map-vector new-pos :left)])

       ]
     (assoc entity :x new-x :y new-y
       :pos new-pos :angle new-texture :direction direction
       :sweep-right new-sweep-right :sweep-left new-sweep-left)
      )
   entity)
  )

(defn- motion-planning [{:keys [robot? direction pos map-vector back-loop-list] :as entity}]
  (if robot?
   (let [
       next-move  (if (or (= :left (:sweep-mode entity)) (= :right (:sweep-mode entity)))
                        (case direction
                        :up (if (:unvisited-up-tiles-remain? entity)
                              :up
                              (if (:unvisited-down-tiles-remain? entity)
                                :down
                                (if (obstacle? pos map-vector (:sweep-mode entity))
                                  (if (obstacle? pos map-vector :up)
                                    :down
                                    :up)
                                  (if (visited-tile? map-vector pos (:sweep-mode entity))
                                    (if (obstacle? pos map-vector :up)
                                    :down
                                    :up)
                                    (:sweep-mode entity))
                                  )
                                )
                              )
                        :down (if (:unvisited-down-tiles-remain? entity)
                                :down
                                (if (:unvisited-up-tiles-remain? entity)
                                  :up
                                  (if (obstacle? pos map-vector (:sweep-mode entity))
                                    (if (obstacle? pos map-vector :down)
                                      :up
                                      :down)
                                    (if (visited-tile? map-vector pos (:sweep-mode entity))
                                      (if (obstacle? pos map-vector :down)
                                      :up
                                      :down)
                                      (:sweep-mode entity)))
                                )
                              )
                        :left (if (obstacle? pos map-vector :up)
                                 (if (obstacle? pos map-vector :down)
                                    (if (obstacle? pos map-vector :left)
                                      :right
                                      :left)
                                   :down)
                                 :up)
                        :right (if (obstacle? pos map-vector :up)
                                 (if (obstacle? pos map-vector :down)
                                    (if (obstacle? pos map-vector :right)
                                      :left
                                      :right)
                                   :down)
                                 :up)
                        )

                        (first back-loop-list)
                    )
       ]
       (assoc (move-robot next-move entity) :back-loop-list (rest back-loop-list))
     )
    entity
   )
  )

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage) :camera (orthographic))
    (add-timer! screen :step-time time-quantum time-quantum)
    (add-timer! screen :visit-counter  0.005 time-quantum)
    (add-timer! screen :visit-time     0.01 time-quantum)
    (add-timer! screen :check-success  0.02 time-quantum)
    (add-timer! screen :check-time     0.03 time-quantum)
    (add-timer! screen :cell-decompose 0.04 time-quantum)
    (add-timer! screen :print-status   0.05 time-quantum)
;;     (add-timer! screen :flag-time   0.1 time-quantum)
    (let [full-map  (to-array-2d
                      (vec (repeat row# (vec (repeat col# unvisited-val)))))
          dirty_tile (texture "dirty_tile_wood.jpg")
          obstacle_tile (texture "obstacle_tile_wood.jpg")
          clean_tile (texture "clean_tile_wood.jpg")
          robot (assoc (texture "robot.png")
                  :x (* tile-size (first init-pos))
                  :y (* tile-size (second init-pos))
                  :pos init-pos
                  :angle 0
                  :height tile-size
                  :width tile-size
                  :direction :up
                  :robot? true
                  :unvisited-down-tiles-remain? false
                  :unvisited-up-tiles-remain? false
                  :sweep-right []
                  :sweep-left []
                  :cell-label 2
                  :sweep-mode :right
                  :unvisited-cells [[:bottom]]
                  :back-loop-list []
                  )
          background (for [row (range row#)]
                       (for [col (range col#)]
                       (assoc (texture dirty_tile)
                               :x (* col tile-size)
                               :y (* row tile-size)
                               :pos [col row]
                               :height tile-size
                               :width tile-size
                               :back? true)))
          text-entity (assoc (label "Revisited Tiles: /n Visited Tiles:" (color :white))
                               :text? true
                               :x (/ tile-size 2)
                               :y (/ tile-size 4)
                               :revisit-counter 0
                               :visit-counter 0
                               :total-tiles (- (* (- row# 2) (- col# 2)) (count (distinct obs-list))))]

      (doseq [idx1 (range row#) idx2 (range col#)]
            (if (or (= 0 idx1) (= 0 idx2) (= (dec row#) idx1) (= (dec col#) idx2) (some #(when (= [idx2 idx1] %) %) obs-list))
                [(aset full-map (- (dec row#) idx1) idx2 obs-val)
                 (texture! (nth (nth background idx1) idx2) :set-texture (texture! obstacle_tile :get-texture))]
              )
        )
      (println "Initial Map:")
      (println (clojure.string/join "\n" (map vec full-map)))
      [  background
         text-entity
         (assoc robot
              :map-vector full-map
              :sweep-left [[(aget full-map (- (dec row#) (dec (first init-pos))) (second init-pos)) (dec (first init-pos)) (second init-pos)]]
              :sweep-right [[(aget full-map (- (dec row#) (inc (first init-pos))) (second init-pos)) (inc (first init-pos)) (second init-pos)]]
              :unvisited-down-tiles-remain? (unvisited? init-pos full-map :down)
              :unvisited-up-tiles-remain? (unvisited? init-pos full-map :up))
       ]
      )

    )

  :on-render
  (fn [screen entities]
    (clear!)
    (->> (for [entity entities]
           (if (:text? entity)
             (doto entity (label! :set-text (str "Revisited Tiles: " (:revisit-counter entity)
                                                 "     Visited Tiles: " (:visit-counter entity) "/" (:total-tiles entity)
                                                 "\nEfficiency:" (* 100 (- 1 (float (/ (:revisit-counter entity) (:total-tiles entity)))))
                                                 "     Difficulty:" (* 10 (float (/ obs# (* (dec col#) (dec row#))))))))
             entity))
         (render! screen)))

  :on-resize
  (fn [screen entities]
    (if (>= row# col#)
      (height! screen (* tile-size row#))
    (width! screen (* tile-size col#)))
  )

  :on-key-down
  (fn [screen entities]
    (cond
     (key-pressed? :r) (app! :post-runnable #(set-screen! cleanbot-game main-screen))
     :else entities))

  :on-timer
  (fn [screen entities]
    (case (:id screen)
      :step-time   [(map (partial motion-planning) entities)]
      :visit-counter [(map (partial visit-count (:pos (last entities)) (:map-vector (last entities))) entities)]
      :visit-time  [(map (partial visit-tile (:pos (last entities))) entities)]
      :check-time  [(map (partial check-state) entities)]
      :cell-decompose [(map (partial cell-decomposition) entities)]
      :print-status [ (println "\nSTATUS REPORT")
                      (println (clojure.string/join "\n" (map vec (:map-vector (last entities)))))
                      (println "Robot position:" (:pos (last entities)))
                      (println "Unvisited down lines:" (:unvisited-down-tiles-remain? (last entities)))
                      (println "Unvisited up lines  :" (:unvisited-up-tiles-remain? (last entities)))
                      (println "Direction:" (:direction (last entities)))
                      (println "Right sweep line:" (:sweep-right (last entities)))
                      (println "Left sweep line:" (:sweep-left (last entities)))
                      (println "Sweep mode:" (:sweep-mode (last entities)))
                      (println "Back loop move list:" (:back-loop-list (last entities)))
                      (println "Back loop cell list:" (distinct (:unvisited-cells (last entities))))
                      entities]
      :check-success (if (map-covered? (:map-vector (last entities)))
                       (do
                         (do
                          (remove-timer! screen :step-time )
                          (remove-timer! screen :visit-counter)
                          (remove-timer! screen :visit-time)
                          (remove-timer! screen :check-time)
                          (remove-timer! screen :print-status)
                          (remove-timer! screen :cell-decompose)
                          (remove-timer! screen :check-success)
                          (println "All area is covered!")
                          (println "Obstacle List:" obs-list)
                         )
                          entities)
                       entities)
;;       :flag-time (println "FLAG")
      )
    )
  )

(defgame cleanbot-game
  :on-create
  (fn [this]
    (set-screen! this main-screen)))
