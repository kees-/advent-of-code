(ns aoc.week-2.day-10
  (:require [aoc.util :as util]))

(defonce input
  (util/get-input 10))

(def instructions
  "Flat seq of ints and keyword :skip, representing the two CPU instructions."
  (->> input
       util/sl
       (map #(re-find #"noop|-?\d+" %))
       (map #(if (= "noop" %) :skip (util/str->int %)))))

(defn parse
  "The entire simulated state as a seq of vecs [cycle-count register-value]."
  [instructions]
  (loop [cyc 1
         reg 1
         history []
         [this & remaining] instructions]
    (if this
      (let [skip? (= :skip this)]
        (recur (+ cyc (if skip? 1 2))
               (+ reg (if skip? 0 this))
               (reduce conj history (take (if skip? 1 2)
                                          (iterate #(update % 0 inc)
                                                   [cyc reg])))
               remaining))
      history)))

;; This solution is fairly but not completely accurate.
;; The register needs to loop, as its values are between 1 and 40,
;; and assumes all screen rows share an xpos.
;; -1 =/= 39, but draw-step treats them as equivalent,
;; and visual issues will occur at vertical boundaries.
(defn draw
  "Depict a string representation of an imaginary screen of dimensions x,y.
   The proximity of the register to the cycle count makes pixels hot."
  [history x y]
  (let [draw-step (fn [[cyc reg]]
                    (let [sprite (take y (iterate #(+ x %) reg))]
                      (if (some #(<= % cyc (+ 2 %)) sprite)
                        \█ \space)))]
    (->> history
         (map draw-step)
         (partition-all x)
         (map #(reduce str %)))))

(comment
  ;; Get every 40th signal strength from 20 on.
  (->> (parse instructions)
       (drop 19)
       (take-nth 40)
       (mapv #(apply * %))
       (reduce +))
  ;; => 15880
  
  ;; Render hot and cold pixels based on register position.
  (draw (parse instructions) 40 6)
  ;; => ("███  █     ██  ████ █  █  ██  ████  ████"
  ;;     "█  █ █    █  █ █    █ █  █  █    █ █  ██"
  ;;     "█  █ █    █    ███  ██   █  █   █  █   █"
  ;;     "███  █    █ ██ █    █ █  ████  █   █ ██ "
  ;;     "█    █    █  █ █    █ █  █  █ █    █  █ "
  ;;     "█    ████  ███ █    █  █ █  █ ████  ███ ") 
  ;; Note the top right.
  )
