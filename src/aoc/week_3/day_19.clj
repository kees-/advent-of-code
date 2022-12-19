(ns aoc.week-3.day-19
  (:require [aoc.util :as util]))

(defonce input
  (util/get-input 19))

(def blueprints
  "Maps of the resource costs for each type of robot."
  (let [costs (fn [[_ ore<-ore clay<-ore obs<-ore obs<-clay geo<-ore geo<-obs]]
                {:ore {:ore ore<-ore}
                 :clay {:ore clay<-ore}
                 :obs {:ore obs<-ore :clay obs<-clay}
                 :geo {:ore geo<-ore :obs geo<-obs}})]
    (->> input
         util/sl
         (mapv #(costs (mapv util/str->int
                             (re-seq #"\d+" %)))))))

(def initial
  {:bots {:ore 1 :clay 0 :obs 0 :geo 0}
   :resources {:ore 0 :clay 0 :obs 0 :geo 0}})

(defn craft
  "Create a new robot, making no attempts to validate available cost."
  [state blueprint bot]
  (-> state
      (update-in [:bots bot] inc)
      (update :resources #(merge-with - % (blueprint bot)))))

(defn extract
  "All available robots hit the mines."
  [state]
  (update state :resources #(merge-with + % (:bots state))))

(defn permute
  "Iterate one permutation of all crafting options from a state and blueprint."
  [state blueprint]
  (let [bots (keys blueprint)
        extracted (extract state)
        affordable? (fn [state blueprint bot]
                      (when-not (some neg? (vals (:resources
                                                  (craft state blueprint bot))))
                        (craft extracted blueprint bot)))]
    (->> (map #(affordable? state blueprint %) bots)
         (filter some?)
         (cons extracted))))

(defn state-comparator
  "Return a fn that orders vals of a simulation state by a particular priority."
  [primacy]
  (let [[a b] (cond-> [:bots :resources] (= primacy :resources) reverse)]
    (fn [state]
      (->> [(a state) (b state)]
           (mapv #(vector (:geo %) (:obs %) (:clay %) (:ore %)))
           (apply interleave)))))

(defn compare-states
  "Given two arrays of ordered values, compare the first unique pair."
  [s1 s2]
  (let [comparisons (mapv #(compare %2 %1) s1 s2)]
    (some #(when (not= 0 %) %) comparisons)))

(defn mine
  "From an initial state and test boundaries, return all possible states!"
  [{:keys [state blueprint steps cap comparator-primacy]}]
  (loop [states [state]
         counter steps]
    (if (zero? counter)
      states
      (recur
       (->> (map #(permute % blueprint) states)
            (reduce into #{})
            (sort-by (state-comparator comparator-primacy) compare-states)
            (take cap))
       (dec counter)))))

;; :cap ensures no more than the 'best' n states are retained each permutation.
;; What is considered best is determined by the priorities of comparisons;
;; among each material type, check the amount of :resources or :bots first.

(comment
  ;; Sums of quality of best results mining with all blueprints for 24 minutes.
  (->> (mapv #(-> {:state initial :blueprint %
                   :steps 24 :cap 83 :comparator-primacy :resources}
                  mine first :resources :geo)
             blueprints)
       (map * (rest (range)))
       (reduce +))
  ;; => 851

  ;; Product of best results mining with first three blueprints for 32 minutes.
  (->> (mapv #(-> {:state initial :blueprint %
                   :steps 32 :cap 1443 :comparator-primacy :bots}
                  mine first :resources :geo)
             (take 3 blueprints))
       (reduce *))
  ;; => 12160
  )
