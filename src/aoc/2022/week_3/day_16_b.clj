;; > 2098

(ns aoc.2022.week-3.day-16-b
  (:require [aoc.util :as util]))

(defonce input
  (util/get-input 2022 16))

(defonce small-input
  (slurp "assets/16sample.txt"))

(defn description->map
  ([m nodes]
   (let [r (fn [m [node weight & conn]]
             (assoc m node {:weight weight :conn conn :open? false}))]
     (reduce r m nodes))))

(def graph
  (->> input
       util/sl
       (map #(vec (re-seq #"[A-Z]{2}|\d+" %)))
       (map #(update % 1 util/str->int))
       (description->map {})))

(defn calculate-score
  ([path] (calculate-score path 0 0))
  ([path score influx]
   (let [r (fn [[total influx pos] step]
             (if (= step :open)
               (let [{:keys [weight]} (graph pos)]
                 [(+ total influx) (+ influx weight) pos])
               [(+ total influx) influx step]))]
     (subvec (reduce r [score influx nil] path) 0 2))))

(defn expand
  ([path] (expand graph path))
  ([graph path]
   (let [pos (last (remove #(= :open %) path))
         can-open? (and (not (get-in graph [pos :open?]))
                        (not= :open (last path))
                        (< 0 (get-in graph [pos :weight])))
         {:keys [conn]} (graph pos)
         freq (frequencies path)
         lollygagging (remove #(when-let [n (freq %)] (> 4 n)) conn)]
     (cond-> (map #(conj path %) lollygagging)
       can-open? (conj (conj path :open))))))

(defn calc-with-existing [paths path]
  (if-let [existing (some #(when (paths %) [(paths %) %])
                          (iterate butlast path))]
    (if (= (paths existing) path)
      (paths existing)
      (apply calculate-score
             (subvec path (dec (count existing)))
             (paths existing)))
    (calculate-score path)))

(defn traverse
  [start steps]
  (loop [countdown steps
         graph graph
         next-paths #{[start]}
         paths {nil [0 0]}]
    (println countdown
             "| paths:" (count paths)
             "next-paths:" (count next-paths))
    (if (some #(false? (:open? %)) (vals graph))
      (if (zero? countdown)
        paths
        (let [#_#_p (into paths next-paths)
              np (reduce into #{} (map expand next-paths))
              p (->> next-paths
                     (map #(calc-with-existing paths %))
                     (zipmap next-paths)
                     (merge paths))]
          (recur (dec countdown)
                 graph
                 np
                 p)))
      paths)))

(comment
  (traverse "AA" 5)
  (->> (traverse "AA" 20) vals (map first) (reduce max)))
