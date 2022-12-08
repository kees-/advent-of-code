(ns aoc.week-1.day-07-fail
  "Failure scratchpad"
  (:require [aoc.util :as util]
            [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.walk :as walk]))

(defonce input
  (util/get-input 7))

(def history
  (->> input
       s/split-lines
       (remove #(= "$ ls" %))
       (mapv #(s/split % #" "))))

(defn cd
  "Handles 'directory discovery' or traversing to parent"
  [hierarchy path [_ _ dir]]
  (if (or (= ".." dir)
          ((get-in hierarchy path) dir))
    hierarchy
    (assoc-in hierarchy (conj path dir) {})))

(defn dir
  [hierarchy path [_ d]]
  (let [np (conj path d)]
    (assoc-in hierarchy np {})))

(defn file
  [hierarchy path [size filename]]
  (let [size (util/str->int size)
        filename filename]
    (assoc-in hierarchy (conj path filename) size)))

(defn update-path
  [path this]
  (let [path (if (= "cd" (this 1))
               (if (= ".." (this 2))
                 (butlast path)
                 (conj path (this 2)))
               path)]
    (filterv identity path)))

(defn which-cmd
  [line-seq]
  (if (= "$" (first line-seq))
    cd
    (if (= "dir" (line-seq 0))
      dir
      file)))

(defn parse
  [h]
  (loop [z {}
         path []
         [this & remaining] h]
    (if this
      (recur ((which-cmd this) z path this)
             (update-path path this)
             remaining)
      z)))

(def tree (parse history))

(def small-tree (parse (take 50 history)))

(comment
  (walk/prewalk
   (fn [v]
     (println "->" v)
     (if (and (vector? v)
              (number? (val v)))
       (do
         (println "CHANGING")
         (assoc v 1 :a))
       v))
   small-tree))

;; ========== REDO =============================================================
(defn add-sizes
  [z path size]
  (loop [z z
         path path]
    (if (empty? path)
      z
      (recur
       #_(update z path + size)
       (do (println "adding" size "to" path)
           z)
       (subvec path 0 (dec (count path)))))))

(defn parse-hierarchy
  [z path this]
  (if (= "$" (this 0))
    (if (not= ".." (this 2))
      (assoc-in z (conj (conj path 0) (this 2)) [{} 0])
      z)
    (let [[a b] this]
      (if (= "dir" a)
        (assoc-in z (conj (conj path 0) b) [{} 0]) ; if dir
        (let [size (util/str->int a)] ; if file
          (add-sizes z path size))))))

(defn cd-path
  [path [_ _ d]]
  (if d
    (if (= ".." d)
      (subvec path 0 (- (count path) 2))
      (conj (conj path 0) d))
    path))

(defn which-cmd
  [line-seq]
  (if (= "$" (first line-seq))
    cd
    (if (= "dir" (line-seq 0))
      dir
      file)))

(defn parse-2
  [c]
  (loop [z [{} 0]
         path []
         [this & remaining] c]
    (if this
      (let [new-z (parse-hierarchy z path this)
            new-path (cd-path path this)]
        (recur new-z new-path remaining))
      z)))

(pprint
 (parse-2 (take 20 history)))
