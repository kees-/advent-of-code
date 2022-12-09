(ns aoc.util
  (:require [clojure.string]
            [clj-http.client :as http]
            [clojure.java.io :as io]))

(def sl "[s] Abbreviation for clojure.string/split-lines" clojure.string/split-lines)
(def s "[s re] [s re limit] Abbreviation for clojure.string/split" clojure.string/split)

;; ========== SITE-SPECIFIC ====================================================
;; session=...
(def cookie
  (slurp "assets/cookie.txt"))

(defn get-input
  "Digit 1-25 retrieves daily AOC input."
  [day]
  (let [file (format "assets/%02d.txt" day)
        url (format "https://adventofcode.com/2022/day/%s/input" day)]
    (when-not (.exists (io/file file))
      (println "Attempting to grab input from adventofcode.com...")
      (->> (http/get url
                     {:headers {"Cookie" cookie}})
           :body
           (spit file)))
    (slurp file)))

;; ========== GENERAL PURPOSE FNS ==============================================
;; Integer/parseInt doesn't map anonymously on sequences well
(defn str->int
  "Coerce string to integer"
  [s]
  (Integer/parseInt s))

;; https://groups.google.com/g/clojure-dev/c/NaAuBz6SpkY
(defn take-until
  "Returns a lazy sequence of successive items from coll until
   (pred item) returns true, including that item. pred must be
   free of side-effects."
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if (pred (first s))
       (cons (first s) nil)
       (cons (first s) (take-until pred (rest s)))))))
