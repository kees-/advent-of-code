(ns aoc.util
  (:require [clojure.string]
            [clj-http.client :as http]
            [clojure.java.io :as io]))

(def sl clojure.string/split-lines)
(def s clojure.string/split)

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

;; Integer/parseInt doesn't work well anonymously on sequences
(defn str->int
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
