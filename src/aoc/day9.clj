(ns aoc.day9
  (:require
   [clojure.repl]
   [clojure.pprint :as pp]
   [clojure.string :as s]
   [clojure.test :as t]
   [clojure.set :as st]
   [clojure.data :as data]
   [clojure.walk :as w]
   [clojure.core.match :as mt]
   ; [clojure.core.matrix :as m]
   ; [clojure.core.reducers :as r]
   ; [clojure.math.numeric-tower :as m]
   )
  (:use aoc.core))

(def day 9)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))


(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)]
    (vec (map parse-int lines))))

(def t1 (prepare-input testfile))
(def input (prepare-input infile))

; (pp/pprint t1)

(def step 5)
; (partition step 1 t1)

(defn is-sum-of [{ :keys [n prevs] }]
  (let [compl (map #(- n %) prevs)
        pred (set prevs)
        ]
    (some pred compl)))


(defn to-test [input preamble]
  (map
    (fn [n prev] { :n n :prevs prev })
    (drop preamble input)
    (partition preamble 1 input))
  )

; (to-test t1 5)

(defn part1 [input preamble]
  (:n (first (remove is-sum-of (to-test input preamble)))
    ))

; (part1 t1 5)
; (part1 input 25)
; (println (time (part1 input)))

(defn contig-sum-n [input target n]
  (some #(if (= target (sum %)) % nil)   (partition n 1 input))
  )

(contig-sum-n t1 127 4)

; (contig-sum-n t1 127 4)

(defn contig-sum [input target]
  (some
   #(let [rang (contig-sum-n input target %)]
      (if rang rang nil)
      )
   (range 2 (count input))))

(contig-sum t1 127)

(defn part2 [input preamble]
  (let [target (part1 input preamble)
        contig (contig-sum input target)
        sorted (sort contig)]
    (println sorted)
    (+ (first sorted) (last sorted))
    ))

; (part2 t1 5)
; (part2 input 25)

; (prn (time (part2 input)))
; (part2 input)

(defn solve-problem [infile]
  (let [input-string (slurp infile)
        input (prepare-input input-string)]
    (println "Part 1:")
    (println (time (part1 input)) )
    (println "")
    (println "Part 2:")
    (println (time (part2 input)) )))

(solve-problem (mk-input-filename day))
