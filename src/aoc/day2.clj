(ns aoc.day2
  (:require
   [clojure.repl]
   [clojure.pprint :as pp]
   [clojure.string :as s]
   [clojure.test :as t]
   ; [clojure.set :as st]
   ; [clojure.core.matrix :as m]
   ; [clojure.core.reducers :as r]
   ; [clojure.math.numeric-tower :as m]
   )
  (:use aoc.core))

(def day 2)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

(defn parse-line [s]
  (let [[_ mn mx letter password] (re-matches #"(\d+)-(\d+) (.): (.+)" s)]
    { :mn (Integer/parseInt mn) :mx (Integer/parseInt mx) :letter letter :password password}
    )
  )

(defn prepare-input [str-input]
  (map #(parse-line %) (s/split-lines str-input)
   ))


(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)

(defn is-valid [{:keys [mn mx letter password]}]
  (let [freqs (frequencies password)
        n (get freqs (.charAt letter 0) nil)
        ]
    (cond 
      (nil? n) false
      (and (>= n mn) (<= n mx)) true
      :else false
      )
    )
  )

(map is-valid t1)

(t/are [i o] (= i o)
       [1 1])



(defn part1 [input]
  (count (filter is-valid input)))

(part1 input)
(println (time (part1 input)))


(defn is-valid2 [{:keys [mn mx letter password]}]
  (let [ch (.charAt letter 0)
        a (nth password (dec mn))
        b (nth password (dec mx))
        ]
    (cond 
      (and (= a ch) (not (= b ch))) true
      (and (= b ch) (not (= a ch))) true
      :else false
      )
    )
  )

(map is-valid2 t1)

(defn part2 [input]
  (count (filter is-valid2 input)))

; (prn (time (part2 input)))
(part2 input)

(defn solve-problem [infile]
  (let [input-string (slurp infile)
        input (prepare-input input-string)]
    (println "Part 1:")
    (println (part1 input))
    (println "")
    (println "Part 2:")
    (println (part2 input))))

(solve-problem (mk-input-filename day))
