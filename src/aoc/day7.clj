(ns aoc.day7
  (:require
   [clojure.repl]
   [clojure.pprint :as pp]
   [clojure.string :as s]
   [clojure.test :as t]
   [clojure.set :as st]
   [clojure.data :as data]
   [clojure.walk :as w]
   ; [clojure.core.matrix :as m]
   ; [clojure.core.reducers :as r]
   ; [clojure.math.numeric-tower :as m]
   )
  (:use aoc.core))

(def day 7)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

; This don't work for "no other bag" that just returns nil.
; So maybe thats fine
(defn parse-children [string]
  (let [matches (re-seq #" (\d+) ([^,]+?) bags?,?" string)
        items (map (fn [[_ number color]]
                     [(keyword color) (Integer/parseInt number)]) matches)
        ]
    (into {} items)
    ))

; (parse-line (first (s/split-lines testfile)))

(defn parse-line [line]
  (let [matches (re-matches #"^(.+) bags? contain(.+)$" line)
        [_ color children] matches
        children' (parse-children children)
        ]
    [(keyword color) children']))


(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)]
    (into {} (map parse-line lines))))

(def t1 (prepare-input testfile))
(def input (prepare-input infile))

; (pp/pprint t1)

; (println t1)

; (first (filter #(empty? (:children %))  t1) )
; (pp/pprint t1)

(def end-color (keyword "shiny gold"))

(defn stop? [arg]
  (cond
    (= end-color arg) true
    ; (empty? arg) true
    :default false
    ))


; (keys (t1 (keyword "muted yellow")))

; (pp/pprint
;   (tree-seq #(not (stop? %)) (fn [k] (keys (t1 k)) ) (keyword "dark orange"))
;  )

(defn build-tree [defs color]
  (tree-seq #(not (stop? %)) (fn [k] (keys (defs k)) ) color))

(defn is-end-color [k] (= k end-color))

; (some is-end-color (build-tree t1 (keyword "dotted black")))

(defn color-goes-to-end [defs color]
  (some is-end-color (build-tree defs color))
  )

; (color-goes-to-end t1 (keyword "dotted black"))
; (pp/pprint t1)

(defn part1 [input]
  (let [input' (dissoc input end-color)
        colors (keys input')
        to-end (keep #(color-goes-to-end input' %) colors)]
    (count to-end)
    ))

; (part1 t1)
; (part1 input)
; (println (time (part1 input)))


(defn build-nesting [defs]
  (tree-seq
   #(not (empty? (defs (first %))))
   (fn [k] (defs (first k)))
   [end-color 0]))

; (pp/pprint t1)
(t1 (first [end-color 0]))
(build-nesting t1)
(sum (map second (build-nesting t1)))

(pp/pprint t1)
(defn build-down-tree [defs start-color]
  { :color start-color :num 1 :children (map) }
  )

(build-down-tree t1 end-color)

(defn part2 [input]
  (sum (map (fn [in] (count (reduce (fn [acc item] (st/intersection acc item)) (map set in))))  input)))

; (part2 input)

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
