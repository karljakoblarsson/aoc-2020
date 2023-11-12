(ns aoc.day12
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

(def day 12)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))
(def testfile2 (slurp "inputs/test10b.txt"))

(defn parse-line [line]
  (let [[_ cmd arg] (re-matches #"(.)(\d+)" line)]
    { :cmd (keyword cmd) :arg (parse-int arg) }
    ))

(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)]
    (map parse-line lines)))

(def t1 (prepare-input testfile))
(def input (prepare-input infile))

; (frequencies (filter (fn [{ :keys [cmd arg]}] (contains? #{ :L :R} cmd)) input))
; (frequencies (filter #(i)))

(def init-state { :pos [0 0] :direction 90 })

(defn apply-step [{ [n e] :pos dir :direction :as state} { :keys [cmd arg] :as step}]
  (case cmd
    :N (assoc state :pos [(+ n arg) e])
    :S (assoc state :pos [(- n arg) e])
    :E (assoc state :pos [n (+ e arg)])
    :W (assoc state :pos [n (- e arg)e])
    :L (update state :direction #(- % arg))
    :R (update state :direction #(+ % arg))
    :F (assoc state :pos
              (case (mod dir 360)
                0 [(+ n arg) e]
                180 [(- n arg) e]
                90 [n (+ e arg)]
                270 [n (- e arg)]
                ))
    )
  )

; (run t1)

(defn run [steps]
  (reduce apply-step init-state steps)
  )

; (run t1)

(defn part1 [input]
  (->> input
      (run)
      (:pos)
      (map abs)
      (sum)
   )
  )

; (part1 t1)
; (part1 input)
; (println (time (part1 input)))

; (println (-> t2 (lst) (diffs)))

(def init-state2 { :pos [0 0] :waypoint [10 1] })

    ; :F (assoc state :pos
    ;           (case (mod dir 360)
    ;             0 [(+ n arg) e]
    ;             180 [(- n arg) e]
    ;             90 [n (+ e arg)]
    ;             270 [n (- e arg)]
    ;             ))

(defn sin [dir]
  (case (mod dir 360)
    0   0
    90  1
    180 0
    270 -1))
(defn cos [dir]
  (case (mod dir 360)
    0   1
    90  0
    180 -1
    270 0))

; (defn rotate-around [[wn we] degs]
;   (let [s (sin degs)
;         c (cos degs)
;         ]
;   [
;   ;  float xnew = p.x * c - p.y * s;
;   ; float ynew = p.x * s + p.y * c;
;    (+ (* we s) (* wn c))
;    (+ (* we c) (* wn s))
;    ]))

(defn rotate-around [[we wn] degs]
  (case (mod degs 360)
    0 [wn we]
    90 [wn (* -1 we)]
    180 [(* -1 we) (* -1 wn)]
    270 [(* -1 wn) we]
    )
  )


(rotate-around [10 4] 90)


(defn apply-step2 [{ [e n] :pos
                    [we wn] :waypoint
                    :as state} { :keys [cmd arg] :as step}]
  (println state step)
  (case cmd
    :N (assoc state :waypoint [we (+ wn arg)])
    :S (assoc state :waypoint [we (- wn arg)])
    :E (assoc state :waypoint [(+ we arg) wn])
    :W (assoc state :waypoint [(- we arg) wn])
    :L (assoc state :waypoint (rotate-around [we wn] (* -1 arg)))
    :R (assoc state :waypoint (rotate-around [we wn] arg))
    :F (assoc state :pos [(+ e (* arg we)) (+ n (* arg wn))])
    )
  )


(defn run2 [steps]
  (reduce apply-step2 init-state2 steps)
  )

(println (run2 t1))

(defn part2 [input]
  (->> input
      (run2)
      (:pos)
      (map abs)
      (sum)
   )
  )


; (part2 t1)
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
