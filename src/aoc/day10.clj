(ns aoc.day11
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
   [taoensso.tufte :as tufte :refer [p profile]]
   )
  (:use aoc.core))

(def day 11)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))


(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)]
    (map (fn [l] (map (fn [c] (case c \L :empty \. :floor \# :occupied)) l)) lines)))

(def t1 (prepare-input testfile))
(def input (prepare-input infile))

; (pp/pprint t1)

(defn to-indices-set [input]
  (reduce-kv
   (fn [acc1 r lst]
     (reduce-kv (fn [acc2 c e] (assoc acc2 [r c] e))
                  acc1
                  (vec lst)
                ))
   {}
   (vec input)
   ))

; (to-indices-set t1)
(def t2 (to-indices-set t1))

(defn dimensions [state]
  (->> (keys state)
      (sort)
      (reverse)
      (first)
      ))

; (dimensions t2)

(defn adjacent-indices [[r c] [maxr maxc]]
  (filter
    (fn [[x y]]
      (and
      (>= x 0)
      (<= x maxr)
      (>= y 0)
      (<= y maxc)
      ))
   [
    [(inc r) (dec c)]
    [(inc r) c]
    [(inc r) (inc c)]
    [(dec r) (dec c)]
    [(dec r) c]
    [(dec r) (inc c)]
    [r       (inc c)]
    [r       (dec c)]
    ]))

; (adjacent-indices [9 5] (dimensions t2))

(defn index-is-occupied [state pos] (= (state pos) :occupied))
(defn index-is-free [state pos] (= (state pos) :empty))

(defn next-seat-state [oldstate dimensions pos current]
  (let [neig (adjacent-indices pos dimensions)
        any-occupied (some identity (map #(index-is-occupied oldstate %) neig))
        no-occupied (count (filter identity (map #(index-is-occupied oldstate %) neig)))
        ]
    (cond
      (= current :floor) :floor
      (and (= current :empty) (not any-occupied)) :occupied
      (and (= current :occupied) (>= no-occupied 4)) :empty
      :default current
      )))


(defn step [old-state]
  ; (println "iteration")
  (let [dim (dimensions old-state)]
    (reduce-kv
      (fn [acc pos v] (assoc acc pos (next-seat-state old-state dim pos v)))
      {}
      old-state
      ) 
    )
  )

; (step t2)

(defn e-to-char [e] ({ :empty \L :floor \. :occupied \#} e))
(defn print-state [state]
  (let [[maxr maxc] (dimensions state)]
    (doseq [r (range 0 (inc maxr))]
      (doseq [c (range 0 (inc maxc))]
        (print (e-to-char (state [r c])))
        ; (print c)
        )
      (println "")
      )
    ))

; (-> (to-indices-set input)
;     (step)
;     (step)
;     (print-state)
;     )
; (-> (to-indices-set input)
;     (step)
;     (print-state)
;     )

(defn fixpoint [state]
  (reduce #(if (= %1 %2) (reduced %1) %2) (iterate step state))
  )

; (print-state (fixpoint t2))

(defn count-occupied [state]
  (->> state
   (vals)
   (filter #(= :occupied %))
   (count)
   ))

; (count-occupied (fixpoint t2))

(defn part1 [input]
  (->> input
   (to-indices-set)
   (fixpoint)
   (vals)
   (filter #(= :occupied %))
   (count)
   )
  )

; (part1 t1)
; (part1 input)
; (println (time (part1 input)))

(defn get-direction-indices [[r c] [maxr maxc]]
  (let [bounds-fn 
        (fn [[x y]]
          (and
           (>= x 0)
           (<= x maxr)
           (>= y 0)
           (<= y maxc)
           ))
        get-direction
        (fn [next-fn]
          (->> [r c]
               (iterate next-fn)
               (rest)
               (take-while bounds-fn)
               (vec)

               ))
        ]
    [
     (get-direction #(vector (inc (first %)) (dec (second %))))
     (get-direction #(vector (inc (first %)) (second %)))
     (get-direction #(vector (inc (first %)) (inc (second %))))
     (get-direction #(vector (dec (first %)) (dec (second %))))
     (get-direction #(vector (dec (first %)) (second %)))
     (get-direction #(vector (dec (first %)) (inc (second %))))
     (get-direction #(vector (first %)       (inc (second %))))
     (get-direction #(vector (first %)       (dec (second %))))
     ]))

; (pp/pprint (get-direction-indices [1 1] [5 5]))

(defn get-seen [start-state dim pos]
  (let [possible (get-direction-indices pos dim)]
    (remove
     nil?
     (map
      (fn [potentials] (some (fn [p] (if (= (start-state p) :floor) nil p)) potentials))
      possible)
     )
    )
  )

; (get-seen t2 (dimensions t2) [9 0])
; ((build-seen t2) [9 0])


(defn build-seen [start-state]
  (let [dim (dimensions start-state)]
    (reduce-kv
      (fn [acc pos v] (assoc acc pos (get-seen start-state dim pos)))
      {}
      start-state
      )
    )
  )



(defn next-seat-state2 [oldstate seen pos current]
  (let [neig (seen pos)
        any-occupied (some identity (map #(index-is-occupied oldstate %) neig))
        no-occupied (count (filter identity (map #(index-is-occupied oldstate %) neig)))
        ]
    (cond
      (= current :floor) :floor
      (and (= current :empty) (not any-occupied)) :occupied
      (and (= current :occupied) (>= no-occupied 5)) :empty
      :default current
      )))

(defn step2 [seen dim old-state]
  ; (println "iteration")
  (reduce-kv
   (fn [acc pos v] (assoc acc pos (next-seat-state2 old-state seen pos v)))
   {}
   old-state
   )
  )

(defn fixpoint2 [seen dim state]
  (reduce #(if (= %1 %2) (reduced %1) %2) (iterate #(step2 seen dim %) state))
  )

; (print-state (fixpoint t2))


(count-occupied (fixpoint2 (build-seen t2) (dimensions t2) t2))

(defn part2 [input]
  (let [start (to-indices-set input)
        seen (build-seen start)
        dim (dimensions start)
        ]
    (->> start
         (fixpoint2 seen dim)
         ; (step2 seen dim)
         ; (step2 seen dim)
         ; (print-state)
         (count-occupied)
         )))

(println (part2 input))

((build-seen t2) [9 0])

; (println (-> t2 (lst) (diffs)))

; (defn part2 [input]
;   (reduce * (map combs (map count (runs-of-one input))))
;     )

; ; (part2 t1)
; ; (part2 t2)
; ; (part2 input)

; (prn (time (part2 input)))
; ; (part2 input)


; (defn solve-problem [infile]
;   (let [input-string (slurp infile)
;         input (prepare-input input-string)]
;     (println "Part 1:")
;     (println (time (part1 input)) )
;     (println "")
;     (println "Part 2:")
;     (println (time (part2 input)) )))

; (solve-problem (mk-input-filename day))
