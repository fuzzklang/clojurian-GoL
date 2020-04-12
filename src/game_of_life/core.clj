(ns game-of-life.core
  (:require [game-of-life.functions :as f :refer :all])
  (:gen-class))

(defn -main
  [& args]
  (println "Hello, World!"))

(def x-boundary 5)
(def y-boundary 5)
;; Assumption: top left = [0,0] and bottom right = [x-max, y-max]
;; Also: a cell is alive if it is in set.
(def cells #{[2 1]  [3 1]  [3 2]})

(defn alive? [board [x y]]
  "Returns true if cell is alive"
  (contains? board [x y]))

;; Optional implementation written in.
(defn is-neighbour? [c [x y]]
  "Check if `[x y]` and c are neigbours. `c` must also be a vector of coords."
  (comment (some #(= % c)
          [[(dec x) (dec y)]
           [x       (dec y)]
           [(inc x) (dec y)]
           [(dec x)  y]
           [(inc x)  y]
           [(dec x) (inc y)]
           [x       (inc y)]
           [(inc x) (inc y)]]))
  (cond (= c [(dec x) (dec y)]) true
        (= c [x       (dec y)]) true
        (= c [(inc x) (dec y)]) true
        (= c [(dec x)  y     ]) true
        (= c [(inc x)  y     ]) true
        (= c [(dec x) (inc y)]) true
        (= c [x       (inc y)]) true
        (= c [(inc x) (inc y)]) true))

(defn living-neighbours [board [x y]]
  "Returns a set of living-neighbours to [x y], as coordinates."
  (set (filter #(is-neighbour? % [x y]) board)))

(defn all-neighbours [[x y]]
  (str "Returns a `set` of all coordinates surrounding `[x y]`"
       "Implementation of this functions decides whether board"
       "boundaries are hard set, or if coords wrap around board.")
  (let [surrounds #{[(dec x) (dec y)]
                    [x       (dec y)]
                    [(inc x) (dec y)]
                    [(dec x)  y]
                    [(inc x)  y]
                    [(dec x) (inc y)]
                    [x       (inc y)]
                    [(inc x) (inc y)]}]
    (set (filter #(and
                (is-neighbour? % [x y])
                (>= (first %) 0)
                (< (first %) x-boundary)
                (>= (second %) 0)
                (< (second %) y-boundary))
              surrounds))))

(defn next-generation [board]
  nil)

(defn draw []
  nil
  )



