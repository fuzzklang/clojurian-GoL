(ns game-of-life.core
  (:require [game-of-life.functions :as f :refer :all]
            [clojure.string :as str])
  (:gen-class))

;; Assumption: top left = [0,0] and bottom right = [x-max, y-max]
;; Also: a cell is alive if it is in set.
(def x-boundary 50)
(def y-boundary 30)
;;(def cells #{[2 1] [3 1] [3 2]})
;;(def cells #{[0 0]})
(def cells #{[2 0] [2 1] [2 2] [1 2] [0 1]})       ; glider
(declare draw)
(declare next-generation)

(defn -main
  [& args]
  (draw (next-generation cells))
  (loop [board cells
         n 1]
    (when (<= 0)
      (Thread/sleep 30)
      (draw board 60)
      (recur (next-generation board) (dec n)))))

;; Maybe redundant?
(defn alive?
  "Returns true if cell is alive"
  [board [x y]]
  (contains? board [x y]))

;; Optional implementation written in.
(defn is-neighbour?
  "Check if `[x y]` and c are neigbours. `c` must also be a vector of coords.
  Implementation of this function decides whether game wraps around boundaries or not."
  [c [x y]]
  (cond (= c [(dec x) (dec y)]) true
        (= c [x       (dec y)]) true
        (= c [(inc x) (dec y)]) true
        (= c [(dec x)  y     ]) true
        (= c [(inc x)  y     ]) true
        (= c [(dec x) (inc y)]) true
        (= c [x       (inc y)]) true
        (= c [(inc x) (inc y)]) true))

(defn living-neighbours
  "Returns a set of living-neighbours to [x y], as coordinates."
  [board [x y]]
  (set (filter #(is-neighbour? % [x y]) board)))

(defn dead-neighbours
  "Returns a `set` of coordinates of all dead cells surrounding `[x y]`
  Implementation of this functions decides whether board
  boundaries are hard set, or if coords wrap around board."
  [board [x y]]
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
                   (not (contains? board %))
                   (>= (first %) 0)
                   (< (first %) x-boundary)
                   (>= (second %) 0)
                   (< (second %) y-boundary))
                 surrounds))))

(defn continues?
  "Return `bool` for whether given cell lives on in next generation or not."
  [board coord]
  (let [n (count (living-neighbours board coord))]
    (cond (< n 2) false
          (> n 3) false
          :else true)))

(defn awakens?
  "Return `bool` for whether given cell comes alive in next generation or not."
  [board coord]
  (= (count (living-neighbours board coord)) 3))

(defn next-generation
  "Generates a new board based on previous one"
  [board]
  (let [potentials (set (mapcat #(dead-neighbours board %) board))
        new-cells  (filter #(awakens? board %) potentials)
        continuing (filter #(continues? board %) board)]
    (set (concat new-cells continuing))))

(defn token [board [x y]]
  (cond (alive? board [x y]) "O"
        :else "."))

(defn draw
  "Draws board. Can be supplied with optional number of terminal lines to refresh with"
  ([board]
   (draw board 50))

  ([board n-lines]
   (dotimes [i n-lines]
     (println))

   (println
    (loop [i 0, string (token board [0 0]), [x y] [0 0]]
      (if (>= i (* x-boundary y-boundary))
        string
                                        ;(.substring (java.lang.String. string) 0 (- (count  string) 1))
        (let [x (mod i x-boundary)
              y (int (/ i y-boundary))
              tok (token board [x y])
              expanded-str (cond
                             (= x (dec x-boundary)) (str/join [string tok "\n"]) ; If at end of board, add newline
                             :else (str/join [string tok]))] ;otherwise not
                                        ;(println "x:" x "y:" y "i:" i)
          (recur (inc i) expanded-str [x y])))))))
