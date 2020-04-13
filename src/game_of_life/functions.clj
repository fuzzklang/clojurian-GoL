(ns game-of-life.functions
  (:require [clojure.string :as str])
  (:gen-class))

(declare x-boundary)
(declare y-boundary)
(declare cells)

;; Assumption: top left = [0,0] and bottom right = [x-max, y-max]
;; Also: a cell is alive if it is in set.
(defn alive?
  "Returns true if cell is alive"
  [cells [x y]]
  (contains? cells [x y]))

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
  [cells [x y]]
  (set (filter #(is-neighbour? % [x y]) cells)))

(defn dead-neighbours
  "Returns a `set` of coordinates of all dead cells surrounding `[x y]`
  Implementation of this functions decides whether cells
  boundaries are hard set, or if coords wrap around cells."
  [cells [x y]]
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
                   (not (contains? cells %))
                   (>= (first %) 0)
                   (< (first %) x-boundary)
                   (>= (second %) 0)
                   (< (second %) y-boundary))
                 surrounds))))

(defn continues?
  "Return `bool` for whether given cell lives on in next generation or not."
  [cells coord]
  (let [n (count (living-neighbours cells coord))]
    (cond (< n 2) false
          (> n 3) false
          :else true)))

(defn awakens?
  "Return `bool` for whether given cell comes alive in next generation or not."
  [cells coord]
  (= (count (living-neighbours cells coord)) 3))

(defn next-generation
  "Generates a new cells based on previous one"
  [cells]
  (let [potentials (set (mapcat #(dead-neighbours cells %) cells))
        new-cells  (filter #(awakens? cells %) potentials)
        continuing (filter #(continues? cells %) cells)]
    (set (concat new-cells continuing))))

(defn generation-n [cells n]
  "Get the cells of generation `n`"
  (loop [i n
         c cells]
    (if (<= i 0)
      c
      (recur (dec i) (next-generation c)))))

(defn token [cells [x y]]
  (cond (alive? cells [x y]) "O"
        :else "."))

(defn to-string [cells]
  (let [max-i (* x-boundary y-boundary)
        string (token cells [0 0])]
    (loop [i 1
           s string
           x (mod i x-boundary)
           y (int (/ i x-boundary))]
      (if (= i max-i)
        s
        (recur (inc i)
               (str/join [s
                          (cond (= x (dec x-boundary)) (str/join [(token cells [x y]) "\n"])
                                :else (token cells [x y]))])
               (mod (inc i) x-boundary)
               (int (/ (inc i) x-boundary)))))))

(defn draw
  "Draws cells. Default 50 newlines for terminal printout"
  ([set]
   (draw set 50))

  ([set n-lines]
   (dotimes [i n-lines]
     (println))
   (println (to-string set))
   (println "Number of cells: " (count set))))

(defn create-rand-set
  "TODO: Return a set of random coordinates."
  [x-max y-max]
  #{[2 0] [2 1] [2 2] [1 2] [0 1]} ;; Glider
  ;;(def ^:const cells #{[2 1] [3 1] [3 2]})
  ;;(def ^:const cells #{[0 0]})
  ;;(def ^:const cells #{[]})
  ;;(def ^:const cells #{[2 0] [2 1] [2 2] [1 2] [0 1]})       ; glider
  )

(defn check-args [args]
  (every? #(number? %) args))

(defn setup-and-run
  "Setup parameters and run loop function `n-generations` times."
  ([]
   (setup-and-run 100))

  ([n-generations]
   (setup-and-run n-generations 40 25))

  ([n-generations x-max y-max]
   (setup-and-run n-generations x-max y-max (create-rand-set x-max y-max)))

  ([n-generations x-max y-max cells]
   (def ^:const x-boundary x-max)
   (def ^:const y-boundary y-max)
   (def ^:const n-gens n-generations)
   (loop [c cells
          n n-gens
          i 0]
     (when (<= 0 n)
       (Thread/sleep 40)
       (draw c 60)
       (println (str "Generation: " (format "%4d /%4d" i n-gens)))
       (recur (next-generation c) (dec n) (inc i))))))

(defn print-error []
  (println "Usage: lein run")
  (println "or     lein run <num generations>")
  (println "or     lein run <num generations> <n columns> <n columns>"))
