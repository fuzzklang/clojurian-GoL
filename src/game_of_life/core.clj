(ns game-of-life.core
  (:require [game-of-life.functions :as functions]
            [clojure.string :as str])
  (:gen-class))

(defn -main
  [& args]
  (def ^:const n-gens 100)
  
  (comment (loop [c functions/cells
           n n-gens
           i 0]
      (when (<= 0 n)
        (Thread/sleep 40)
        (functions/draw c 60)
        (println (str "Generation: " (format "%4d/%4d" i n-gens)))
        (recur (functions/next-generation c) (dec n) (inc i))))))


