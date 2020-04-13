(ns game-of-life.core
  (:require [game-of-life.functions :as functions]
            [clojure.string :as str])
  (:gen-class))

(defn -main
  [& args]
  (if (functions/check-args args)
    (let [args (map functions/parse-arg args)]
      (cond
        (= 0 (count args)) (functions/setup-and-run)
        (= 1 (count args)) (functions/setup-and-run (first args))
        (= 2 (count args)) (functions/setup-and-run (first args) (second args))
        (= 3 (count args)) (functions/setup-and-run (first args) (second args) (last args))
        (= 4 (count args)) (functions/setup-and-run (first args) (second args) (first (take-last 2 args)) (last args))
        :else (functions/print-error)))
    (functions/print-error)))
