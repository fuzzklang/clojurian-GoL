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
        (= 3 (count args)) (functions/setup-and-run (first args) (second args) (nth args 2))
        (= 4 (count args)) (functions/setup-and-run (first args) (second args) (nth args 2) (nth args 3))
        :else (functions/print-error)))
    (functions/print-error)))
