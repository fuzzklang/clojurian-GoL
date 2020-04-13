(ns game-of-life.core
  (:require [game-of-life.functions :as functions]
            [clojure.string :as str])
  (:gen-class))

(defn -main
  [& args]
  ;;(def ^:const n-gens 100)
  (if (functions/check-args args)
    (cond
      (= 0 (count args)) (functions/setup-and-run)
      (= 1 (count args)) (functions/setup-and-run (first args))
      (= 3 (count args)) (functions/setup-and-run (first args) (second args) (last args))
      :else (functions/print-error)))
  (println "Finished"))
