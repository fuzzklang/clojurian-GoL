(ns game-of-life.core-test
  (:require [clojure.test :refer :all]
            [game-of-life.core :refer :all]
            [game-of-life.functions :refer :all]))

(deftest check-all-neighbours1
  (testing "Expecting is-neigbour? to return true for all x in coords wrt cmp."
    (let [cmp [2 2]]
      (is (= true (is-neighbour? cmp [1 1])))
      (is (= true (is-neighbour? cmp [2 1])))
      (is (= true (is-neighbour? cmp [3 1])))
      (is (= true (is-neighbour? cmp [1 2])))
      (is (= true (is-neighbour? cmp [3 2])))
      (is (= true (is-neighbour? cmp [1 3])))
      (is (= true (is-neighbour? cmp [2 3])))
      (is (= true (is-neighbour? cmp [3 3]))))))

(deftest check-all-neighbours2
  (testing "Expecting is-neigbour? to return true for all x in coords wrt cmp."
    (let [coords [[1 1] [2 1] [3 1] [1 2] [3 2] [1 3] [2 3] [3 3]]
          cmp [2 2]
          result (every?
                  #(= % true)
                  (map is-neighbour? (repeat (count coords) cmp) coords))]
      (is (= true result)))))

(deftest not-neighbours1
  (testing "Expecting is-neighbour? to return falsey vals for all compares"
    (let [cmp [2 2]]
      (is (not= true (is-neighbour? cmp [2 2])))
      (is (not= true (is-neighbour? cmp [0 0])))
      (is (not= true (is-neighbour? cmp [4 4])))
      (is (not= true (is-neighbour? cmp [0 2])))
      (is (not= true (is-neighbour? cmp [0 4]))))))

(deftest not-neighbours2
  (testing "Expecting is-neighbour? to return falsey vals for all compares"
    (let [coords [[2 2] [0 0] [4 4] [0 2] [0 4] [2 4] [4 4] [4 0]]
          cmp [2 2]
          result (every?
                  #(not= % true)
                  (map is-neighbour? (repeat (count coords) cmp) coords))]
      (is (= true result)))))

(deftest getting-neighbours
  (testing (str "Expecting neighbours-function to return only "
                "the neighbour coords of a given cell/coordinate.")
    (let [board #{[0 0] [2 1] [3 1] [3 2] [4 4]}
          cell  [2 2]
          exp   #{[2,1] [3 1] [3 2]}]
      (is (= (living-neighbours board cell) exp)))))
