(ns pig.core-test
  (:require [clojure.test :refer :all]
            [pig.core :refer :all]))

(deftest initial-state-test
  (testing "1 player"
    (is (=
          (initial-state 1)
          {:turn        []
           :player      0
           :points      [0]
           :round       0
           :num-players 1
           :win-points 100})))

  (testing "2 players"
    (is (=
          (initial-state 2)
          {:turn []
           :player 0
           :points [0 0]
           :round 0
           :num-players 2
           :win-points 100}))))

(deftest next-player-test
  (testing "next player"
    (is (=
          (next-player
            { :player 0  :num-players 2 :round 2})
          { :player 1 :num-players 2 :round 2})))

  (testing "next round"
    (is (=
          (next-player
            { :player 1  :num-players 2 :round 2})
          { :player 0 :num-players 2 :round 3})))

  (testing "one player"
    (is (=
          (next-player
            { :player 0  :num-players 1 :round 2})
          { :player 0 :num-players 1 :round 3}))))

(deftest accumulate-points-test
  (testing "accumulate"
    (is (=
          (accumulate-points
            {:turn [3 4]
             :points [10 23]
             :player 1})
          {:turn []
           :points [10 30]
           :player 1}))))

(deftest hold-one-test
  (testing "hold turn"
    (is (=
          (hold
            {:turn [ 1 2]
             :num-players 2
             :points [ 10 20]
             :player 1
             :round 4})
          {:turn []
           :num-players 2
           :points [ 10 23]
           :player 0
           :round 5})))
  (testing "one turn"
    (is (=
          (one
            {:turn [ 1 2]
             :num-players 2
             :points [ 10 20]
             :player 1
             :round 4})
          {:turn []
           :num-players 2
           :points [ 10 20]
           :player 0
           :round 5}))))

(deftest serialize-state-test
  (testing "repl prompt"
    (is (=
          (serialize-state
            {:turn [ 1 2]
             :num-players 2
             :points [ 10 20]
             :player 1
             :round 4})
          "Round=4 Ptos=[10 20] PLAYER=1 Turn=[1 2]"))))

(deftest dice-test
  (testing "dice thorw"
    (is (=
          (dice {:turn [1 2]} 3)
          {:turn [1 2 3]})))
  (testing "random dice"
    (is (=
          (reduce (fn [s _ ] (conj  s (random-dice :no-print))) #{} (range 100))
          #{ 1 2 3 4 5 6}))))

(deftest game-over-test
  (testing "player wins"
    (is (=
          (winner? {:points [80 110] :player 1 :win-points 100})
          true)))
  (testing "player lose"
    (is (=
          (winner? {:points [80 110] :player 0 :win-points 100})
          false))))

(defn move-generator
  [l]
  (let
    [a (atom l)]
    (fn [state]
      (let
        [s @a]
        (reset! a (next s))
        (if
          (empty? s)
          nil
          (first s))))))

(defn roller-generator
  [sequence]
  (let
    [a (atom sequence)]
    (fn []
      (let
        [s @a]
        (reset! a (next s))
        (if
          (empty? s)
          nil
          (first s))))))

(deftest complete-game-test
  (testing "roller generator"
    (let
      [gen (roller-generator [1 2 3])]
      (is (= (gen) 1))
      (is (= (gen) 2))
      (is (= (gen) 3))))
  (testing "move-generator"
    (let
      [gen (move-generator ["d" "h" "d"])]
      (is (= (gen {}) "d"))
      (is (= (gen {}) "h"))
      (is (= (gen {}) "d"))))
  (testing "one player game"
    (is (=
          (pig-loop
             1
             10
             (roller-generator  [1    5  6])
             (move-generator    ["d" "d" "d" "h"]))
          { :turn [],
           :player 0,
           :points [11],
           :round 1,
           :num-players 1,
           :win-points 10})))
  (testing "two players game"
    (is (=
          (pig-loop
            2
            10
            (roller-generator [ 1   5       6   1   5])
            (move-generator   ["d" "d" "h" "d" "d" "d" "h"]))
          {
           :turn [],
           :player 1,
           :points [0 10],
           :round 1,
           :num-players 2,
           :win-points 10})))
  (testing "Tree players game"
    (is (=
          (pig-loop
            3
            10
            (roller-generator [ 5   4       1   4   6])
            (move-generator   ["d" "d" "h" "d" "d" "d" "h"]))
          {
           :turn [],
           :player 2,
           :points [9 0 10],
           :round 0,
           :num-players 3,
           :win-points 10}))))


