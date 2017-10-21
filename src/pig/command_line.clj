(ns pig.command-line
  (:require [pig.core :as pig]))

(defn serialize-state [state]
  (format "Round=%d Ptos=%s PLAYER=%d Turn=%s" (state :round) (state :points) (state :player) (state :turn)))

;; Function for read values
(defn- get-user-selection
  [ state]
  (do
    (println "")
    (println
      (format "%s"
              (serialize-state state)))
    (println "PLAYER" (state :player) "(h)old (d)ice > ")
    (read-line)))

(defn- read-option
  [txt]
  (do
    (println txt "?")
    (read-string (read-line))))

;; Function for dice
(defn random-dice [ & no-print]
  (let
    [ val
     (->
       (rand-int 6)
       inc)]
    (if (empty? no-print) (println "dice=" val))
    val))

;; Declare winner
(defn- print-winner
  [state]
  (println
    (format "WINS %s   %s"
            (state :player)
            (pig/serialize-log-state state))))

(defn -main
  "Start interactive game"
  [& args]

  (println "PIG GAME")
  (let
    [
     players (read-option "Number of players")
     target (read-option "Target points")
     roller random-dice
     get-move get-user-selection]
    (->
      (pig/pig-loop players target roller get-move)
      (print-winner))))