(ns pig.core
  (:gen-class))


(defn serialize-state [state]
  (format "Round=%d Ptos=%s PLAYER=%d Turn=%s" (state :round) (state :points) (state :player) (state :turn)))

(defn serialize-log-state [state]
  (format "%s" state))


(defn log-state
  [txt state]
  (do
    (println (format "%s->[%s]" txt (serialize-log-state state) " ]"))
    state))

(defn initial-state
  ([num-players win-points]
   {:turn        []
    :player      0
    :points      (vec (repeat num-players 0))
    :round       0
    :num-players num-players
    :win-points  win-points})
  ([num-players]
   (initial-state num-players 100)))

(defn next-player
  [ {:keys  [player num-players round] :as state}]
  (let
    [next-player (mod (inc player)  num-players)
     next-round (if (zero? next-player) (inc round) round)]
    (merge
      state
      {:player (mod (inc player)  num-players)
       :round next-round})))


(defn accumulate-points
  [{:keys [turn points player] :as state}]
  (let [acc-points
         (reduce
           #(+ %1 %2)
           (points player)
           turn)]
     (merge
        state
        {:turn   []
         :points (assoc points player acc-points)})))


(defn hold [state]
  ((comp next-player accumulate-points ) state))

(defn one [state]
  (next-player
    (merge state {:turn []})))

(defn dice
  [ { :keys [turn] :as state}  num]
  (do
    (merge state {:turn (conj turn num)})))





(defn winner?
  [ {:keys  [points player win-points] :as state}]
  (>= (points player) win-points))






(defn do-hold
  "Hold decision"
  [state]
  (let
    [acc-state (accumulate-points state)]
    (if
      (winner? acc-state)
      acc-state
      (next-player acc-state))))



(defn do-dice
  "Dice decision"
  [state roller]
  (let
    [val (roller)]
    (if
      (= val 1)
      (one state)
      (dice state val))))



(defn play [state roller get-move]
  (let

    [option (get-move state)]
    (cond
     (= option "h") (do-hold state)
     (= option "d") (do-dice state roller)
     :else (do
             ;(println "OPTION" option)))))
             (play state roller get-move)))))


(defn pig-loop
  "Game main loop"
  [players target roller get-move]
  (loop
    [state (initial-state players target)]
    (let
      [
       new-state (play state roller get-move)]
      (if
        (winner? new-state)
        new-state
        (recur new-state)))))


;; Function for read values
(defn get-user-selection
  [ state]
  (do
    (println "")
    (println
      (format "%s"
              (serialize-state state)))
    (println "PLAYER" (state :player) "(h)old (d)ice > ")
    (read-line)))

(defn read-option
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
(defn print-winner
  [state]
  (println
    (format "WINS %s   %s"
      (state :player)
      (serialize-log-state state))))

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
     (pig-loop players target roller get-move)
     (print-winner))))

