
; Guess-a-number game. Solution is [1-100]

(println
  (str
    "Guess-a-number game!\n"
    "I'm thinking of a number between 1 and 100.\n"
    "You have 5 tries to guess it correctly.\n"
    "What's your guess?"))

(defn take-guess [guess-num]
  (printf "Guess %d: " guess-num)
  (flush)
  (read-string (read-line)))

(let [solution (+ (rand-int 100) 1)]
  (loop [guess-num 1]
    (let [guess (take-guess guess-num)]
      (cond
        (= guess solution) (println "Correct! You have won!")
        (= guess-num 5) (println "Haha, I won! The number was" solution)
        :else (do
                (if (< guess solution)
                  (println "Too low! Try again!")
                  (println "Too high! Try again!"))
                (recur (inc guess_num)))))))
