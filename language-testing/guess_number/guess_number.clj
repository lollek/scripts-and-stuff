(use '[clojure.string :only (join)])

(println
  (join "\n"
        ["Guess-a-number game!"
         "I'm thinking of a number between 1 and 100."
         "You have 5 tries to guess it correctly."
         "What's your guess?"]))
(let [solution (+ (rand-int 100) 1)]
  ; Solution is [1-100]
  (loop [guess_num 1]
    (print "Guess" guess_num ": ")
    (flush)
    (let [guess (read-string (read-line))]
      (if (= guess solution)
        (println "Correct! You have won!")
        (if (= guess_num 5)
          (println "Haha, I won! The number was" solution)
          (do
            (if (< guess solution)
              (println "Too low! Try again!")
              (println "Too high! Try again!"))
            (recur (inc guess_num))))))))
