(format t "Guess-a-number game!~%~
           I am thinking of a number between 1 and 100~%~
           You have 5 tries to guess it correctly or I win~%~
           What's your guess?~%")

(setf *random-state* (make-random-state t))
(let ((solution (+ 1 (random 100))))
  (defvar tries 0)
  (loop
    (incf tries)
    (format t "Guess ~D: " tries)
    (let ((try (parse-integer (read-line))))
      (cond
        ((= try solution) (return (format t "Correct! You won!~%")))
        ((= tries 5)      (return (format t "Haha, I won! The number was ~D~%"
                                            solution)))
        ((< try solution) (format t "Too low! Try again!~%"))
        (t                (format t "Too high! Try again!~%"))))))
