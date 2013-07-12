#! /usr/bin/env io

# Random value 0-100
GameNum := Object clone

GameNum target := (Random value * 100) floor
GameNum current := -1

GameNum take_guesses := method(

  while (self current != self target,
    self current = (File standardInput readLine) asNumber

    if (self current > self target) then (
      "Too high! Guess again: " print

    ) elseif (self current < self target) then (
      "Too low! Guess again: " print)
    )

  "Correct! You won!" println
)

"Guess-a-number game!" println
"I am thinking of a number between 1 and 100" println
"What's your guess? " print

GameNum take_guesses

/* TAIL INFO:
Name: Guess Number
Language: Io
State: Done

Play guess-a-number game



Example: ./guess_number.io
*/
