#! /usr/bin/env scala
!#

import scala.util.Random

println("Guess-a-number game!")
println("I am thinking of a number between 1 and 100")
print("What's your guess? ")

val target = Random.nextInt(100) + 1
var current = 0

while (current != target) {

  current = Console.readInt
  if (current < target)
    print("Too low! Guess again: ")
  else if (current > target)
    print("Too high! Guess again: ")

}

println("Correct! You won!")

/** TAIL INFO:
 * Name: Guess Number
 * Language: Scala
 * State: Done
 *
 * Play guess-a-number game
 *
 *
 * Example: ./guess_number.scala
*/
