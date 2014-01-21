import scala.util.Random

object guess_number {

  val target = Random.nextInt(100) + 1
  var current = 0

  def main(argv: Array[String]) {
    println("Guess-a-number game!")
    println("I am thinking of a number between 1 and 100")
    print("What's your guess? ")
    
    guess()
  }

  def guess() {
    while (current != target) {

      current = Console.readInt()

      if (current < target)
        print("Too low! Guess again: ")

      else if (current > target)
        print("Too high! Guess again: ")

      else {
        println("Correct! You won!")
        return
      }
    }
  }
  
}

/** TAIL INFO:
 * Name: Guess Number
 * Language: Scala
 * Compile: scalac guess_number.scala
 * State: Done
 *
 * Play guess-a-number game
 *
 * Example: scala guess_number
*/
