#! /usr/bin/env scala
!#

import java.io.EOFException

def fib (rot:Int) : Unit = {

  var tmp = 0
  var old_value = 1
  var new_value = 0

  for (i <- 0 until rot) {
    tmp = old_value
    old_value = new_value
    new_value = tmp + old_value
  }

  print(new_value)
}


/* If no arguments: print 20 fibs: */
if (args.length == 0) {
  for(i <- 1 to 10) {
    fib(i)
    print("\t")
    fib(i + 10)
    println
  }
} 

/* Otherwise we'll fib argv[1] */
else {
  fib(args(0).toInt)
  println
}

/* TAIL INFO
 * Name: Fibonacci Sequence
 * Language: Scala
 * State: Done
 *
 * Prints out numbers from the fibonacci sequence
 *
 * Example: ./fibonacci.scala
 * Example2: ./fibonacci.scala 42
 * 
*/
