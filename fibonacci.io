#! /usr/bin/env io

fib := method(rot,
  tmp := 0
  old_value := 1
  new_value := 0
              
  while(rot > 0,
    tmp = old_value
    old_value = new_value
    new_value = tmp + old_value
    rot = rot - 1
  )

  new_value print
)

# If there are no arguments, run 20 fibs:
if (System args size == 1) then (
  i := 0
  e := try (
    while(i < 10,
      fib(i)      
      "\t" print
      fib(i + 10)
      "\n" print
      i = i + 1
    )
  )
  e catch(Exception, nil)

# Else, print the fib from argv[1]:
) else (
  cmd := System args at(1) asNumber
  if (cmd >= 0) then (
    fib(System args at(1) asNumber)
    "" println
  ) else ("Not a number!" println)
)

/*
Name: Fibonacci Sequence
Language: Io
State: Done

Prints out numbers from the fibonacci sequence


Example: ./fibonacci.io
Example2: ./fibonacci.io 42
*/
