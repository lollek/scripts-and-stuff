#! /usr/bin/env io

Number value := method(return self)
Number rot13 := method(

  # If lower case:
  if ("a" at(0) <= self value and self value <= "z" at(0)) then (
    self curr_min := "a" at(0)
    self curr_max := "z" at(0)


  # Else if upper case:
  ) elseif ("A" at(0) <= self value and self value <= "Z" at(0)) then (
    self curr_min := "A" at(0)
    self curr_max := "Z" at(0)


  # Otherwise, it's not a letter, print & return:
  ) else ( self value asCharacter print; return )


  # Move +13 steps and check if it's still a lower/upper case letter
  # If it is: print it
  # If it's not: Go -13 steps and print

  self new_value := self value + 13
  if (self curr_min <= self new_value and self new_value <= self curr_max) then (
    (self new_value asCharacter print)
  ) else ((self value - 13) asCharacter print)
)

# If there are no arguments, go to loop:
if (System args size == 1) then (
  e := try (
    loop (
      string := File standardInput readLine
      num := 0

      while (num < string size,
        string at(num) rot13
        num = num + 1
      )
     Sequence println
    )
  )
  e catch(Exception, "Exiting .." println)

# Else, rot13 the argv:
) else ( 
  System args foreach (k, v, 
    if (k > 0) then (
      v foreach(j, j rot13)
      " " print))
  "" println
)

/* TAIL INFO:
Name ROT13
Language: Io
State: Done

Rot13 a string



Example: ./rot13.io hello world
*/
