#! /usr/bin/env python3

from random import randint

print("Guess-a-number game!",
      "I am thinking of a number between 1 and 100",
      "What's your guess?",
      sep="\n")

target = randint(1, 100)
number = 0
current = 0

while 1:

    number += 1
    current = int(input("Guess %d: " % number))

    if current == target:
        print("Correct! You won!")
        break
    elif number == 5:
        print("Haha, I won! The number was %d" % target)
        break
    elif current < target: 
        print("Too low! Guess again!")
    elif current > target: 
        print("Too high! Guess again!")



"""
Name: Guess Number
Language: Python3
State: Done

Play guess-a-number game



Example ./guess_number.py
"""
