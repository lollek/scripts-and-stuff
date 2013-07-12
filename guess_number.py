#! /usr/bin/env python3

from random import randint

print("Guess-a-number game!",
      "I am thinking of a number between 1 and 100",
      "What's your guess?",
      sep="\n", end=" ")

target = randint(1, 100)
current = 0

while current != target:

    current = int(input())

    if current < target:
        print("Too low! Guess again: ", end="")
    elif current > target:
        print("Too high! Guess again: ", end="")

print("Correct! You won!")

"""
Name: Guess Number
Language: Python3
State: Done

Play guess-a-number game



Example ./guess_number.py
"""
