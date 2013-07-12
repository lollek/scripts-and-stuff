#! /usr/bin/env python3

from sys import argv

def fib(rot):
    
    old_value = 1
    new_value = 0

    while rot > 0:
        
        old_value, new_value = new_value, new_value + old_value
        rot -= 1

    print(new_value, end="")

# If no arguments, print 20 fibs::
if len(argv) == 1:
    
    for r in range(10):
        fib(r)
        print("\t", end="")
        fib(r + 10)
        print("\n", end="")

# Otherwise we'll fib the first argument:
else:
    fib(int(argv[1]))
    print()

""" TAIL INFO:
Name: Fibonacci Sequence
Language: Python3
State: Done

Prints out numbers from the fibonacci sequence


Example: ./fibonacci.py
Example2: ./fibonacci.py 42
"""
