#! /usr/bin/env python3

from sys import argv

def test():
    print("Running test-cases")
    try:
        assert fib(0) == 0
        assert fib(1) == 1
        assert fib(2) == 1
        assert fib(42) == 267914296
    except:
        print("Failure!")
        raise
    print("Success!")
    exit(0)

def fib(rot):
    old_value = 1
    new_value = 0
    for _ in range(rot):
        old_value, new_value = new_value, new_value + old_value
    return new_value

# If no arguments, print 20 fibs
# Otherwise we'll fib the first argument

if not len(argv) == 1:
    test() if argv[1] == "test" else print(fib(int(argv[1])))
else:
    for r in range(10):
        print("%s\t%s" % (fib(r), fib(r+10)))

""" TAIL INFO:
Name: Fibonacci Sequence
Language: Python3
State: Done

Prints out numbers from the fibonacci sequence


Example: ./fibonacci.py
Example2: ./fibonacci.py 42
"""
