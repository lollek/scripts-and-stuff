#! /usr/bin/env python3

# Prints out numbers from the fibonacci sequence
# Example usage: ./fibonacci.py
# Example usage: ./fibonacci.py 42

from sys import argv

def fib(n):
    old_value = 1
    new_value = 0
    for _ in range(n):
        old_value, new_value = new_value, new_value + old_value
    return new_value

# If no arguments, print 20 fibs
# Otherwise we'll fib the first argument

if __name__ == "__main__":
    if len(argv) > 1:
        print(fib(int(argv[1])))
    else:
        print("\n".join("%s\t%s" % (fib(r), fib(r+10)) for r in range(10)))
