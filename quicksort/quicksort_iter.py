#! /usr/bin/env python3.2

from random import randint

def quicksort(seq):
    if len(seq) < 2:
        return seq

    this = seq[0]
    less = []
    med = []
    more = []

    for s in seq:
        if s < this:
            less.append(s)
        elif s > this:
            more.append(s)
        else:
            med.append(s)

    less = quicksort(less)
    more = quicksort(more)
    return less + med + more

if __name__ == "__main__":
    longlist = [randint(1, 100000) for r in range(100000)]
    print(quicksort(longlist))

""" TAIL INFO
Name: Quicksort Iterative
Language: Python
State: Done

Created this to compare list comprehension quicksort speed to normal iteration
normal iteration was faster

Example: ./quicksort_iter.py

"""
