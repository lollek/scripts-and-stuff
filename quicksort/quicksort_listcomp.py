#! /usr/bin/env python3

from random import randint

def quicksort(seq):
    if len(seq) < 2:
        return seq
    return (quicksort([s for s in seq[1:] if s < seq[0]]) +
            [seq[0]] + 
            quicksort([s for s in seq[1:] if s >= seq[0]]))

if __name__ == "__main__":
    longlist = [randint(1, 100000) for r in range(100000)]
    print(quicksort(longlist))

""" TAIL INFO
Name: Quicksort Listcomp
Language: Python
State: Done

Created this to compare list comprehension quicksort speed to normal iteration
normal iteration was faster

Example: ./quicksort_listcomp.py

"""
