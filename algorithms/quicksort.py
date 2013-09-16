#! /usr/bin/env python3

from sys import argv
from random import randint

# quicksort 1 - List Comprehension
# quicksort 2 - Iteration
# quicksort 3 - Filter

def quicksort1(seq):
    if len(seq) < 2:
        return seq
    return (quicksort1([s for s in seq[1:] if s < seq[0]]) +
            [seq[0]] + 
            quicksort1([s for s in seq[1:] if s >= seq[0]]))

def quicksort2(seq):
    if len(seq) < 2:
        return seq
    pivot = seq[0]
    low, med, high = [], [], []
    for item in seq:
        if item < pivot:
            low.append(item)
        elif item > pivot:
            high.append(item)
        else:
            med.append(item)
    return quicksort2(low) + med + quicksort2(high)

def quicksort3(seq):
    if len(seq) < 2:
        return seq
    low = list(filter(lambda x: x < seq[0], seq[1:]))
    high = list(filter(lambda x: x >= seq[0], seq[1:]))
    return quicksort3(low) + [seq[0]] + quicksort3(high)

if __name__ == "__main__":
    longlist = [randint(1, 100000) for r in range(100000)]
    quicksort = None
    if len(argv) > 1:
        if argv[1] == "1":
            quicksort = quicksort1
        elif argv[1] == "2":
            quicksort = quicksort2
        elif argv[1] == "3":
            quicksort = quicksort3
    if quicksort is None:
        print("Usage: {} id".format(argv[0]),
              "IDs:",
              "\t1 - List Comprehension",
              "\t2 - Iterative",
              "\t3 - Filter",
              sep="\n")
    else:
        quicksort(longlist)

""" TAIL INFO
Name: Quicksort 
Language: Python3

Comparison of different implementations of quicksort
./quicksort.py 1  2.43s user 0.03s system 98% cpu 2.491 total
./quicksort.py 2  1.80s user 0.02s system 98% cpu 1.857 total
./quicksort.py 3  3.85s user 0.02s system 98% cpu 3.917 total

Example: ./quicksort_listcomp.py 1
"""
