#! /usr/bin/env python3

from sys import argv
from random import randint
from datetime import datetime

def quicksort_iterative(seq):
    if len(seq) < 2:
        return seq
    pivot = seq[0]
    low, med, high = [], [seq[0]], []
    for item in seq[1:]:
        if item < pivot: low.append(item)
        elif item > pivot: high.append(item)
        else: med.append(item)
    return quicksort_iterative(low) + med + quicksort_iterative(high)

def quicksort_listcomp(seq):
    if len(seq) < 2:
        return seq
    return (quicksort_listcomp([s for s in seq[1:] if s < seq[0]]) +
            [seq[0]] + 
            quicksort_listcomp([s for s in seq[1:] if s >= seq[0]]))

def quicksort_filter(seq):
    if len(seq) < 2:
        return seq
    low = list(filter(lambda x: x < seq[0], seq[1:]))
    high = list(filter(lambda x: x >= seq[0], seq[1:]))
    return quicksort_filter(low) + [seq[0]] + quicksort_filter(high)

if __name__ == "__main__":
    longlist = [randint(1, 100000) for r in range(100000)]
    print("Quicksort:")

    t = datetime.now()
    result = quicksort_iterative(longlist)
    t = datetime.now() - t
    print("Iterative: %1d sec %4d ms" % (t.seconds, t.microseconds))

    t = datetime.now()
    result = quicksort_listcomp(longlist)
    t = datetime.now() - t
    print("List Comp: %1d sec %4d ms" % (t.seconds, t.microseconds))

    t = datetime.now()
    result = quicksort_filter(longlist)
    t = datetime.now() - t
    print("Filter:    %1d sec %4d ms" % (t.seconds, t.microseconds))

""" TAIL INFO
Name: Quicksort 
Language: Python3

Comparison of different  implementations of quicksort
Quicksort:
    Iterative: 1 sec 207595 ms
    List Comp: 1 sec 732009 ms
    Filter:    3 sec 146174 ms

"""
