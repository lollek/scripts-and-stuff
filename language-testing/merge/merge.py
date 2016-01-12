#! /usr/bin/env python3

def merge(list1, list2):
    small, large = sorted([list1[::-1], list2[::-1]], key=lambda x: len(x))

    ratio = len(large) // len(small)
    if not len(large) % len(small):
        ratio -= 1

    new_list = []
    while small or large:
        for _ in range(ratio):
            if large:
                new_list.append(large.pop())
        if small:
            new_list.append(small.pop())

    return new_list

def test():
    print("Starting test-cases")
    assert merge([1, 2, 3, 4],['a', 'b',  'c']) == [1, 'a', 2, 'b', 3, 'c', 4]
    assert merge([1, 2, 3, 4, 5], ['a', 'b']) == [1, 2, 'a', 3, 4, 'b', 5]
    assert merge(['a', 'b', 'c'], [1, 2, 3, 4, 5]) == [1, 'a', 2, 'b', 3, 'c', 4, 5]
    assert merge([1, 2, 3, 4, 5, 6], ['a', 'b']) == [1, 2, 'a', 3, 4, 'b', 5, 6]
    assert merge(list(range(1,9)), ['A', 'B', 'C', 'D', 'E']) == [1, 'A', 2, 'B', 3, 'C', 4, 'D', 5, 'E', 6, 7, 8]
    print("Success!")

if __name__ == "__main__":
    test()

""" TAIL INFO:
Name: Merge
Language: Python3
State: Done

This is the answer to a python-test question at my school, basically:
It should take two lists (which are not of the same length)
It should add the smaller list's element to the larger so they are separated
as evenly as possible

"""
