
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


""" TAIL INFO:
Name: Merge
Language: Python3
State: Done

This is the answer to a python-test question at my school, basically:
It should take two lists (which are not of the same length)
It should add the smaller list's element to the larger so they are separated
as evenly as possible

"""
