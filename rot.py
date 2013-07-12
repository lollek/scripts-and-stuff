#! /usr/bin/env python3.2

from sys import argv

def wrap_around(_min, _curr, _max):
    while _curr < _min:
        _curr = _max - (_min - _curr)
    while _curr > _max:
        _curr = _min + (_curr - _max)-1
    return _curr

def rotate(rotations, string):
    newstring = ""
    for s in string:
        new_ord = ord(s)
        if ord("a") <= ord(s) <= ord("z"):
            new_ord = wrap_around(ord("a"), ord(s)+int(rotations), ord("z"))
        elif ord("A") <= ord(s) <= ord("Z"):
            new_ord = wrap_around(ord("A"), ord(s)+int(rotations), ord("Z"))
        newstring += chr(new_ord)
    print(newstring)

def usage():
    print("usage: {} <rot> <string>".format(argv[0]))
    print("<rot> - how much to rotate, 1 rotates A->B")
    print("<string> - string to rotate")

if __name__ == "__main__":
    if len(argv) < 3 or argv[1].isdigit() is False:
        usage()
    else: rotate(argv[1], " ".join(argv[2:]))

""" TAIL INFO:
Name: Rot
Language: Python3.2
State: Done

This application works like rot13 but expects the user to tell how many rotations are needed


example: ./rot 13 hello world
"""
    
