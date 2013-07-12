#! /usr/bin/env python3

from sys import argv

def rot13(sentence):

    new_sentence = ""
    for letter in sentence:

        if letter.isupper():
            a = ord('A')
            z = ord('Z')
        else:
            a = ord('a')
            z = ord('z')

        if a <= ord(letter) <= z:
            if a <= ord(letter) + 13 <= z:
                new_sentence += chr(ord(letter) + 13)
            else:
                new_sentence += chr(ord(letter) - 13)
        else:
            new_sentence += letter

    print(new_sentence)

# No arguments means a loop:
if len(argv) == 1:
    try:
        while 1:
            rot13(input())
    except EOFError: pass
    except KeyboardInterrupt: pass

# Otherwise we'll rot13 the arguments:
else:
    rot13(" ".join(argv[1:]))

""" TAIL INFO:
Name ROT13
Language: Python3
State: Done

Rot13 a string



Example: ./rot13.py hello world
"""
