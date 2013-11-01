#! /usr/bin/env python3

from sys import argv, stdin

def rot13(sentence):

    new_sentence = ""
    for letter in sentence:
        
        if 'a' <= letter.lower() <= 'm':
            new_sentence += chr(ord(letter) + 13)
        elif 'n' <= letter.lower() <= 'z':
            new_sentence += chr(ord(letter) - 13)
        else:
            new_sentence += letter

    print(new_sentence, end="")

# No arguments means a pipe:
if len(argv) == 1:
    for line in stdin:
        rot13(line)

# Otherwise we'll rot13 the arguments:
else:
    rot13(" ".join(argv[1:]+["\n"]))

""" TAIL INFO:
Name ROT13
Language: Python3
State: Done

Rot13 a string


Example: ./rot13.py hello world
Example2: echo "hello world" | ./rot13.py
"""
