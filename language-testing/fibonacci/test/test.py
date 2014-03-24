#! /usr/bin/env python3

import unittest

from fibonacci import fib

class TestClass(unittest.TestCase):
    def test(self):
        self.assertEqual(fib(0), 0)
        self.assertEqual(fib(1), 1)
        self.assertEqual(fib(2), 1)
        self.assertEqual(fib(42), 267914296)

if __name__ == "__main__":
    unittest.main()
