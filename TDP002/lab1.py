#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from functools import reduce
import math

# Uppgift 1A
def sum_natural_numbers(range_end):
    """ Ger summeran av alla naturliga tal till och med range_end """
    return reduce(lambda x, y: x + y, range(1, range_end+1))

# Uppgift 1B
def product_natural_numbers(range_end):
    """ Ger produkten av alla naturliga tal till och med range_end """
    return reduce(lambda x, y: x * y, range(1, range_end+1), 1)

# Uppgift 1C:
def smallest_divisible(range_end):
    """ Ger det minsta naturliga talet som kan delas jamnt med alla naturliga tal
        till och med range_end """

    def not_divisible(dividend):
        for divisor in range(2,range_end+1):
            if dividend % divisor != 0:
                return True
        return False
    
    current = 1
    while not_divisible(current):
        current += 1
    return current


# Uppgift 1D:
def sum_of_primes(range_end):
    """ Summera alla primtal mindre an range_end """

    def is_prime(dividend):
        """ Sant om dividend inte kan delas jamnt med nagot foregaende tal """

        # For en snabbare version, byt ut foljande rad mot denna:
        # for divisor in range(2, int(math.sqrt(dividend))+1):
        for divisor in range(2,dividend):
            if dividend % divisor == 0:
                return False
        return True

    return reduce (lambda x, y: x + y if is_prime(y) else x, 
                   range(2, range_end))


