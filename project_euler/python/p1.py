#!/usr/bin/env python 3

""" Problem 1 - Multiples of 3 and 5

    If we list all the natural numbers below 10 that are multiples of 3 or 5,
    we get 3,5,6, and 9. The sum of these multiples is 23.

    Find the sum of all multiples of 3 or 5 below 1000.
"""

def solution(number):
    return sum([x for x in range(1, number) if x % 3 == 0 or x % 5 == 0])
    
print(solution(1000)) # 233168