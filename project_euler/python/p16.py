#!/usr/bin/env python3
# -*- coding: utf-8 -*-

""" Problem 16 - Power digit sum 

    2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

    What is the sum of the digits of the number 21000?
"""

def pow_sum(x, n):
    """ Calculate the sum of the power.

        Parameters
          x : int : The base integer.
          n : int : The power.
        Return
          int : The sum.
    """
    ps = x ** n
    
    s = 0
    while ps > 0:
        s += ps%10
        ps = ps // 10
        
    return s
    
print("Result: " + str(pow_sum(2, 1000))) # 1366