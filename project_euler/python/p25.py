#!/usr/bin/env python
# -*- coding: utf-8 -*-

import math

""" Problem 25 - 1000-digit Fibonacci number
    
    The Fibonacci sequence is defined by the recurrence relation:

    Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
    Hence the first 12 terms will be:

        F1 = 1
        F2 = 1
        F3 = 2
        F4 = 3
        F5 = 5
        F6 = 8
        F7 = 13
        F8 = 21
        F9 = 34
        F10 = 55
        F11 = 89
        F12 = 144
        
    The 12th term, F12, is the first term to contain three digits.

    What is the index of the first term in the Fibonacci sequence to contain 
    1000 digits?
"""

PHI = 1.6180339887498948
LTFT = 0.34948500216  # log10(5) / 2

def find_digits(n):
    """ Iterate starting from n*10 < 0 to find digits.
    
        Parameter
          n : int : Number of digits to find.
        Return
          int : The F(N) that is first to equal n.
    """
    i = n*10
    result = 0
    while i > 0:
        digits = binet(i)
        i -= 1
        if digits < n:
            break
        elif digits == n:
            result = i
        else:
            i -= 1
    return result + 1   # 1+binet(n)

def binet(n):
    """ Calculate the digits by using Binet's Rule.
    
        http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/fibFormula.html
    """
    return math.ceil((n * math.log10(PHI)) - LTFT)

def fib(n):
    """ Use a recurrence formula to calculate the nth number. 
    
        If n is even then k = n/2
        F(n) = [2*F(k-1) + F(k)] * F(k)
        
        If n is odd then k = (n+1)/2
        F(n) = F(k)*F(k) + F(k-1)*F(k-1)
    """
    if n == 0:
        return 0
    if n == 1 or n == 2:
        return 1
        
    # Calculate f(k) and f(k-1) once for the equations.
    if n & 1 == 0:
        k = n // 2
    else:
        k = (n + 1) // 2
            
    fk = fib(k)
    fko = fib(k-1)
    
    if n & 1 == 0:
        return (2 * fko + fk) * fk
    return fk * fk + fko * fko

print("Results: " + str(find_digits(1000))) # 4782
