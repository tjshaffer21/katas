;;;; problem_sixteen.lisp
;;;; Project Euler
;;;; Problem 16 - Power digit sum
;;;;
;;;;
;;;; 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
;;;;
;;;; What is the sum of the digits of the number 2^1000?
(in-package #:project-euler)

(defun problem-sixteen (n)
  (iterate:iter
    (iterate:for i iterate::from 1)
    (iterate:for digit iterate::first 0 iterate::then (integer-at (expt 2 n) (1- i)))
    (iterate:while digit)
    (unless (null digit) (iterate:sum digit))))