;;;; problem_sixteen.lisp
;;;; Project Euler
;;;; Problem 16 - Power digit sum
;;;; URL: https://projecteuler.net/problem=16
;;;;
(in-package #:project-euler)

(defun problem-sixteen (n)
  (reduce #'+ (integer-into-sequence (expt 2 n))))