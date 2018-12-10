;;;; problem_three.lisp
;;;; Project Euler
;;;; Problem 3 - Largest prime factor
;;;; URL: https://projecteuler.net/problem=3
;;;;
(in-package #:project-euler)

(defun problem-three (n)
  (reduce #'max (prime-factors n)))
