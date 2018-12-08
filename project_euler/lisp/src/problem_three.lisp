;;;; problem_three.lisp
;;;; Project Euler
;;;; Problem 3 - Largest prime factor
;;;;
;;;; The prime factors of 13195 are 5, 7, 13, 29.
;;;; What is the largest prime factor of the number 600851475143?
;;;;
(in-package #:project-euler)

(defun problem-three (n)
  (reduce #'max (prime-factors n)))
