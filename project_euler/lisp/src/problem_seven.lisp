;;;; problem_seven.lisp
;;;; Project Euler
;;;; Problem 7 - 10001st prime
;;;;
;;;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
;;;; that the 6th prime is 13.
;;;;
;;;; What is the 10001st prime number?
;;;;
(in-package #:project-euler)

(defun problem-seven (n)
  (iterate:iter
    (iterate:for current iterate::first (* 2 n) iterate::then (+ current n))
    (iterate:for sieve = (erathosthenes-sieve current))
    (iterate:while (< current most-positive-fixnum))
    (when (>= (length sieve) n)
          (return-from problem-seven (nth (1- n) sieve)))))
