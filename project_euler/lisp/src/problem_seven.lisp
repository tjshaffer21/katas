;;;; problem_seven.lisp
;;;; Project Euler
;;;; Problem 7 - 10001st prime
;;;; URL: https://projecteuler.net/problem=6
;;;;
(in-package #:project-euler)

(defun problem-seven (n)
  (iterate:iter
    (iterate:for current iterate::first (* 2 n) iterate::then (+ current n))
    (iterate:for sieve = (erathosthenes-sieve current))
    (iterate:while (< current most-positive-fixnum))
    (when (>= (length sieve) n)
          (return-from problem-seven (nth (1- n) sieve)))))
