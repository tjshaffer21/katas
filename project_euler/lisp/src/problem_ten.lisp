;;;; problem_ten.lisp
;;;; Project Euler
;;;; Problem 10 - Summation of primes
;;;; URL: https://projecteuler.net/problem=10
;;;;
(in-package #:project-euler)

(defun problem-ten (upto-n &optional (version :soe))
  (case version
    (:soe (reduce #'+ (erathosthenes-sieve upto-n)))
    (:sos (reduce #'+ (sundaram-sieve upto-n)))))