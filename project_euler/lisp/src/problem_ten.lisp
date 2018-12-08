;;;; problem_ten.lisp
;;;; Project Euler
;;;; Problem 10 - Summation of primes
;;;;
;;;; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17
;;;; Find the sum of all the primes below two million.
;;;;
(in-package #:project-euler)

(defun problem-ten (upto-n &optional (version :soe))
  (case version
    (:soe (reduce #'+ (erathosthenes-sieve upto-n)))
    (:sos (reduce #'+ (sundaram-sieve upto-n)))))