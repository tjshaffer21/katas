;;;; problem_seven.lisp
;;;; Project Euler
;;;; Problem 7
;;;;
;;;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
;;;; that the 6th prime is 13.
;;;;
;;;; What is the 10001st prime number?
;;;;
(in-package #:project-euler)

(defun problem-seven ()
  (iterate:iter
    (iterate:with current = 20000)
    (iterate:with max = 1000000)
    (iterate:while (< current max))
    (let ((sieve (sieve-of-erathosthenes current)))
      (if (>= (length sieve) 10001)
          (return-from problem-seven
            (nth 10000 sieve)) ; 10001 prime
          (setf current (+ current 10000))))))

(defun problem-seven-main ()
  (format t "The 10001st prime number is ~S.~%" (problem-seven)))
