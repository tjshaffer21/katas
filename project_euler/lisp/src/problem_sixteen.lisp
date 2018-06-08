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
  (let ((power (expt 2 n)))
    (iterate:iter 
      (iterate:with digit = 0)
      (iterate:for i iterate::initially 1 iterate::then (1+ i))
      (iterate:while digit)
      (setf digit (integer-at power i))
      (unless (null digit) (iterate:sum digit)))))