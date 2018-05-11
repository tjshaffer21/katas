;;;; problem_six.lisp
;;;; Project Euler
;;;; Problem 6
;;;;
;;;; The sum of the squares of the first ten natural numbers is,
;;;;   1^2 + 2^2 + ... + 10^2 = 385
;;;; The square of the sum of the first ten natural numbers is,
;;;;   (1 + 2 + ... + 10)^2 = 55^2 = 3025
;;;; Hence the difference between the sum of the sqares of the first ten natural
;;;; numbers and the square of the sum is 3025 - 385 = 2640.
;;;; Find the difference between the sum of the squares of the first one hundred
;;;; natural numbers and the square of the sum.
;;;;
(in-package #:project-euler)

(defun sum-of-squares (number)
  "Calculate the sum of squares.
 Args
   NUMBER - Integer. The nth value to sum.
 Return
   Integer"
  (declare (type integer number))
  
  (iterate:iter
    (iterate:for i iterate::from 1 iterate::to number)
    (iterate::sum (* i i))))

(defun square-of-sum (number)
  "Square the sum.
 Args
   NUMBER - Integer. The nth value to sum.
 Return
   Integer"
  (declare (type integer value))
  
  (expt (iterate:iter
         (iterate::for i iterate::from 1 iterate::to value)
         (iterate::sum i)) 2))

(defun problem-six ()
 (- (square-of-sum 100)  (sum-of-squares 100)))

(defun problem-six-main ()
  (format t "The difference between the sum of the squares of the first one\
 hundred natural numbers and the square of the sum is ~S.~%" (problem-six)))