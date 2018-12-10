;;;; problem_six.lisp
;;;; Project Euler
;;;; Problem 6 - Sum square difference
;;;; URL: https://projecteuler.net/problem=6
;;;;
(in-package #:project-euler)

(defun sum-of-squares (number)
  "Calculate the sum of squares.

  Parameters
    number : int : nth value to sum.
  Return
    int
  Error
    type-error : NUMBER is not an integer"
  (declare (type integer number))
  (iterate:iter
    (iterate:for i iterate::from 1 iterate::to number)
    (iterate::sum (* i i))))

(defun square-of-sum (number)
  "Square the sum.

  Parameters
    number : int : nth value to sum.
  Return
    int
  Error
    type-error : NUMBER is not an integer"
  (declare (type integer number))
  (expt (iterate:iter
         (iterate::for i iterate::from 1 iterate::to number)
         (iterate::sum i)) 2))

(defun problem-six (n)
  (- (square-of-sum n) (sum-of-squares n)))
