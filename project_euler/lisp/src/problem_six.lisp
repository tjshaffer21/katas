;;;; problem_six.lisp
;;;; Project Euler
;;;; Problem 6 - Sum square difference	
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
  
   Parameters
    number : int : nth value to sum.
   Return
    int"
  (declare (type integer number))
  (iterate:iter
    (iterate:for i iterate::from 1 iterate::to number)
    (iterate::sum (* i i))))

(defun square-of-sum (number)
  "Square the sum.

   Parameters
    number : int : nth value to sum.
   Return
    int"
  (declare (type integer number))
  (expt (iterate:iter
         (iterate::for i iterate::from 1 iterate::to number)
         (iterate::sum i)) 2))

(defun problem-six (n)
 (- (square-of-sum n) (sum-of-squares n)))
