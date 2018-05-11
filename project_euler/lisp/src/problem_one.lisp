;;;; problem_one.lisp
;;;; Project Euler
;;;; Problem 1
;;;;
;;;; If we list all the natural numbers below 10 that are multiples of 3 or 5,
;;;; we get 3,5,6, and 9. The sum of these multiples is 23.
;;;;
;;;; Find the sum of all multiples of 3 or 5 below 1000.
;;;;
(in-package #:project-euler)

(defun multiples-below (numbers limit)
  "Find multiples of NUMBERS below (exclusive) LIMIT.
 Args
   NUMBERS - List of numbers to find multiples of.
     LIMIT - The integer limit of the search (exclusive).
 Return
   A list of integers.
     The list is neither sorted nor removes duplicates."
  (declare (type list numbers) (type integer limit))
  
  (unless (null numbers)
    (append (iterate:iter
              (iterate:for i iterate::from (first numbers) iterate::below limit)
              (when (= (mod i (first numbers)) 0)
                (iterate:collect i)))
            (multiples-below (rest numbers) limit))))

(defun problem-one ()
  (apply #'+ (remove-duplicates (multiples-below (list 3 5) 1000))))

(defun problem-one-main ()
  "Print the solution for the problem."
  (format t "The sum of all multiples of 3 or 5 below 1000 is ~S~%"
          (problem-one)))
