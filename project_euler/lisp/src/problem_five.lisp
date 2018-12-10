;;;; problem_five.lisp
;;;; Project Euler
;;;; Problem 5 - Smallest multiple
;;;; URL: https://projecteuler.net/problem=5
;;;;
(in-package #:project-euler)

(defun multiples-below (n limit)
  "Find multiples of N below the LIMIT.

  Parameters
    n : list
    limit : int
  Return
    list : Non-sorted, possible duplicates.
  Error
    type-error : if N is not a list, and LIMIT is not an integer."
  (declare (type list n) (type integer limit)
           (optimize (speed 3) (safety 0)))
  (unless (null n)
    (append (iterate:iter
              (iterate:for i iterate::from (first n) iterate::below limit)
              (when (= (mod i (first n)) 0)
                (iterate:collect i)))
            (multiples-below (rest n) limit))))

;;; TODO Better function name
(defun generate-list (value)
  "Generate a list of integers from 1 to value; where values that have multiples
   existing in the list are removed.

   Example
    (generate-list 20) := (11 12 13 14 15 16 17 18 19 20)
    (generate-list 10) := (6 7 8 9 10)
   Parameters
    value : int : maximum value
   Return
    list"
  (declare (type integer value)
           (optimize (speed 3) (safety 0)))

  (remove-if #'(lambda (x) (if (> (length (multiples-below (list x) (1+ value))) 1)
                               t
                               nil))
             (iterate:iter
               (iterate::for i iterate::from 2 iterate::to value)
               (iterate::collect i))))

(defun problem-five (&optional (n 20) (limit 1000000000))
    (iterate:iter
      (iterate:with lst = (generate-list n))
      (iterate:for i iterate::from n iterate::to limit iterate::by n)
      (when (iterate:iter
              (iterate:for j iterate:in lst)
              (iterate:always (= (mod i j) 0)))
        (return i))
      (iterate::finally (return 0))))
