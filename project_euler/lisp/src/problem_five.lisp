;;;; problem_five.lisp
;;;; Project Euler
;;;; Problem 5 - Smallest multiple	
;;;;
;;;; 2520 is the smallest number that can be divided by each of the numbers from
;;;; 1 to 10 without any remainder.
;;;;
;;;; What is the smallest positive number that is evenly divisible by all the
;;;; numbers from 1 to 20?
(in-package #:project-euler)

(defun multiples-below (numbers limit)
  "Find multiples of numbers below the limit.
 
   Args
    numbers : list 
    limit   : int
   Return
    list : Non-sorted, possible duplicates."
  (declare (type list numbers) (type integer limit))
  (unless (null numbers)
    (append (iterate:iter
              (iterate:for i iterate::from (first numbers) iterate::below limit)
              (when (= (mod i (first numbers)) 0)
                (iterate:collect i)))
            (multiples-below (rest numbers) limit))))

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
  (declare (type integer value))
  
  (remove-if #'(lambda (x) (if (> (length (multiples-below (list x) (1+ value))) 1)
                               t
                               nil))
             (iterate:iter
               (iterate::for i iterate::from 2 iterate::to value)
               (iterate::collect i))))

#|
  TODO 
    + Make more efficient 
    + Test generate-list
|#
(defun problem-five ()
  ;; Remove lower values if their multiples exist in the list.
  (let ((lst (generate-list 20)) (limit 1000000000))
    ;; Start at 20 since < 20 cannot be divisible by 20.
    ;; Non-even values can be ignored.
    ;; Values that are not divisible by 10 can be ignored (since 5 is not
    ;; divisble by 2).
    (iterate:iter
      (iterate:for i iterate::from 20 iterate::to limit iterate::by 20)
      (when (iterate:iter
              (iterate:with flag = nil)
              (iterate:for j iterate:in lst)
              (when (not (= (mod i j) 0))
                (setf flag t)
                (iterate::finish))
              (iterate::finally (when (null flag) (return t))))
        (return i))
      (iterate::finally (return 0)))))
