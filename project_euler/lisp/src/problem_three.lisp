;;;; problem_three.lisp
;;;; Project Euler
;;;; Problem 3 - Largest prime factor	
;;;;
;;;; The prime factors of 13195 are 5, 7, 13, 29.
;;;; What is the largest prime factor of the number 600851475143?
;;;;
(in-package #:project-euler)

(defun prime-factors (factor)
  "Get the prime factors of the given integer.
 
   Parameters
    factor : int
   Return
    list"
  (declare (type integer factor))
  (iterate:iter
    (iterate:with i = 2)
    (iterate:with v = factor)
    (iterate:while (> v 1))
    (if (= (mod v i) 0)
        (progn (iterate::collect i)
               (setf v (/ v i)))
        (incf i))))

(defun problem-three (n)
  (reduce #'max (prime-factors n)))
