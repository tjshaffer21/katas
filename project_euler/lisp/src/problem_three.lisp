;;;; problem_three.lisp
;;;; Project Euler
;;;; Problem 3
;;;;
;;;; The prime factors of 13195 are 5, 7, 13, 29.
;;;; What is the largest prime factor of the number 600851475143?
;;;;
(in-package #:project-euler)

(defun prime-factors (value)
  "Get the prime factors of the given VALUE.
 Args
   VALUE - Integer of the prime factor to calculate.
 Return
   A list of prime factors."
  (iterate:iter
    (iterate:with i = 2)
    (iterate:with v = value)
    (iterate:while (> v 1))
    (cond ((= (mod v i) 0)
           (iterate::collect i)
           (setf v (/ v i)))
          (t (incf i)))))

(defun problem-three ()
  (reduce #'max (prime-factors 600851475143)))

(defun problem-three-main ()
  (format t "The largest prime factor of the number 600,851,475,143 is ~S.~%"
          (problem-three)))
