;;;; problem_twelve.lisp
;;;; Project Euler
;;;; Problem 12: Highly divisible triangular number
;;;; Problem URL: https://projecteuler.net/problem=12
;;;;
(in-package #:project-euler)

(defun count-divisors (x)
  "Count divisors of X.

  Parameters
    x : integer : x > 0
  Return
    integer"
  (declare (type (unsigned-by 32) x)
           (optimize (speed 3) (safety 0)))
  (when (= x 1) (return-from count-divisors 1))

  (iterate:iter
    (iterate:for n in (prime-factors-list x))
    (iterate:multiplying (1+ (first (last n))))))

(defun problem-twelve (max) ; max := 500 divisors
  (let ((y 0))
  (iterate:iter
    (iterate:for n iterate::from 1)
    (iterate:for tnc = (/ (* n (+ n 1)) 2))
    (iterate:until (>= (count-divisors tnc) max))
    (iterate:finally (iterate::return tnc)))))