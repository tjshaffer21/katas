;;;; problem_one.lisp
;;;; Project Euler
;;;; Problem 1 - Multiples of 3 and 5
;;;;
;;;; If we list all the natural numbers below 10 that are multiples of 3 or 5,
;;;; we get 3,5,6, and 9. The sum of these multiples is 23.
;;;;
;;;; Find the sum of all multiples of 3 or 5 below 1000.
;;;;
(in-package #:project-euler)

(defun problem-one (n)
  (let ((multiples-below #'(lambda (x y) (floor (1- x) y)))
        (ident #'(lambda (x k) (* x (/ (1+ x) 2) k))))
    (- (+ (funcall ident (funcall multiples-below n 3) 3)
          (funcall ident (funcall multiples-below n 5) 5))
       (funcall ident (funcall multiples-below n 15) 15))))