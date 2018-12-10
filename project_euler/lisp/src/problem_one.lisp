;;;; problem_one.lisp
;;;; Project Euler
;;;; Problem 1 - Multiples of 3 and 5
;;;; URL: https://projecteuler.net/problem=1
;;;;
(in-package #:project-euler)

(defun problem-one (n)
  (let ((multiples-below #'(lambda (x y) (floor (1- x) y)))
        (ident #'(lambda (x k) (* x (/ (1+ x) 2) k))))
    (- (+ (funcall ident (funcall multiples-below n 3) 3)
          (funcall ident (funcall multiples-below n 5) 5))
       (funcall ident (funcall multiples-below n 15) 15))))