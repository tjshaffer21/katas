;;;; problem_four.lisp
;;;; Project Euler
;;;; Problem 4 - Largest palindrome product
;;;; URL: https://projecteuler.net/problem=4
;;;;
(in-package #:project-euler)

(defun palindromep (value)
  "Check if integer VALUe is a palindrome.

  Parameters
    value : int : Should be at least 3 or more digits.
  Return
    bool
  Errors
    type-error : if VALUE is not an integer."
  (declare (type integer value)
           (optimize (speed 3) (safety 3) (debug 0)))
  (iterate:iter
    (iterate:with digits = (1+ (log value 10)))
    (iterate:with reversed = (smath:reverse-integer value))
    (iterate:for i iterate::from 1 iterate::to digits)
    (iterate:if-first-time (when (< digits 3) (iterate:leave)))
    (iterate:always (= (smath:integer-at value i) (smath:integer-at reversed i)))))

(defun problem-four ()
  (iterate:iter
    (iterate:with result = 0)
    (iterate::with i = 999)
    (iterate::with j = 999)
    (iterate::with i-saved = 0) ; used to cut max iterations down.
    (iterate:for ij = (* i j))
    (iterate:for palindrome = (palindromep ij))
    (iterate::while (> j 100))
    (when palindrome
      (setf result (max result ij))
      (cond ((= i-saved 0) (setf i-saved i))
            ((and (> i i-saved) (> j i-saved)) (iterate::finish))))

      (if (< i 100)
          (setf j (setf i (1- j))) ; i == 99 then i = j--; j--
          (decf i))
  (iterate:finally (return result))))
