;;;; problem_four.lisp
;;;; Project Euler
;;;; Problem 4 - Largest palindrome product
;;;;
;;;; A palindromic number reads the same both ways. The largest palindrome made
;;;; from both the product of two 2-digit numbers is 9009 = 91 x 99.
;;;;
;;;; Find the largest palindrome made from the product of two 3-digit numbers.
;;;;
(in-package #:project-euler)

(defun reverse-integer (n)
  "Reverse the integer N.

  Parameters
    n : integer
  Return
    integer
  Error
    type-error : if N is not an integer."
  (declare (type integer n)
           (optimize (speed 3) (safety 0)))

  (when (and (>= n -9) (<= n 9)) (return-from reverse-integer n))
  (labels ((nrev (x res)
            (if (= x 0)
                res
                (multiple-value-bind (m n)
                  (floor x 10)
                  (nrev m (+ (* res 10) n))))))
    (let ((result (nrev (abs n) 0)))
      (if (< n 0) (- result) result))))

(defun palindromep (value)
  "Check if integer VALUe is a palindrome.

  Parameters
    value : int : Should be at least 3 or more digits.
  Return
    bool
  Errors
    type-error : if VALUE is not an integer."
  (declare (type integer value)
           (optimize (speed 3) (safety 0)))
  (iterate:iter
    (iterate:with digits = (1+ (log value 10)))
    (iterate:with reversed = (reverse-integer value))
    (iterate:for i iterate::from 1 iterate::to digits)
    (iterate:if-first-time (when (< digits 3) (iterate:leave)))
    (iterate:always (= (integer-at value i) (integer-at reversed i)))))

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
