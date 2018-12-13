;;;; problem-fourteen.lisp
;;;; Project Euler
;;;; Problem 14 - Longest Collatz sequence
;;;; URL: https://projecteuler.net/problem=14
;;;;
(in-package #:project-euler)

(defun ncollatz-length (n cache)
  "Determine the length of the collatz sequence for N.

  Parameters
    n : int
    cache : hash-table
  Side Effects
    cache is modified as necessary.
  Return
    int
  Error
    type-error : If N is not an integer.
               : If CACHE is not a hash table."
  (declare (type integer n)
           (type hash-table cache)
           (optimize (speed 3) (safety 3) (debug 0)))
  (if (= n 1)
      (progn
        (unless (gethash 1 cache) (setf (gethash 1 cache) 1))
        1)
      (let ((len (gethash n cache)))
        (if len
            len
            (progn
              (setf (gethash n cache)
                    (1+ (ncollatz-length (next-collatz-term n) cache))))))))

(defun next-collatz-term (n)
  "Calculate the next collatz term for N.

  Paramters
    n : int
  Return
    int
    nil : Invalid integer N.
  Error
    type-error : if N is not an integer."
  (declare (type integer n))
  (cond ((= n 1) 1) ; Collatz sequences are said to finish at 1.
        ((oddp n) (1+ (* 3 n)))
        ((evenp n) (ash n -1))
        (t nil)))

(defun problem-fourteen (&optional (limit 1000000))
  (iterate:iter
    (iterate:with longest = (list 1 1))
    (iterate:with cache = (make-hash-table :size limit))
    (iterate:for i iterate::from 1 iterate::below limit)
    (iterate:for term = (ncollatz-length i cache))
    (when (> term (first longest))
        (setf longest (list term i)))
    (iterate:finally (iterate::return (first (last longest))))))