;;;; common_functions.lisp
;;;; Functions that are used in multiple problems.
(in-package #:project-euler)

(defun digits (number)
  "Calculate the number of digits in a number.
  
   Assumption
    Base 10
   Parameters
    number : int : Non-negative, non-zero integer.
   Return
    int"
  (declare (type integer number))
  (if (= number 0) 1 (floor (1+ (log number 10)))))

(defun greatest-common-divisor (a b &optional (method :binary))
  "Find the gcd of integers a and b.
  
   Parameters
    a : int : Non-negative integer
    b : int : Non-negative integer
    method : keyword 
             :binary
   Return
    int; else nil if inputs are invalid."
  (declare (type integer a b) (type keyword method))
  (when (or (<= a 0) (<= b 0)) (return-from greatest-common-divisor nil))

  (case method
    (:binary (gcd-binary a b))))

(defun gcd-binary (a b)
  "Calculate the gcd of integers a and n using binary method.
  
   Parameters
    a : int : Non-negative integer
    b : int : Non-negative integer
   Return
    int; else nil if inputs are invalid."
  (declare (type integer a b))
  (when (or (<= a 0) (<= b 0)) (return-from gcd-binary nil))

  (let* ((an a) (bn b)
         (d (iterate:iter
                (iterate:with i = 0)
                (iterate:while (and (evenp an) (evenp bn)))
                (setf an (ash an -1))
                (setf bn (ash bn -1))
                (incf i)
                (iterate:finally (return i)))))
        (iterate:iter
            (iterate:while (not (eq an bn)))
            (cond ((evenp an) (setf an (ash an -1)))
                  ((evenp bn) (setf bn (ash bn -1)))
                  ((> an bn) (setf an (ash (- an bn) -1)))
                  (t (setf bn (ash (- bn an) -1)))))
        (* an (expt 2 d))))

(defun integer-at (value index)
  "Get the integer from value at the given index.

   Assumption
    Base 10.
   Note
    Index is 1-based and right to left.
   Example
    (integer-at 1234 1) := 4
    (integer-at 1234 4) := 1
   Parameters
    value : int : Non-negative Integer
    index : int : Search position
   Return
    (int at index, remainder ratio)
    nil : Index is incorrect 
          Value is negative"
  (declare (type integer value index))
  (when (or (< value 0) (<= index 0) (> index (digits value)))
    (return-from integer-at nil))
  (floor (mod (/ value (expt 10 (1- index))) 10)))

(defun multiples (number limit)
  "Get the multiples of the given number up to, and including, limit.
  
   Parameters
    number : int
    limit  : int 
   Return
    list; nil if number <= 0."
  (declare (type integer number limit))
  (unless (<= number 0)
    (iterate::iter
      (iterate::for i iterate::from (1+ number) iterate::to limit)
      (if (= (mod i number) 0) (iterate::collect i)))))