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

(defun reverse-integer (value)
  "Reverse an integer.
 
   Parameters
    value : int 
   Return 
    int"
  (declare (type integer value))

  (let* ((flag (if (< value 0) t nil))
         (pos (if (< value 0) (- value) value))
         (res (iterate:iter
                (iterate:for result iterate::initially 0 iterate::then
                             (+ (* result 10) (mod x 10)))
                (iterate:for x iterate::initially pos iterate::then
                             (floor (/ x 10)))
                (iterate:while (> x 0))
                (iterate:finally (return result)))))
    (if flag (- res) res)))

(defun palindromep (value)
  "Check if integer is a palindrome.
  
   Parameters
    value : int : Should be at least 3 or more digits.
   Return
    t if palindrome; otherwise, nil."
  (declare (type integer value))
  (let ((digits (1+ (log value 10))))
    (when (<= digits 3) (return-from palindromep nil))

    (iterate:iter
      (iterate:with result = t)
      (iterate:with reversed = (reverse-integer value))
      (iterate:for i iterate::from 1 iterate::to digits)
      (when (not (= (integer-at value i) (integer-at reversed i)))
        (setf result nil)
        (iterate::finish))
      (iterate::finally (return result)))))

(defun problem-four ()
  (iterate:iter
    (iterate::with i = 999)
    (iterate::with j = 999)
    (iterate::with result = 0)
    ;; i-saved and continue are used to cut max iterations down.
    (iterate::with i-saved = 0)
    (iterate::with continue = t)
    
    ;; Problem Four only cares about 3 digits.
    (iterate::while (and continue (> j 100)))
    (let ((mult (* i j)))
      (when (palindromep mult)
        (cond ((= i-saved 0)
               (setf i-saved i)
               (setf result (max result (* i j))))
               ((and (> i i-saved) (> j i-saved))
                (setf result (max result (* i j)))
                (setf continue nil))
               (t (setf result (max result (* i j))))))

      ;; Problem Four only cares about 3-digits.
      ;; When i reaches 99 then reset to one less than j then decrement j.
      (if (< i 100)
          (progn 
             (setf i (- j 1))
             (setf j (- j 1)))
          (decf i))
      (iterate:finally (return result)))))
