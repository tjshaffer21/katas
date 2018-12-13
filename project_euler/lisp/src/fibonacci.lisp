;;;; fibonacci.lisp
;;;;
;;;; Functions and macros involving the fibonacci sequence.
(in-package #:project-euler)

(defun dynamic-fibonacci (term &optional (partial nil))
  "Calculate the fibonacci sequence up to, and including, the term.

  Parameters
    term : int : TERM >= 0
    partial : array : Array of previously calculated primes.
  Return
    sequence : where size = term+1
    nil : TERM < 0.
  Error
    type-error : TERM must be an integer"
  (declare (type integer term)
           (optimize (speed 3) (safety 3) (debug 0)))

  (when (< term 0) (return-from dynamic-fibonacci nil))

  (let ((fibs (if (null partial)
                  (make-array (1+ term) :element-type 'integer :initial-element 0)
                  (make-array (1+ (- term (length partial))) :initial-element 0
                              :element-type 'integer)))
        (start 2))
        (if (null partial)
            (progn
              (setf (aref fibs 0) 0)
              (when (> term 0) (setf (aref fibs 1) 1)))
            (progn
              ;; Simplify the loop by setting the first two elements that
              ;; require access to previous array.
              (setf (aref fibs 0) (+ (aref partial (1- (length partial)))
                                     (aref partial (- (length partial) 2))))
              (setf (aref fibs 1) (+ (aref fibs 0)
                                     (aref partial (1- (length partial)))))
              (setf start (+ (length partial) 2))))
        (iterate:iter
          (iterate:for i iterate::from start iterate::to term)
          (if (null partial)
              (setf (aref fibs i) (+ (aref fibs (1- i)) (aref fibs (- i 2))))
              (setf (aref fibs (+ (- i start) 2))
                    (+ (aref fibs (1+ (- i start))) (aref fibs (- i start))))))
    (if (null partial)
        fibs
        (concatenate 'vector partial fibs))))

(defun even-sum-fibonacci (n)
  "Calculate the sum of even fibonacci terms using the recurence equation.

  Parameters
    n : int
  Return
    int
  Error
    type-error : N is not an integer"
  (declare (type integer n)
           (optimize (speed 3) (safety 3) (debug 0)))

  (iterate:iter
    (iterate:for even-1 iterate::initially 0 iterate::then even-2)
    (iterate:for even-2 iterate::initially 2 iterate::then even-3)
    (iterate:for even-3 iterate::initially 0 iterate::then (+ (* 4 even-2) even-1))
    (iterate:for sum iterate::initially 0 iterate::then (+ sum even-2))
    (iterate:while (< even-2 n))
    (when (> even-3 n) (iterate:finish))
    (iterate:finally (iterate::return sum))))

;;; Deprecated

(defun recursive-fibonacci (term)
  "Calculate the fibonacci number of the given integer.

   Parameters
    term : int : Non-negative, non-zero integer
   Return
    int
   Error
    simple-error : term < 0"
  (declare (type integer term))
  (cond ((< term 0) (error "Incorrect term."))
        ((= term 0) 0)
        ((= term 1) 1)
        (t (+ (recursive-fibonacci (- term 2)) (recursive-fibonacci (1- term))))))