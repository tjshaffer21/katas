;;;;primes.lisp
;;;;
;;;; Functions relating to primes.
(in-package #:project-euler)

;;; Algorithm: Prime Factorization using trial division.
(defun prime-factors (n)
  "Get the prime factors of the given integer N.

  If n is prime then a list with only the prime number is returned.

  Parameters
    n : int
  Return
    list
    nil : N <= 1
  Error
    type-error : If N is not an integer"
  (declare (type integer n)
           (optimize (speed 3) (safety 3) (debug 0)))

  (when (<= n 1) (return-from prime-factors nil))
  (let ((mut-n n))
    (append (iterate:iter
              (iterate:while (= (mod mut-n 2) 0))
              (iterate:collecting 2)
              (setf mut-n (/ mut-n 2)))
            (iterate:iter
              (iterate:with x = 3)
              (iterate:until (> (* x x) mut-n))
                (if (= (mod mut-n x) 0)
                    (progn
                      (iterate:collecting x)
                      (setf mut-n (/ mut-n x)))
                    (setf x (+ x 2))))
            (if (not (= mut-n 1)) (list mut-n)))))

;;; Algorithm: Prime Factorization using trial division.
(defun prime-factors-list (n)
  "Get a list of 'tuples' for the prime factors.

  Parameters
    n : int : n > 1
  Return
    list of lists
    nil : special case should n == 1.
  Error
    type-error : If N is not an integer."
  (declare (type integer n)
           (optimize (speed 3) (safety 3) (debug 0)))

  (when (<= n 1) (return-from prime-factors-list nil))
  (let ((mut-n n))
    (append (iterate:iter
              (iterate:for i iterate::from 0)
                (iterate:while (= (mod mut-n 2) 0))
                (setf mut-n (/ mut-n 2))
                (iterate:finally
                  (iterate::return (unless (= i 0) (list (list 2 i))))))
            (iterate:iter
              (iterate:with i = 0)
              (iterate:with x = 3)
              (iterate:until (> (* x x) mut-n))
              (if (= (mod mut-n x) 0)
                  (progn
                    (setf mut-n (/ mut-n x))
                    (incf i))
                  (progn
                    (unless (= i 0)
                      (iterate:collect (list x i) iterate::into factors)
                      (setf i 0))
                    (setf x (+ x 2))))
              (iterate:finally
                (iterate::return
                  (unless (= i 0)
                    ;; Case: If x == mut-n then there is an existing value for x
                    ;;       to be handled, and mut-n needs to be set to stop
                    ;;       the final case.
                    (if (not (= x mut-n))
                        (append factors (list (list x i)))
                        (progn
                          (setf mut-n 1)
                          (append (list (list x (1+ i))))))))))
            ;; Case when mut-n is a prime.
            (when (not (= mut-n 1)) (list (list mut-n 1))))))

;;; Deprecrated
;;; Algorithms kept for convenience sake

(defun prime-fermat (number &optional (k 128))
  "Use Fermat's test to check if NUMBER is a prime within given K confident. If
  NUMBER is probably a prime then T is returned else NIL. A SIMPLE-ERROR is
  returned if K <= 0, and a SIMPLE-WARN is called if NUMBER > 5."
  (declare (type integer number k)
           (optimize (speed 3) (safety 3) (debug 0)))

  (cond ((<= k 0)
         (error "Invalid input k: ~S" k))
        ((or (<= number 0) (and (> number 2) (evenp number)))
         (return-from prime-fermat nil))
        ((or (= number 2) (= number 3))
         (return-from prime-fermat t)))

  (let ((number-digits (smath:digits number)))
    ;; Post-six digits, operations become highly inefficient.
    (when (>= number-digits 6) (warn "Number too large."))

    (iterate:iter
      (iterate:for a = (+ (random (- number 2)) 2)) ; [2, n-2]
      (iterate:repeat k)
      (if (/= (mod (expt a (1- number)) number) 1) ; a^n-1 % n
          (return-from prime-fermat nil)))
    t))
