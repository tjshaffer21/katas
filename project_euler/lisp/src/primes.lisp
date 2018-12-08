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
           (optimize (speed 3) (safety 0)))

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
           (optimize (speed 3) (safety 0)))

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

(defun primep (n &optional (test :mr) (k 1))
  "Prime test on given N.

  See
    * Documentation for the specific functions.

  Parameters
    n : integer : n > 1
    test : keyword
    k : integer : confidence level.
  Return
    bool
  Error
    type-error : n,k must be integers
                 test must be keyword"
  (declare (type integer n k) (type keyword test))

  (case test
    (:fermat (prime-fermat n k))
    (:mr (prime-miller-rabin n k))))

(defun prime-miller-rabin (n &optional (k 1))
  "Check if N is a prime using Miller-Rabin.

  Parameters
    n : integer : n > 2
    k : integer : k >= 1
  Return
    bool
  Error
    type-error : n, k must be non-negative integers.
  Warn
    if k < 1"
  (declare (type integer n k)
           (optimize (speed 3) (safety 0)))

  (cond ((or (<= n 1) (and (> n 2) (evenp n)))
          (return-from prime-miller-rabin nil))
        ((or (= n 2) (= n 3))
          (return-from prime-miller-rabin t)))
  (iterate:iter
    (iterate:with mut-k = (if (< k 1)
                              (progn
                                (warn "setting k to 1.")
                                1)
                              k))
    (iterate:with rd = (iterate:iter ; factor power of twos
                          (iterate:for r iterate::from 1)
                          (iterate:for d iterate::initially (1- n)
                                         iterate::then (ash d -1))
                          (iterate:while (= (logand d 1) 0))
                          (iterate:finally (iterate::return (list r d)))))
    (iterate:for a = (+ (random (- n 2)) 2)) ; [2, n-2]
    (iterate:for x = (mod (expt a (first (last rd))) n)) ; a^d mod n
    (iterate:repeat mut-k) ; witness loop
    (unless (or (= x 1) (= x (1- n)))
      (iterate:iter
        (iterate:repeat (1- (first rd)))
        (setf x (mod (* x x) n))
        (when (= x (1- n)) (iterate:leave))
        (iterate:finally (return-from prime-miller-rabin nil)))))
  t)

(defun sundaram-sieve (n)
  "Sieve of Sundaram algorithm.

  Parameters
    n : integer : 1 < n < (array-total-size-limit+2) * 2
  Return
    list
    nil : invalid input
  Error
    type-error : N is not an integer."
  (declare (type integer n)
           (optimize (speed 3) (safety 0)))

  (let ((adjust (floor (- n 2) 2)))
    (when (or (<= adjust 1) (>= adjust array-total-size-limit))
      (return-from sundaram-sieve nil))
    (iterate:iter
      (iterate:with sieve = (make-array adjust :element-type 'bit :initial-element 1))
      (iterate:for i iterate::from 1 iterate::to adjust)
      (iterate:iter
        (iterate:for k = (- (+ i j (* 2 i j)) 2))
        (iterate:for j iterate::initially 1 iterate::then (1+ j))
        (iterate:while (< k adjust))
        (setf (bit sieve k) 0))
      (iterate:finally
        (iterate::return
          (append (list 2 3)
                  (iterate:iter
                    (iterate:for c iterate::initially 0 iterate::then (1+ c))
                    (iterate:while (< c adjust))
                    (when (= (bit sieve c) 1)
                      ;; 2n+1; but, c(0..n] -> 2..n
                      (iterate::collecting (1+ (* 2 (+ c 2))))))))))))

;;; Deprecrated
;;; Algorithms kept for convenience sake

(defun prime-fermat (number &optional (k 128))
  "Use Fermat's test to check if NUMBER is a prime within given K confident. If
  NUMBER is probably a prime then T is returned else NIL. A SIMPLE-ERROR is
  returned if K <= 0, and a SIMPLE-WARN is called if NUMBER > 5."
  (declare (type integer number k)
           (optimize (speed 3) (safety 0)))

  (cond ((<= k 0)
         (error "Invalid input k: ~S" k))
        ((or (<= number 0) (and (> number 2) (evenp number)))
         (return-from prime-fermat nil))
        ((or (= number 2) (= number 3))
         (return-from prime-fermat t)))

  (let ((number-digits (digits number)))
    ;; Post-six digits, operations become highly inefficient.
    (when (>= number-digits 6) (warn "Number too large."))

    (iterate:iter
      (iterate:for a = (+ (random (- number 2)) 2)) ; [2, n-2]
      (iterate:repeat k)
      (if (/= (mod (expt a (1- number)) number) 1) ; a^n-1 % n
          (return-from prime-fermat nil)))
    t))

(defun erathosthenes-sieve (number)
  "Sieve of Erathosthenes algorithm.

  Parameter
    number : int : 1 < NUMBER < array-total-size-limit
  Return
    list
    nil : invalid input
  Error
    type-error : NUMBER is not an integer."
  (declare (type integer number)
           (optimize (speed 3) (safety 0)))

  (when (or (<= number 1) (>= number array-total-size-limit))
    (return-from erathosthenes-sieve nil))
  (iterate:iter
    (iterate:with primes = (make-array (1- number) :element-type 'bit
                                                   :initial-element 1))
    (iterate:for i iterate::from 2 iterate::to (ceiling (sqrt number)))
    (when (primep i :mr)
          (iterate:iter
            (iterate:for j iterate:in (multiples i number))
            (setf (bit primes (- j 2)) 0)))
    (iterate:finally
      (iterate::return
        (iterate:iter
          (iterate:for i iterate::from 0 iterate::to (- number 2))
          (when (= (bit primes i) 1) (iterate::collect (+ i 2))))))))