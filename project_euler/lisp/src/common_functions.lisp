;;;; common_functions.lisp
;;;; Functions that are used in multiple problems.
(in-package #:project-euler)

(defun digits (number)
  "Calculate the number of digits in a NUMBER.
 Assumption
   Base 10
 Args
   NUMBER - Non-negative integer.
 Return
   Integer"
  (declare (type integer number))

  (floor (1+ (log number 10))))

(defun greatest-common-divisor (a b &optional (method :binary))
  "Find the gcd of integers A and B.
 Args
   A - Non-negative integer
   B - Non-negative integer
   METHOD - Keyword of which gcd method to use.
     :binary
 Return
   nil if inputs are invalid;
   else an integer."
  (declare (type integer a b) (type keyword method))

  (when (or (<= a 0) (<= b 0)) (return-from greatest-common-divisor nil))

  (case method
    (:binary (gcd-binary a b))))

(defun gcd-binary (a b)
  "Calculate the gcd of integers A and B using binary method.
 Args
   A - Non-negative integer
   B - Non-negative integer
 Return
   nil if inputs are invalid;
   else integer."
  (declare (type integer a b))

  (when (or (<= a 0) (<= b 0)) (return-from gcd-binary nil))

  (let ((d 0))
    (iterate:iter
      (iterate:while (and (evenp a) (evenp b)))
      (setf a (/ a 2))
      (setf b (/ b 2))
      (incf d))

    (iterate:iter
      (iterate:while (/= a b))
      (cond ((evenp a) (setf a (/ a 2)))
            ((evenp b) (setf b (/ b 2)))
            ((> a b) (setf a (/ (- a b) 2)))
            (t (setf b (/ (- b a) 2)))))
    (* (expt 2 d))))

(defun integer-at (value index)
  "Get the integer from VALUE at INDEX.

 Assumption
   Base 10.
 Note
   Index is 1-based and right to left.
 Example
   (integer-at 1234 1) := 4
   (integer-at 1234 4) := 1
 Args
   VALUE - Non-negative Integer
   INDEX - Integer - Search position
 Return
   If INDEX is incorrect or VALUE is negative then return nil.
   Else, the first result is the integer at INDEX while the second value is the
   remaining numbers as a ratio."
  (declare (type integer value index))

  (when (or
         (< value 0)
         (<= index 0)
         (> index (digits value)))
    (return-from integer-at nil))

  (floor (mod (/ value (expt 10 (1- index))) 10)))

(defun multiples (number limit)
  "Get the multiples of the GIVEN number up to, and including, LIMIT.
 Args
   NUMBER - Integer. Number to get the multiples of.
   LIMIT  - Integer. Max number.
 Return
   List of integers.
   nil if number <= 0."
  (declare (type integer number limit))

  (unless (<= number 0)
    (iterate::iter
      (iterate::for i iterate::from (1+ number) iterate::to limit)
      (if (= (mod i number) 0) (iterate::collect i)))))

(defun primep (number &optional (test :fermat) (k 1))
  "Check if NUMBER is prime.
 Args
   NUMBER    - Positive integer to check if prime.
   TEST      - Method of checking primality.
     :fermat - Uses Fermat's equation.
     :mr     - Uses Miller-Rabin
   K         - Non-negative integer for accuracy testing.
 Return
   t is prime; else nil.
 Error
   simple-error if K <= 0."
  (declare (type integer number k) (type keyword test))

  (when (<= k 0) (error "Invalid input k: ~S" k))

  (when (or (<= number 0) (and (> number 2) (evenp number)))
    (return-from primep nil))

  (case test
    (:fermat (prime-fermat number k))
    (:mr (prime-miller-rabin number k))))

(defun prime-miller-rabin (number &optional (k 1))
  "Check if NUMBER is prime using Miller Rabin.
 Arg
   NUMBER - Non-negative integer.
   K - Accuracy for the test.
 Return
   nil if invalid input or not prime.
   t is prime.
 Error
   simple-error if K <= 0."
  (declare (type integer number k))

  (when (or (<= number 0) (and (> number 2) (evenp number)))
    (return-from prime-miller-rabin nil))
  (when (<= k 0) (error "Invalid input k: ~S" k))

  ;; Special case.
  ;; Number should be n > 3.
  (when (or (= number 2) (= number 3)) (return-from prime-miller-rabin t))

  ;; power-of-two
  ;; Calculate the powers of two for the integer N.
  ;; N - non-negative integer.
  ;; Returns a list with the first element the power of two and the second
  ;; element the remaining value.
  (flet ((power-of-two (n)
           (iterate:iter
             (iterate:for x iterate::initially 1 iterate::then (1+ x))
             (iterate:for v iterate::initially (/ n 2) iterate::then (/ v 2))
             (iterate:while (and (integerp v) (evenp v))) ; n % 2 = 0
             (iterate:finally (return (list x v))))))
    (let ((rd (power-of-two (1- number)))) ; n-1 := 2^r * d
      (iterate:iter
        (iterate:with a = (+ (random (- number 2)) 2)) ; [2, n-2]
        (iterate:with x = (mod (expt a (first (last rd))) number)) ; a^d mod n
        (iterate:repeat k)
        (cond ((or (= x 1) (= x (1- number)))
               (iterate:next-iteration))
              (t (iterate:iter ; Check if strong liar.
                   (iterate:repeat (1- (first rd))) ; r-1 times
                   (let ((nx (mod (* x x) number)))
                     (cond ((= nx 1)
                            (return-from prime-miller-rabin nil))
                           ((= nx (1- number))
                            (iterate:finish))))
                   (iterate:finally (iterate:next-iteration)))
                 (return-from prime-miller-rabin nil))))
      t)))

(defun prime-fermat (number &optional (k 1))
  "Check if NUMBER is prime using Fermat's test.
 Args
   NUMBER - Non-negative integer.
        K - Confidence value.
 Return
   nil if NUMBER <= 0.
   t is NUMBER is probably prime; nil if not prime.
 Error
   simple-error if K <= 0.
 Warn
   simple-warn if NUMBER > 5 digits."
  (declare (type integer number k))

  (when (<= k 0) (error "Invalid input k: ~S" k))

  (when (or (<= number 0) (and (> number 2) (evenp number)))
    (return-from prime-fermat nil))

  (let ((number-digits (digits number)))
    ;; Post-six digits, operations become highly inefficient.
    (when (>= number-digits 6) (warn "Number too large."))

    (iterate:iter
      (iterate:for a = (+ (random (- number 2)) 2)) ; [2, n-2]
      (iterate:repeat k)
      (if (/= (mod (expt a (1- number)) number) 1) ; a^n-1 % n
          (return-from prime-fermat nil)))
    t))

(defun sieve-of-erathosthenes (number)
  "Sieve of Erathosthenes algorithm.
 Args
    NUMBER - Integer to perform the sieve with
 Return
    If NUMBER is invalid then return nil;
    An integer list of all the primes up to n."
  (declare (type integer number))

  (when (or (<= number 1)
            (>= number array-total-size-limit))
    (return-from sieve-of-erathosthenes nil))

  (let* ((primes (make-array (- number 1)
                             :element-type 'bit :initial-element 1)))
    (iterate:iter
      (iterate:for i iterate::from 2 iterate::to (ceiling (sqrt number)))
      (when (primep i :mr)
        (iterate:iter
          (iterate:for j iterate:in (multiples i number))
          (setf (bit primes (- j 2)) 0))))

    ;; Convert bit array to an integer list.
    (iterate::iter
      (iterate:for i iterate::from 0 iterate::to (- number 2))
      (when (= (bit primes i) 1) (iterate::collect (+ i 2))))))

(defun sieve-of-sundaram (number)
  "Sieve of Sundaram algorithm.
 Generates all prime numbers below 2n+2.

 Args
   NUMBER - Non-negative integer.
 Return
   If NUMBER is invalid then return nil;
   An integer list."
  (declare (type integer number))

  (when (or (<= number 1) (>= number array-total-size-limit))
    (return-from sieve-of-sundaram nil))

  (let* ((index (- number 2))
         (primes (make-array index :element-type 'bit :initial-element 1)))
    (iterate:iter
      (iterate:for i iterate::from 1 iterate::to (/ index 2))
      (iterate:iter
        ;; k = i + j + 2ij; then shift - 2 to fit bit index.
        (iterate:for k = (- (+ i j (* 2 i j)) 2))
        (iterate:for j iterate::initially i iterate::then (1+ j))
        (iterate:while (< k index))
        (setf (bit primes k) 0)))

   (append (list 2 3) (iterate:iter
                        (iterate:for c iterate::initially 0 iterate::then
                                     (1+ c))
                        (iterate:while (< c index))
                        (when (= (bit primes c) 1)
                          ;; 2n+1; but, c(0..n] -> 2..n
                          (iterate::collecting (1+ (* 2(+ c 2)))))))))
