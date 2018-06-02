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

  (when (or
         (< value 0)
         (<= index 0)
         (> index (digits value)))
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

(defun primep (number &optional (test :fermat) (k 1))
  "Check if number is prime.
  
   Parameters
    number : int     : Non-negative integer
    test   : keyword : Primality test
                        :fermat - Uses Fermat's equation.
                        :mr     - Uses Miller-Rabin
    k      : int     : Non-negative - accuracy value
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
  "Check if number is prime using Miller Rabin.
  
   Parameters
    number : int : Non-negative integer.
    k      : int : Accuracy for the test.
   Return
    t is prime; else nil.   Error
    simple-error if K <= 0."
  (declare (type integer number k))
  
  (when (<= k 0) (error "Invalid input k: ~S" k))
  (when (or (<= number 0) (and (> number 2) (evenp number)))
    (return-from prime-miller-rabin nil))

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
             (iterate:for v iterate::initially (ash n -1) iterate::then (ash v -1))
             (iterate:while (and (integerp v) (evenp v)))
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
  "Check if number is prime using Fermat's test.
  
   Parameters
    number : int : Non-negative integer.
    k      : int : Confidence value.
   Return
    t is number is probably prime; nil if not prime.
    nil if number <= 0.
   Error
    simple-error if K <= 0.
   Warn
    simple-warn if number > 5 digits."
  (declare (type integer number k))

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

(defun sieve-of-erathosthenes (number)
  "Sieve of Erathosthenes algorithm.
  
   Parameters
    number : int
   Return
    list; else nil if number is invalid "
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

   Parameters
    number : int : Non-negative integer.
   Return
    list; nil if number is invalid."
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
                          (iterate::collecting (1+ (* 2 (+ c 2)))))))))
