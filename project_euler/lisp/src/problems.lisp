;;;; problems.lisp
;;;; Collections of problems that do not use unique helper functions.
(in-package #:project-euler)

;;; Problem 1 - Multiples of 3 and 5
;;; URL: https://projecteuler.net/problem=1
(defun problem-one (n)
  (let ((multiples-below #'(lambda (x y) (floor (1- x) y)))
        (ident #'(lambda (x k) (* x (/ (1+ x) 2) k))))
    (- (+ (funcall ident (funcall multiples-below n 3) 3)
          (funcall ident (funcall multiples-below n 5) 5))
       (funcall ident (funcall multiples-below n 15) 15))))

;;; Problem 2 - Even Fibonacci numbers
;;; URL: https://projecteuler.net/problem=2
(defun problem-two (limit) (smath:even-sum-fibonacci limit))

;;; Problem 3 - Largest prime factor
;;; URL: https://projecteuler.net/problem=3
(defun problem-three (n)
  (reduce #'max (prime-factors n)))

;;; Problem 7 - 10001st prime
;;; URL: https://projecteuler.net/problem=7
(defun problem-seven (n)
  (iterate:iter
    (iterate:for current iterate::first (* 2 n) iterate::then (+ current n))
    (iterate:for sieve = (smath:erathosthenes-sieve current))
    (iterate:while (< current most-positive-fixnum))
    (when (>= (length sieve) n)
          (return-from problem-seven (nth (1- n) sieve)))))

;;; Problem 8 - Largest product in a series
;;; URL: https://projecteuler.net/problem=8
(defun problem-eight (&optional (n 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450)
                                (steps 13))
  (iterate:iter
    (iterate:with num-seq = (smath:integer-into-sequence n))
    (iterate:for i iterate::from 0 iterate::to (- (length num-seq) steps 1))
    (iterate:maximizing (reduce #'* (subseq num-seq i (+ i steps))))))

;;; Problem 10 - Summation of primes
;;; URL: https://projecteuler.net/problem=10
(defun problem-ten (upto-n &optional (version :sos))
  (case version
    (:soe (reduce #'+ (smath:erathosthenes-sieve upto-n)))
    (:sos (reduce #'+ (smath:sundaram-sieve upto-n)))))

;;; Problem 16 - Power digit sum
;;; URL: https://projecteuler.net/problem=16
(defun problem-sixteen (n)
  (reduce #'+ (smath:integer-into-sequence (expt 2 n))))

;;; Problem 25 - 1000-digit Fibonacci Number
;;; URL: https://projecteuler.net/problem=25
(defun problem-twenty-five (&optional (d 1000))
  (iterate:iterate
    (iterate:for x iterate::from 1)
    (iterate:while (< (smath:digits (smath:fibonacci x)) d))
    (iterate:finally (iterate::return x))))

;;; Common Helper Functions