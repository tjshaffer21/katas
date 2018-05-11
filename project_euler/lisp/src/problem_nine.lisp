;;;; problem_nine.lisp
;;;; Project Euler
;;;; Problem 9
;;;;
;;;; A Pythagorean triplet is a set of three natural numbers, a < b < c, for
;;;; which, a^2 + b^2 = c^2
;;;;
;;;; For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
;;;;
;;;; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;;;; Find the product abc.
;;;;
(in-package #:project-euler)

(defun euclid-pythagorean-triple (m n k)
  "Generate a triplet using Euclid's formula.
 https://en.wikipedia.org/wiki/Pythagorean_triple#Generating_a_triple

 Args
   M - Non-negative Integer
   N - Non-negative Integer
   K - Non-negative Integer. K is the value used for creating multiples of
       coprimes.
 Return
   Multiple integer values (a, b, c)
   nil if m <= n <= 0"
  (declare (type integer m n k))

  (unless (and (> m n) (> n 0)) (return-from euclid-pythagorean-triple nil))

  (when (< k 1) (setf k 1))

  (values (* k (- (* m m) (* n n)))
          (* k (* 2 m n))
          (* k (+ (* m m) (* n n)))))

(defun pythagorean-triplet-p (a b c)
  "Validate if the numbers A, B, and C are a pythagorean triplet.
 Args
   A - Non-negative Integer
   B - Non-negative Integer
   C - Non-negative Integer
 Return
   t if a triplet; else nil."
  (declare (type integer a b c))
  
  (if (= (+ (* a a) (* b b)) (* c c)) t nil))

(defun generate-coprimes (max)
  "Generate all coprime pairs up to MAX.
 Args
   MAX - Integer. Limit of generation.
 Return
   Sequence of integers.
   nil if max <= 0"
  (declare (type integer max))
  (when (<= max 0) (return-from generate-coprimes nil))

  (let ((coprimes (make-array (+ max 2)))
        (m 2)
        (n 1))
    (iterate:iter
      (iterate::for i iterate::from 0 iterate::to max iterate::by 3)
      (setf (aref coprimes i) (list (- (* 2 m) n) m))
      (setf (aref coprimes (1+ i)) (list (+ (* 2 m) n) m))
      (setf (aref coprimes (+ i 2)) (list (+ m (* 2 n)) n))

      (setf m (1+ m))
      (setf n (1+ n)))
    coprimes))
    
(defun problem-nine ()
  (let ((coprimes (generate-coprimes 100)))
    (iterate::iter
      (iterate::for k iterate::from 1 iterate::to 100)
      (iterate::iter
        (iterate:for i iterate::in-vector coprimes)
        (multiple-value-bind (a b c) (euclid-pythagorean-triple (first i)
                                                                (first (last i))
                                                                k)
          (when (pythagorean-triplet-p a b c)
            (if (= (+ a b c) 1000) (return-from problem-nine (* a b c)))))))))

(defun problem-nine-main ()
  (format t "The product abc is ~S.~%" (problem-nine)))
