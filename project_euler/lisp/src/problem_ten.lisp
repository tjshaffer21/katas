;;;; problem_ten.lisp
;;;; Project Euler
;;;; Problem 10
;;;;
;;;; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17
;;;; Find the sum of all the primes below two million.
;;;;
(in-package #:project-euler)

(defun problem-ten (upto-n &optional (version :soe))
  (case version
    (:soe (reduce #'+ (sieve-of-erathosthenes upto-n)))
    ;; SoS calculates primes for 2n+2.
    ;; - n 3 because we want sum below n.
    (:sos (reduce #'+ (sieve-of-sundaram (- (/ upto-n 2) 3))))))

(defun problem-ten-main ()
  (format t "The sum of all primes below two million is ~S.~%"
          (problem-ten 2000000 :sos)))
