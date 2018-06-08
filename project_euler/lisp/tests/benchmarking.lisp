(in-package #:project-euler.benchmarks)

(defvar *timer* (benchmark:make-timer))

(defun benchmark-digits (limit)
  (benchmark:with-sampling (*timer*)
    (dotimes (i limit t)
      (project-euler::digits (+ 1 i)))))
      
(defun benchmark-even-sum-fib (term limit)
  (benchmark:with-sampling (*timer*)
    (dotimes (i limit t)
      (project-euler::even-sum-fibonacci term))))  
  
(defun benchmark-recursive-fibonacci (term limit)
  (benchmark:with-sampling (*timer*)
    (dotimes (i limit t)
      (project-euler::recursive-fibonacci term))))
      
(defun benchmark-dynamic-fibonacci(term limit)
  (benchmark:with-sampling (*timer*)
    (dotimes (i limit t)
      (project-euler::dynamic-fibonacci term))))

(defun benchmark-gcd-binary (limit)
  (benchmark:with-sampling (*timer*)
    (dotimes (i limit t)
      (project-euler::greatest-common-divisor 100 1000 :binary))))
  
(defun benchmark-integer-at (limit)
  (benchmark:with-sampling (*timer*)
      (dotimes (i limit t)
        (project-euler::integer-at most-positive-fixnum 10))))
  
(defun benchmark-prime-mr (limit)
  (benchmark:with-sampling (*timer*)
    (iterate:iter
      (iterate:for i iterate::from 2 iterate::to limit)
      (project-euler::prime-miller-rabin i))))

(defun benchmark-prime-fermat (limit)
  (benchmark:with-sampling (*timer*)
    (iterate:iter
      (iterate:for i iterate::from 2 iterate::to limit)
      (project-euler::prime-fermat i))))