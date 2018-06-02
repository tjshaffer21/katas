(in-package #:project-euler.benchmarks)

(defvar *timer* (benchmark:make-timer))

(defun benchmark-digits ()
  (benchmark:with-sampling (*timer*)
    (dotimes (i 1000 t)
      (project-euler::digits (+ 1 i)))))
  
(defun benchmark-gcd-binary ()
  (benchmark:with-sampling (*timer*)
    (dotimes (i 1000 t)
      (project-euler::greatest-common-divisor 100 1000 :binary))))
  
(defun benchmark-integer-at ()
  (benchmark:with-sampling (*timer*)
      (dotimes (i 1000 t)
        (project-euler::integer-at 12345678901234567890 10))))
  
(defun benchmark-prime-mr ()
  (benchmark:with-sampling (*timer*)
    (iterate:iter
      (iterate:for i iterate::from 2 iterate::to 100)
      (project-euler::prime-miller-rabin i))))

(defun benchmark-prime-fermat ()
  (benchmark:with-sampling (*timer*)
    (iterate:iter
      (iterate:for i iterate::from 2 iterate::to 100)
      (project-euler::prime-fermat i))))
  
(defun run-benchmarks ()
  (dotimes (i 50 t) (benchmark-digits))
  (benchmark:report *timer*)
  (benchmark:reset *timer*)
  (dotimes (i 50 t) (benchmark-gcd-binary))
  (benchmark:report *timer*)
  (benchmark:reset *timer*)
  (dotimes (i 50 t) (benchmark-integer-at))
  (benchmark:report *timer*)
  (benchmark:reset *timer*)
  (dotimes (i 50 t) (benchmark-prime-mr))
  (benchmark:report *timer*)
  (benchmark:reset *timer*)
  (dotimes (i 50 t) (benchmark-prime-fermat))
  (benchmark:report *timer*)
  (benchmark:reset *timer*))