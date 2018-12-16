(in-package #:project-euler.benchmarks)

(defvar *timer* (benchmark:make-timer))

(defmacro do-benchmark-report (sample-size &rest body)
  `(progn
    (benchmark:reset *timer*)
    (dotimes (i ,sample-size t)
      (benchmark:with-sampling (*timer*)
        ,@body))
    (benchmark:report *timer*)))

(defun benchmark-prime-factors (sample-size n)
  (do-benchmark-report sample-size (project-euler::prime-factors n)))

(defun benchmark-prime-factors-list (sample-size n)
  (do-benchmark-report sample-size (project-euler::prime-factors-list n)))