;;;; P99 implemented using lisp.
;;;; Arithmetic problems.
(defun primep (n &optional (k 128))
  "Check if N is prime using confidence check K. Returning a bool."
  (declare (type integer n k))
  (cond ((or (<= n 1) (and (> n 2) (evenp n)))
         (return-from primep nil))
        ((or (= n 2) (= n 3)) (return-from primep t)))

  (flet ((calculate-rd (possible-prime)
            (loop for r from 1
                  for d = (1- possible-prime) then (ash d -1)
                  while (= (logand d 1) 0)
                  finally (return (list r d)))))
    (loop for ck from k downto 0
          for rd = (calculate-rd n)
          for a = (+ (random (- n 2)) 2)
          for x = (mod (expt a (first (last rd))) n)
          unless (or (= x 1) (= x (1- n))) do
            (loop named prime? for r = (1- (first rd)) then (1- r)
                  until (= r 0) do
                    (setf x (mod (* x x) n))
                    (when (= x (1- n)) (return-from prime? nil))
                  finally (return-from primep nil)))
    t))

(defun p31 (n)
  "Determine whether a given integer number is prime."
  (primep n))

(defun p32 (n m)
  "Determine the greatest common divisor o two positive integer numbers."
  (let ((copy-n (if (< n m) m n))
        (copy-m (if (>= n m) m n)))

    (loop for r = (rem copy-n copy-m)
          for s from 1
          while (or (not (= r 0)) (> s 1000))
          when (= r 0) do (return-from p32 copy-m)
          do
            (setf copy-n copy-m)
            (setf copy-m r))
    copy-m))