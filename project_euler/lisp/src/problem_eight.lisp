;;;; problem_eight.lisp
;;;; Probject Euler
;;;; Problem 8 - Largest product in a series
;;;;
;;;; URL: https://projecteuler.net/problem=8
;;;;
(in-package #:project-euler)

(defun integer-into-sequence (number)
  "Parse an integer into a sequence.

  Parameters
    number : int : NUMBER >= 0
  Return
    A sequence of integers.
    nil if NUMBER is negative.
  Errors
    type-error : if NUMBER is not an integer."
  (declare (type integer number)
           (optimize (speed 3) (safety 0)))

  (when (< number 0) (return-from integer-into-sequence nil))

  (let* ((size (floor (1+ (log number 10))))
         (integers (make-array size)))
    (when (= size 1)
      (return-from integer-into-sequence
        (make-array 1 :initial-element number :element-type 'integer)))

    (iterate:iter
      (iterate:for j iterate::from 0)
      (iterate:for i iterate::from (1- size) iterate::downto 0)
      (setf (aref integers j) (integer-at number (1+ i))))
    integers))

(defun problem-eight (&optional (n 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450)
                                (steps 13))
  (iterate:iter
    (iterate:with num-seq = (integer-into-sequence n))
    (iterate:for i iterate::from 0 iterate::to (- (length num-seq) steps 1))
    (iterate:maximizing (reduce #'* (subseq num-seq i (+ i steps))))))