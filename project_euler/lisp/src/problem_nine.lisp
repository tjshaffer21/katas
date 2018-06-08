;;;; problem_nine.lisp
;;;; Project Euler
;;;; Problem 9 - Special Pythagorean triplet	
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

#|
   To Do
     + This works to get the specific problem, but it is not fully implementing
       the formula.
     + Checking on k value needs to be handled.
 |#
(defun generate-pythagorean-triple (m n &optional (method :euclid) (k 1))
  "Generate a triplet.
   https://en.wikipedia.org/wiki/Pythagorean_triple#Generating_a_triple
   
   Parameters
    m : int : Integer > 0.
    n : int : Integer > 0.
    k : int : Integer > 0.
              The value used for creating multiples of coprimes.
   Return
     Multiple integer values (a, b, c)
     nil if m <= n <= 0"
  (declare (type integer m n k) (type keyword method))
  (unless (and (> m n) (> n 0)) (return-from generate-pythagorean-triple nil))
  
  (case method
    (:euclid (values (* m m) (* 2 m n) (+ (* m m) (* n n))))
    (:meuclid (values (* k (- (* m m) (* n n))) ; No idea what the actual name is.
                      (* k (* 2 m n))
                      (* k (+ (* m m) (* n n)))))))

(defun pythagorean-triplet-p (a b c)
  "Validate if the numbers a, b, and c are a pythagorean triplet.
 
   Parameters
    a : int : Integer > 0.
    b : int : Integer > 0.
    c : int : Integer > 0.
   Return
    t if a triplet; else nil."
  (declare (type integer a b c))
  (if (= (+ (* a a) (* b b)) (* c c)) t nil))

(defun generate-coprimes (limit)
  "Generate all coprime pairs up to max.
 
   Parameters
    limit : int
   Return
    Sequence (array); nil if max <= 0"
  (declare (type integer limit))
  (when (<= limit 0) (return-from generate-coprimes nil))

  (let ((coprimes (make-array (+ limit 2))) (m 2) (n 1))
    (iterate:iter
      (iterate::for i iterate::from 0 iterate::to limit iterate::by 3)
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
        (multiple-value-bind (a b c) (generate-pythagorean-triple (first i)
                                                                  (first (last i))
                                                                  :meuclid
                                                                  k)
          (when (pythagorean-triplet-p a b c)
            (if (= (+ a b c) 1000) (return-from problem-nine (* a b c)))))))))
