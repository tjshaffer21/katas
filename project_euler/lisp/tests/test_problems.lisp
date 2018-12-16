;;;; test_problems.lisp
;;;; Test cases for Project Euler problems.

(in-package #:project-euler.tests)

(lisp-unit:define-test test-diff-sum-square
  (:tag :aux)
  (lisp-unit:assert-equal 2640 (- (project-euler::square-of-sum 10)
                                  (project-euler::sum-of-squares 10))))

(lisp-unit:define-test test-generate-pythagorean-triple
  (:tag :aux)
  (lisp-unit:assert-error 'type-error
    (project-euler::generate-pythagorean-triple 123 #\C :euclid 3))
  (lisp-unit:assert-error 'type-error
    (project-euler::generate-pythagorean-triple #\D 1 :euclid 4))

  (lisp-unit:assert-false
    (project-euler::generate-pythagorean-triple 0 3 :euclid))
  (lisp-unit:assert-false
    (project-euler::generate-pythagorean-triple 4 0 :euclid)))

(lisp-unit:define-test test-ncollatz-length
  (let ((cache (make-hash-table)))
    (lisp-unit:assert-equal 1 (project-euler::ncollatz-length 1 cache))
    (lisp-unit:assert-equal 2 (project-euler::ncollatz-length 2 cache))
    (lisp-unit:assert-equal 3 (project-euler::ncollatz-length 4 cache))
    (lisp-unit:assert-equal 4 (project-euler::ncollatz-length 8 cache))
    (lisp-unit:assert-equal 5 (project-euler::ncollatz-length 16 cache))
    (lisp-unit:assert-equal 6 (project-euler::ncollatz-length 5 cache))
    (lisp-unit:assert-equal 7 (project-euler::ncollatz-length 10 cache))
    (lisp-unit:assert-equal 8 (project-euler::ncollatz-length 20 cache))
    (lisp-unit:assert-equal 9 (project-euler::ncollatz-length 40 cache))
    (lisp-unit:assert-equal 10 (project-euler::ncollatz-length 13 cache))))

(lisp-unit:define-test test-next-collatz-term
  (lisp-unit:assert-equal 1 (project-euler::next-collatz-term 1))
  (lisp-unit:assert-equal 1 (project-euler::next-collatz-term 2))
  (lisp-unit:assert-equal 2 (project-euler::next-collatz-term 4))
  (lisp-unit:assert-equal 4 (project-euler::next-collatz-term 8))
  (lisp-unit:assert-equal 8 (project-euler::next-collatz-term 16))
  (lisp-unit:assert-equal 16 (project-euler::next-collatz-term 5))
  (lisp-unit:assert-equal 5 (project-euler::next-collatz-term 10))
  (lisp-unit:assert-equal 10 (project-euler::next-collatz-term 20))
  (lisp-unit:assert-equal 20 (project-euler::next-collatz-term 40))
  (lisp-unit:assert-equal 40 (project-euler::next-collatz-term 13)))

(lisp-unit:define-test test-palindromep
  (:tag :aux)
  (lisp-unit:assert-error 'type-error (project-euler::palindromep #\A))

  (lisp-unit:assert-false (project-euler::palindromep 1234))
  (lisp-unit:assert-false (project-euler::palindromep 23))

  (lisp-unit:assert-true (project-euler::palindromep 4334))
  (lisp-unit:assert-true (project-euler::palindromep 232))
  (lisp-unit:assert-true (project-euler::palindromep 9009)))

(lisp-unit:define-test test-prime-factors
  (:tag :aux)
  (lisp-unit:assert-error 'type-error (project-euler::prime-factors #\A))
  (lisp-unit:assert-equal nil (project-euler::prime-factors 1))

  (lisp-unit:assert-equal (list 2 2) (project-euler::prime-factors 4))
  (lisp-unit:assert-equal (list 13) (project-euler::prime-factors 13))
  (lisp-unit:assert-equal (list 2 2 5) (project-euler::prime-factors 20))
  (lisp-unit:assert-equal (list 3 3 5 7) (project-euler::prime-factors 315)))

(lisp-unit:define-test test-prime-factors-list
  (:tag :aux)
  (lisp-unit:assert-error 'type-error (project-euler::prime-factors-list #\A))
  (lisp-unit:assert-equal nil (project-euler::prime-factors-list 1))

  (lisp-unit:assert-equal (list (list 2 2))
    (project-euler::prime-factors-list 4))
  (lisp-unit:assert-equal (list (list 7 1))
    (project-euler::prime-factors-list 7))
  (lisp-unit:assert-equal (list (list 2 1) (list 3 2))
    (project-euler::prime-factors-list 18))
  (lisp-unit:assert-equal (list (list 2 2) (list 5 1))
    (project-euler::prime-factors-list 20))
  (lisp-unit:assert-equal (list (list 2 1) (list 3 1) (list 5 1))
    (project-euler::prime-factors-list 30))
  (lisp-unit:assert-equal (list (list 2 5) (list 3 1))
    (project-euler::prime-factors-list 96)))

(lisp-unit:define-test test-prime-fermat
  (:tag :aux)
  (lisp-unit:assert-error 'type-error (project-euler::prime-fermat #\A))
  (lisp-unit:assert-error 'type-error (project-euler::prime-fermat 3 #\A))
  (lisp-unit:assert-error 'simple-error (project-euler::prime-fermat 3 -1))

  (lisp-unit:assert-false (project-euler::prime-fermat -1))
  (lisp-unit:assert-false (project-euler::prime-fermat 90))

  (lisp-unit:assert-true (project-euler::prime-fermat 2))
  (lisp-unit:assert-true (project-euler::prime-fermat 3))
  (lisp-unit:assert-true (project-euler::prime-fermat 5))
  (lisp-unit:assert-true (project-euler::prime-fermat 7))
  (lisp-unit:assert-true (project-euler::prime-fermat 11))
  (lisp-unit:assert-true (project-euler::prime-fermat 13))
  (lisp-unit:assert-true (project-euler::prime-fermat 7))

  (lisp-unit:assert-false (project-euler::prime-fermat 15))
  (lisp-unit:assert-false (project-euler::prime-fermat 27))
  (lisp-unit:assert-false (project-euler::prime-fermat 81))
  (lisp-unit:assert-false (project-euler::prime-fermat 99)))

(lisp-unit:define-test test-pythagorian-triplet-p
  (:tag :aux)
  (lisp-unit:assert-error 'type-error
    (project-euler::pythagorean-triplet-p 123 #\C 3))
  (lisp-unit:assert-error 'type-error
    (project-euler::pythagorean-triplet-p #\D 3 1))
  (lisp-unit:assert-error 'type-error
    (project-euler::pythagorean-triplet-p 3 2 #\D))

  (lisp-unit:assert-true (project-euler::pythagorean-triplet-p 3 4 5))
  (lisp-unit:assert-true (project-euler::pythagorean-triplet-p 13 84 85))
  (lisp-unit:assert-true (project-euler::pythagorean-triplet-p 160 231 281)))

(lisp-unit:define-test test-square-of-sums
  (:tag :aux)
  (lisp-unit:assert-error 'type-error (project-euler::square-of-sum #\A))

  (lisp-unit:assert-equal 3025 (project-euler::square-of-sum 10)))

(lisp-unit:define-test test-sum-of-squares
  (:tag :aux)
  (lisp-unit:assert-error 'type-error (project-euler::sum-of-squares #\A))

  (lisp-unit:assert-equal 385 (project-euler::sum-of-squares 10)))

;;; The Project Euler questions

(lisp-unit:define-test test-problem-one
  (:tag :prime)
  (lisp-unit:assert-equal 23 (project-euler:problem-one 10))
  (lisp-unit:assert-equal 233168 (project-euler:problem-one 1000)))

(lisp-unit:define-test test-problem-two
  (:tag :prime)
  (lisp-unit:assert-equal 4613732 (project-euler:problem-two 4000000)))

(lisp-unit:define-test test-problem-three
  (:tag :prime)
  (lisp-unit:assert-equal 6857 (project-euler:problem-three 600851475143)))

(lisp-unit:define-test test-problem-four
  (:tag :prime)
  (lisp-unit:assert-equal 906609 (project-euler:problem-four)))

(lisp-unit:define-test test-problem-five
  (:tag :prime)
  (lisp-unit:assert-equal 232792560 (project-euler:problem-five)))

(lisp-unit:define-test test-problem-six
  (:tag :prime)
  (lisp-unit:assert-equal 25164150 (project-euler:problem-six 100)))

(lisp-unit:define-test test-problem-seven
  (:tag :prime)
  (lisp-unit:assert-equal 104743 (project-euler::problem-seven 10001)))

(lisp-unit:define-test test-problem-eight
  (:tag :prime)
  (lisp-unit:assert-equal 23514624000 (project-euler:problem-eight)))

(lisp-unit:define-test test-problem-nine
  (:tag :prime)
  (lisp-unit:assert-equal 31875000 (project-euler:problem-nine)))

(lisp-unit:define-test test-problem-ten
  (:tag :prime)
  (lisp-unit:assert-equal 142913828922 (project-euler::problem-ten 2000000)))

; TODO Not solved.
; (lisp-unit:define-test test-problem-eleven
  ; (lisp-unit:assert-equal 70600674 (project-euler::problem-eleven)))

(lisp-unit:define-test test-problem-twelve
  (:tag :prime)
  (lisp-unit:assert-equal 28 (project-euler::problem-twelve 6))
  (lisp-unit:assert-equal 76576500 (project-euler::problem-twelve 500)))

(lisp-unit:define-test test-problem-fourteen
  (:tag :prime)
  (lisp-unit:assert-equal 837799 (project-euler::problem-fourteen 1000000)))

(lisp-unit:define-test test-problem-sixteen
  (:tag :prime)
  (lisp-unit:assert-eq 26 (project-euler:problem-sixteen 15))
  (lisp-unit:assert-eq 1366 (project-euler:problem-sixteen 1000)))

(lisp-unit:define-test test-problem-twenty-five
  (:tag :prime)
  (lisp-unit:assert-eq 12 (project-euler:problem-twenty-five 3))
  (lisp-unit:assert-eq 4782 (project-euler:problem-twenty-five 1000)))

(defun run-tests ()
  (lisp-unit:run-tests :all 'project-euler.tests))

(defun run-euler ()
  (lisp-unit:run-tags '(:prime) 'project-euler.tests))

(defun run-aux-tests ()
  (lisp-unit:run-tags '(:aux) 'project-euler.tests))
