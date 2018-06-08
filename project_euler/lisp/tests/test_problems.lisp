;;;; test_problems.lisp
;;;; Test cases for Project Euler problems.

(in-package #:project-euler.tests)

(lisp-unit:define-test test-diff-sum-square
  (lisp-unit:assert-equal 2640 (- (project-euler::square-of-sum 10)
                                  (project-euler::sum-of-squares 10))))
                                  
(lisp-unit:define-test test-digits
  (lisp-unit:assert-error 'type-error (project-euler::digits #\A))
  (lisp-unit:assert-eql 1 (project-euler::digits 0))
  (lisp-unit:assert-eql 1 (project-euler::digits 1))
  (lisp-unit:assert-eql 4 (project-euler::digits 1234)))

(lisp-unit:define-test test-even-sum-fibonacci 
  (lisp-unit:assert-error 'type-error (project-euler::even-sum-fibonacci #\A))
  (lisp-unit:assert-eql 188 (project-euler::even-sum-fibonacci 400)))
  
(lisp-unit:define-test test-fibonacci
  (lisp-unit:assert-error 'type-error (project-euler::recursive-fibonacci #\A))
  (lisp-unit:assert-error 'simple-error (project-euler::recursive-fibonacci -2))
  (lisp-unit:assert-equal 55 (project-euler::recursive-fibonacci 10))
  (lisp-unit:assert-equal 0 (project-euler::recursive-fibonacci 0))
  (lisp-unit:assert-equal 1 (project-euler::recursive-fibonacci 1))
  (lisp-unit:assert-equal 8 (project-euler::recursive-fibonacci 6)))

(lisp-unit:define-test test-fibonacci-dynamic
  (lisp-unit:assert-error 'type-error (project-euler::dynamic-fibonacci #\A))
  (lisp-unit:assert-equal 55 (aref (project-euler::dynamic-fibonacci 10) 10))
  (lisp-unit:assert-equal 0 (aref (project-euler::dynamic-fibonacci 0) 0))
  (lisp-unit:assert-equal 1 (aref (project-euler::dynamic-fibonacci 1) 1))
  (lisp-unit:assert-equal 8 (aref (project-euler::dynamic-fibonacci 6) 6)))
  
(lisp-unit:define-test test-gcd
  (lisp-unit:assert-error 'type-error (project-euler::greatest-common-divisor #\A 2 :binary))
  (lisp-unit:assert-error 'type-error (project-euler::greatest-common-divisor 2 #\B :binary))
  (lisp-unit:assert-false (project-euler::greatest-common-divisor -1 2))
  (lisp-unit:assert-false (project-euler::greatest-common-divisor 2 0)))
    
(lisp-unit:define-test test-gcd-binary 
  (lisp-unit:assert-error 'type-error (project-euler::gcd-binary #\A 2))
  (lisp-unit:assert-error 'type-error (project-euler::gcd-binary 2 #\B))
  (lisp-unit:assert-false (project-euler::gcd-binary 0 5))
  (lisp-unit:assert-false (project-euler::gcd-binary 2 -5))
  (lisp-unit:assert-eql 6 (project-euler::gcd-binary 12 6)))
  
(lisp-unit:define-test test-generate-pythagorean-triple
  (lisp-unit:assert-error 'type-error (project-euler::generate-pythagorean-triple 123 #\C :euclid 3))
  (lisp-unit:assert-error 'type-error (project-euler::generate-pythagorean-triple #\D 1 :euclid 4))
  ; TODO: These two are not being caught by CCL's type check.
  ;(lisp-unit:assert-error 'type-error (project-euler::generate-pythagorean-triple 3 4 :euclid #\A))
  ;(lisp-unit:assert-error 'type-error (project-euler::generate-pythagorean-triple 3 4 5 2))
  (lisp-unit:assert-false (project-euler::generate-pythagorean-triple 0 3 :euclid))
  (lisp-unit:assert-false (project-euler::generate-pythagorean-triple 4 0 :euclid)))
  
(lisp-unit:define-test test-integer-at
  (lisp-unit:assert-error 'type-error (project-euler::integer-at 123 #\C))
  (lisp-unit:assert-error 'type-error (project-euler::integer-at #\D 1))
  (lisp-unit:assert-eql 4 (project-euler::integer-at 1234 1))
  (lisp-unit:assert-eq 1 (project-euler::integer-at 1234 4))
  (lisp-unit:assert-equal 1 (project-euler::integer-at 1234 4))
  (lisp-unit:assert-equal 4 (project-euler::integer-at 1234 1))
  (lisp-unit:assert-equal nil (project-euler::integer-at 1234 0))
  (lisp-unit:assert-equal nil (project-euler::integer-at 1234 5))
  (lisp-unit:assert-equal nil (project-euler::integer-at -1234 4)))

(lisp-unit:define-test test-integer-into-sequence
  (lisp-unit:assert-error 'type-error (project-euler::integer-into-sequence #\A))
  (lisp-unit:assert-false (project-euler::integer-into-sequence -1))
  
  ;; Actually positive checks need to be handled.
  )

(lisp-unit:define-test test-multiples
  (lisp-unit:assert-error 'type-error (project-euler::multiples #\A 1))
  (lisp-unit:assert-error 'type-error (project-euler::multiples 1 #\A))
  (lisp-unit:assert-false (project-euler::multiples -1 5))
  (lisp-unit:assert-equal (list 4 6 8) (project-euler::multiples 2 8))
  (lisp-unit:assert-equal (list 6 9 12 15 18) (project-euler::multiples 3 20)))
  
(lisp-unit:define-test test-palindromep
  (lisp-unit:assert-error 'type-error (project-euler::palindromep #\A))
  (lisp-unit:assert-false (project-euler::palindromep 1234))
  (lisp-unit:assert-true (project-euler::palindromep 4334)))
  
(lisp-unit:define-test test-prime-factors
  (lisp-unit:assert-error 'type-error (project-euler::prime-factors #\A))
  (lisp-unit:assert-equal (list 2 2 5 5) (project-euler::prime-factors 100))
  (lisp-unit:assert-equal (list 2 2 19) (project-euler::prime-factors 76))
  (lisp-unit:assert-equal (list 2 2 2 2 3) (project-euler::prime-factors 48))
  (lisp-unit:assert-equal (list 2 2 3 3) (project-euler::prime-factors 36)))
  
(lisp-unit:define-test test-primep
  (lisp-unit:assert-error 'type-error (project-euler::primep 234 :fermat #\C))
  (lisp-unit:assert-error 'type-error (project-euler::primep #\A :fermat 34))
  (lisp-unit:assert-error 'simple-error (project-euler::primep 3 :fermat -1))
  (lisp-unit:assert-false (project-euler::primep -1 :fermat 3))
  (lisp-unit:assert-false (project-euler::primep 4 :fermat 1))
  (lisp-unit:assert-true (project-euler::primep 2))
  (lisp-unit:assert-true (project-euler::primep 13))
  (lisp-unit:assert-false (project-euler::primep 14))
  (lisp-unit:assert-false (project-euler::primep -1)))
  
(lisp-unit:define-test test-prime-mr
  (lisp-unit:assert-error 'type-error (project-euler::prime-miller-rabin #\A))
  (lisp-unit:assert-error 'type-error (project-euler::prime-miller-rabin 3 #\A))
  (lisp-unit:assert-error 'simple-error (project-euler::prime-miller-rabin 3 -1))
  (lisp-unit:assert-false (project-euler::prime-miller-rabin -1))
  (lisp-unit:assert-false (project-euler::prime-miller-rabin 90))
  (lisp-unit:assert-true (project-euler::prime-miller-rabin 2))
  (lisp-unit:assert-true (project-euler::prime-miller-rabin 3))
  (lisp-unit:assert-true (project-euler::prime-miller-rabin 5))
  (lisp-unit:assert-true (project-euler::prime-miller-rabin 7))
  (lisp-unit:assert-true (project-euler::prime-miller-rabin 11))
  (lisp-unit:assert-true (project-euler::prime-miller-rabin 13))
  (lisp-unit:assert-true (project-euler::prime-miller-rabin 7)))
  ; (lisp-unit:assert-false (project-euler::prime-miller-rabin 15 70))
  ; (lisp-unit:assert-false (project-euler::prime-miller-rabin 27 70))
  ; (lisp-unit:assert-false (project-euler::prime-miller-rabin 81 7))
  ; (lisp-unit:assert-false (project-euler::prime-miller-rabin 99 7)))
  
(lisp-unit:define-test test-prime-fermat
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
  ;(lisp-unit:assert-false (project-euler::prime-fermat 15))
  (lisp-unit:assert-false (project-euler::prime-fermat 27))
  (lisp-unit:assert-false (project-euler::prime-fermat 81))
  (lisp-unit:assert-false (project-euler::prime-fermat 99)))
  
(lisp-unit:define-test test-pythagorian-triplet-p
  (lisp-unit:assert-error 'type-error (project-euler::pythagorean-triplet-p 123 #\C 3))
  (lisp-unit:assert-error 'type-error (project-euler::pythagorean-triplet-p #\D 3 1))
  (lisp-unit:assert-error 'type-error (project-euler::pythagorean-triplet-p 3 2 #\D))
  (lisp-unit:assert-true (project-euler::pythagorean-triplet-p 3 4 5))
  (lisp-unit:assert-true (project-euler::pythagorean-triplet-p 13 84 85))
  (lisp-unit:assert-true (project-euler::pythagorean-triplet-p 160 231 281)))
  
(lisp-unit:define-test test-reverse-integer 
  (lisp-unit:assert-error 'type-error (project-euler::reverse-integer #\A))
  (lisp-unit:assert-equal 987654321 (project-euler::reverse-integer 123456789))
  (lisp-unit:assert-equal 32 (project-euler::reverse-integer 23))
  (lisp-unit:assert-equal -32 (project-euler::reverse-integer -23))
  (lisp-unit:assert-equal 231 (project-euler::reverse-integer 132))  
  (lisp-unit:assert-false (project-euler::palindromep 23))
  (lisp-unit:assert-true (project-euler::palindromep 232))
  (lisp-unit:assert-true (project-euler::palindromep 9009)))
  
(lisp-unit:define-test test-sieve-of-erathosthenes 
  (lisp-unit:assert-error 'type-error (project-euler::sieve-of-erathosthenes #\A))
  (lisp-unit:assert-false (project-euler::sieve-of-erathosthenes -1))
  (lisp-unit:assert-false (project-euler::sieve-of-erathosthenes array-total-size-limit))
  (lisp-unit:assert-equal (list 2 3 5 7)
                          (project-euler::sieve-of-erathosthenes 8))
  (lisp-unit:assert-equal (list 2 3 5 7 11 13 17 19)
                          (project-euler::sieve-of-erathosthenes 20)))

(lisp-unit:define-test test-sieve-of-sundaram
  (lisp-unit:assert-error 'type-error (project-euler::sieve-of-sundaram #\A))
  (lisp-unit:assert-false (project-euler::sieve-of-sundaram -1))
  (lisp-unit:assert-false (project-euler::sieve-of-sundaram array-total-size-limit))
  
  ;; Positive testing needs to be done.
  )
  
(lisp-unit:define-test test-square-of-sums 
  (lisp-unit:assert-error 'type-error (project-euler::square-of-sum #\A)) 
  (lisp-unit:assert-equal 3025 (project-euler::square-of-sum 10)))
  
(lisp-unit:define-test test-sum-of-squares
  (lisp-unit:assert-error 'type-error (project-euler::sum-of-squares #\A))
  (lisp-unit:assert-equal 385 (project-euler::sum-of-squares 10)))


;;; The Project Euler questions

(lisp-unit:define-test test-problem-one
  (lisp-unit:assert-equal 23 (project-euler:problem-one 10))
  (lisp-unit:assert-equal 233168 (project-euler:problem-one 1000)))

(lisp-unit:define-test test-problem-two
  (lisp-unit:assert-equal 4613732 (project-euler:problem-two 4000000)))

(lisp-unit:define-test test-problem-three
  (lisp-unit:assert-equal 6857 (project-euler:problem-three 600851475143)))

(lisp-unit:define-test test-problem-four
  (lisp-unit:assert-equal 906609 (project-euler:problem-four)))

(lisp-unit:define-test test-problem-five
  (lisp-unit:assert-equal 232792560 (project-euler:problem-five)
                          "incorrect value"))

(lisp-unit:define-test test-problem-six
  (lisp-unit:assert-equal 25164150 (project-euler:problem-six 100)))

(lisp-unit:define-test test-problem-seven
  (lisp-unit:assert-equal 104743 (project-euler::problem-seven 10001)))

(lisp-unit:define-test test-problem-eight
  (lisp-unit:assert-equal 23514624000 (project-euler:problem-eight)))

(lisp-unit:define-test test-problem-nine
  (lisp-unit:assert-equal 31875000 (project-euler:problem-nine)))

(lisp-unit:define-test test-problem-ten
  (lisp-unit:assert-equal 142913828922 (project-euler::problem-ten 2000000)))

; (lisp-unit:define-test test-problem-eleven
  ; (lisp-unit:assert-equal 70600674 (project-euler::problem-eleven)))
; (lisp-unit:define-test test-problem-twelve
  ; (lisp-unit:assert-equal 0 (project-euler::triangle-number 0)
                          ; "not the triangle number")
  ; (lisp-unit:assert-equal 3 (project-euler::triangle-number 2)
                          ; "not the triangle number")
  ; (lisp-unit:assert-equal 28 (project-euler::triangle-number 7)
                          ; "not the triangle number")

  ; Since default factors uses trial division we test both at once.
  ; (lisp-unit:assert-equal (list 1) (project-euler::factors 1)
                          ; "incorrect factors")
  ; (lisp-unit:assert-equal (list 1 3) (project-euler::factors 3)
                          ; "incorrect factors")
  ; (lisp-unit:assert-equal (list 1 2 4 7 14 28) (project-euler::factors 28)
                          ; "incorrect factors")
  
  ; (lisp-unit:assert-equal 76576500 (project-euler::problem-twelve)))

(lisp-unit:define-test test-problem-sixteen
  (lisp-unit:assert-eq 26 (project-euler:problem-sixteen 15))
  (lisp-unit:assert-eq 1366 (project-euler:problem-sixteen 1000)))

(defun run-tests ()
  (lisp-unit:run-tests :all 'project-euler.tests))
  
(defun run-euler ()
  (lisp-unit:run-tests '(project-euler.tests::test-problem-one 
                         project-euler.tests::test-problem-two
                         project-euler.tests::test-problem-three 
                         project-euler.tests::test-problem-four 
                         project-euler.tests::test-problem-five 
                         project-euler.tests::test-problem-six 
                         project-euler.tests::test-problem-seven
                         project-euler.tests::test-problem-eight
                         project-euler.tests::test-problem-nine
                         project-euler.tests::test-problem-ten
                         project-euler.tests::test-problem-sixteen)
                        'project-euler.tests))
