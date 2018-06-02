;;;; test_problems.lisp
;;;; Test cases for Project Euler problems.

(in-package #:project-euler.tests)

(lisp-unit:define-test test-digits
  (lisp-unit:assert-error 'type-error (project-euler::digits #\A))
  (lisp-unit:assert-eql 1 (project-euler::digits 0))
  (lisp-unit:assert-eql 1 (project-euler::digits 1))
  (lisp-unit:assert-eql 4 (project-euler::digits 1234)))
    
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
 
(lisp-unit:define-test test-primep
  (lisp-unit:assert-error 'type-error (project-euler::primep 234 :fermat #\C))
  (lisp-unit:assert-error 'type-error (project-euler::primep #\A :fermat 34))
  (lisp-unit:assert-error 'simple-error (project-euler::primep 3 :fermat -1))
  (lisp-unit:assert-false (project-euler::primep -1 :fermat 3))
  (lisp-unit:assert-false (project-euler::primep 4 :fermat 1)))
  
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
  (lisp-unit:assert-true (project-euler::prime-miller-rabin 7))
  (lisp-unit:assert-false (project-euler::prime-miller-rabin 15 70))
  (lisp-unit:assert-false (project-euler::prime-miller-rabin 27 70))
  (lisp-unit:assert-false (project-euler::prime-miller-rabin 81 7))
  (lisp-unit:assert-false (project-euler::prime-miller-rabin 99 7)))
  
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
  (lisp-unit:assert-false (project-euler::prime-fermat 15))
  (lisp-unit:assert-false (project-euler::prime-fermat 27))
  (lisp-unit:assert-false (project-euler::prime-fermat 81))
  (lisp-unit:assert-false (project-euler::prime-fermat 99)))

(lisp-unit:define-test test-integer-at
    (lisp-unit:assert-error 'type-error (project-euler::integer-at 123 #\C))
    (lisp-unit:assert-error 'type-error (project-euler::integer-at #\D 1))
    (lisp-unit:assert-eql 4 (project-euler::integer-at 1234 1))
    (lisp-unit:assert-eq 1 (project-euler::integer-at 1234 4)))
    

(lisp-unit:define-test test-problem-one
  ;; Given example
  (lisp-unit:assert-equal (list 3 6 9 5)
                          (project-euler::multiples-below (list 3 5) 10)
                          "incorrect multiples")

  (lisp-unit:assert-equal 23 (apply #'+ (project-euler::multiples-below
                                          (list 3 5) 10))
                          "incorrect sum.")

  ;; Solution
  (lisp-unit:assert-equal 233168 (project-euler::problem-one)
                          "incorrect solution."))

(lisp-unit:define-test test-problem-two
  (lisp-unit:assert-equal 0 (project-euler::fibonacci 0)
                          "incorrect value")
  (lisp-unit:assert-equal 1 (project-euler::fibonacci 1)
                          "incorrect value")
  (lisp-unit:assert-equal 8 (project-euler::fibonacci 6)
                          "incorrect value")
  (lisp-unit:assert-equal 4613732 (project-euler::problem-two)))

(lisp-unit:define-test test-problem-three
  (lisp-unit:assert-equal (list 2 2 5 5) (project-euler::prime-factors 100)
                          "incorrect factors")
  (lisp-unit:assert-equal (list 2 2 19) (project-euler::prime-factors 76)
                          "incorrect factors")
  (lisp-unit:assert-equal (list 2 2 2 2 3) (project-euler::prime-factors 48)
                          "incorrect factors")
  (lisp-unit:assert-equal (list 2 2 3 3) (project-euler::prime-factors 36)
                          "incorrect factors")
  (lisp-unit:assert-equal 6857 (project-euler::problem-three)))

(lisp-unit:define-test test-problem-four
  (lisp-unit:assert-equal 32 (project-euler::reverse-integer 23)
                          "not reversed")
  (lisp-unit:assert-equal -32 (project-euler::reverse-integer -23)
                          "not reversed")
  (lisp-unit:assert-equal 231 (project-euler::reverse-integer 132)
                          "not reversed")

  (lisp-unit:assert-equal 1 (project-euler::integer-at 1234 4)
                          "incorrect value")
  (lisp-unit:assert-equal 4 (project-euler::integer-at 1234 1)
                          "incorrect value")
  (lisp-unit:assert-equal nil (project-euler::integer-at 1234 0)
                          "incorrect value")
  (lisp-unit:assert-equal nil (project-euler::integer-at 1234 5)
                          "incorrect value")
  (lisp-unit:assert-equal nil (project-euler::integer-at -1234 4)
                          "incorrect value")
  
  (lisp-unit:assert-equal nil (project-euler::palindrome-p 23)
                          "not a palindrome")
  (lisp-unit:assert-equal t (project-euler::palindrome-p 232)
                          "palindrome")
  (lisp-unit:assert-equal t (project-euler::palindrome-p 9009)
                          "palindrome")

  (lisp-unit:assert-equal 906609 (project-euler::problem-four)
                          "incorrect result"))

(lisp-unit:define-test test-problem-five
  (lisp-unit:assert-equal 232792560 (project-euler::problem-five)
                          "incorrect value"))

(lisp-unit:define-test test-problem-six
  (lisp-unit:assert-equal 385 (project-euler::sum-of-squares 10)
                          "incorrect result")
  (lisp-unit:assert-equal 3025 (project-euler::square-of-sum 10)
                         "incorrect result")
  (lisp-unit:assert-equal 2640 (- (project-euler::square-of-sum 10)
                                  (project-euler::sum-of-squares 10)))
  (lisp-unit:assert-equal 25164150 (project-euler::problem-six)))

(lisp-unit:define-test test-problem-seven
  (lisp-unit:assert-equal t (project-euler::primep 2)
                          "prime")
  (lisp-unit:assert-equal t (project-euler::primep 13)
                          "prime")
  (lisp-unit:assert-equal nil (project-euler::primep 14)
                          "not prime")
  (lisp-unit:assert-equal nil (project-euler::primep -1)
                          "not prime")

  (lisp-unit:assert-equal (list 4 6 8) (project-euler::multiples 2 8)
                          "incorrect multiples")
  (lisp-unit:assert-equal (list 6 9 12 15 18) (project-euler::multiples 3 20)
                          "incorrect multiples")

  (lisp-unit:assert-equal (list 2 3 5 7)
                          (project-euler::sieve-of-erathosthenes 8)
                          "incorrect sieve")
  (lisp-unit:assert-equal (list 2 3 5 7 11 13 17 19)
                          (project-euler::sieve-of-erathosthenes 20)
                          "incorrect sieve")
  
  (lisp-unit:assert-equal 104743 (project-euler::problem-seven)))

(lisp-unit:define-test test-problem-eight
  ;; Should also test integer-into-sequence but assert-equal, assert-eql
  ;; and assert-eq return incorrectly.
  (lisp-unit:assert-equal 23514624000 (project-euler::problem-eight)))

(lisp-unit:define-test test-problem-nine
  (lisp-unit:assert-true (project-euler::pythagorean-triplet-p 3 4 5)
                         "not a triplet")
  (lisp-unit:assert-true (project-euler::pythagorean-triplet-p 5 12 13)
                         "not a triplet")
  (lisp-unit:assert-true (project-euler::pythagorean-triplet-p 28 96 100))

  ;; Should also test generate-coprimes and euclid-pythagorean-triple
  
  (lisp-unit:assert-equal 31875000 (project-euler::problem-nine)))

(lisp-unit:define-test test-problem-ten
  (lisp-unit:assert-equal 142913828922 (project-euler::problem-ten 2000000)
                          "incorrect sum"))

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
                         project-euler.tests::test-problem-ten)
                        'project-euler.tests))
