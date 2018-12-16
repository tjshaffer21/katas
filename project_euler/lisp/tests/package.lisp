;;;; package.lisp

(defpackage #:project-euler.tests
  (:use #:cl #:project-euler)
  (:export :run-tests :run-euler :run-aux-tests))

(defpackage #:project-euler.benchmarks
    (:use #:cl #:trivial-benchmark #:project-euler)
    (:export :do-benchmark-report
             :benchmark-prime-factors
             :benchmark-prime-factors-list
             :benchmark-sundaram
             :benchmark-gcd-binary
             :benchmark-generate-coprimes))
