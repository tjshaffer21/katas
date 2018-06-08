;;;; package.lisp

(defpackage #:project-euler.tests
  (:use #:cl #:project-euler)
  (:export :run-tests :run-euler))
    
(defpackage #:project-euler.benchmarks
    (:use #:cl #:trivial-benchmark #:project-euler)
    (:export #:run-benchmarks))
