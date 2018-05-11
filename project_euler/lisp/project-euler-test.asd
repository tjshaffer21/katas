;;;; project-euler-test.asd

(asdf:defsystem #:project-euler.tests
  :description "Test cases for Project Euler problems."
  :author "Thomas Shaffer <tjshaffer21@gmail.com>"
  :license ""
  :serial t
  :depends-on (:cl-utilities
               :iterate
               :lisp-unit)
  :components ((:file "tests/package")
               (:file "tests/test_problems")))
