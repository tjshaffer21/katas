;;;; project_euler.asd

(asdf:defsystem #:project-euler
  :description "Project Euler problems."
  :author "Thomas Shaffer <tjshaffer21@gmail.com>"
  :license ""
  :serial t
  :depends-on (:cl-utilities
               :iterate
               :smath)
  :components ((:file "src/package")
               (:file "src/primes")
               (:file "src/problems")
               (:file "src/problem_four")
               (:file "src/problem_five")
               (:file "src/problem_six")
               (:file "src/problem_nine")
               (:file "src/problem_twelve")
               (:file "src/problem_fourteen")))

(asdf:defsystem #:project-euler-tests
  :description "Project Euler problems."
  :author "Thomas Shaffer <tjshaffer21@gmail.com>"
  :license ""
  :serial t
  :depends-on (:cl-utilities
               :iterate
               :lisp-unit
               :trivial-benchmark
               :project-euler)
  :components ((:file "tests/package")
               (:file "tests/test_problems")
               (:file "tests/benchmarking")))
