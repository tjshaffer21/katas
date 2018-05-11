;;;; projecteuler.asd

(asdf:defsystem #:project-euler
  :description "Project Euler problems."
  :author "Thomas Shaffer <tjshaffer21@gmail.com>"
  :license ""
  :serial t
  :depends-on (:cl-utilities
               :iterate)
  :components ((:file "src/package")
               (:file "src/common_functions")
               (:file "src/problem_one")
               (:file "src/problem_two")
               (:file "src/problem_three")
               (:file "src/problem_four")
               (:file "src/problem_five")
               (:file "src/problem_six")
               (:file "src/problem_seven")
               (:file "src/problem_eight")
               (:file "src/problem_nine")
               (:file "src/problem_ten")))
