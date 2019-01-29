;;;; P99 implemented using lisp.
;;;; List problems.

(declaim (inline flatten-list))
(defun flatten-list (list)
  "Flatten a list LIST."
  (cond ((null list) nil)
        ((atom list) (funcall #'list list))
        (t (mapcan #'flatten-list list))))

(declaim (inline flatten-list))
(defun reverse-list (list)
  "Reverse a given list LIST."
  (cond ((null list) '())
        (t (append (reverse-list (rest list))
                   (funcall #'list (first list))))))

(defun p01 (n)
  "Find the last element of a list N."
  (if (null (rest n))
      (first n)
      (p01 (rest n))))

(defun p02 (n)
  "Find the last but one element of a list N."
  (labels ((rec-list (lst length position)
              (cond ((null lst) nil)
                    ((= length position) (first lst))
                    (t (rec-list (rest lst) length (1+ position))))))
    (rec-list n (- (length n) 2) 0)))

(defun p03 (n k)
  "Find the K'th element of a list N. Where the first element is K=1."
  (when (< k 1) nil)
  (loop for char in n
        for x from 1

        when (= x k)
        return char))

(defun p04 (n)
  "Find the number of elements of a list."
  (loop for nil in n
        for x from 1
        finally (return x)))

(defun p05 (n)
  "Reverse a list N."
  (reverse-list n))

(defun p06 (n)
  "Find out whether a list N can be a palindrome."
  (equal n (reverse-list n)))

(defun p07 (n)
  "Flatten a nested list N."
  (flatten-list n))

(defun p08 (n)
  "Eliminate consecutive duplicates of list elements."
  (loop for ele in n
        for prev = nil then curr
        for curr = ele
        unless (equal prev curr)
        collect ele))

(defun p09 (n)
  "Pack consecutive duplicates of list elements"
  (labels ((rec-list (lst prev)
            (cond ((null lst) (return-from rec-list (list prev)))
                  ((null prev) (rec-list (rest lst) (list (first lst))))
                  ((equal (first lst) (first prev))
                    (rec-list (rest lst) (append prev (list (first lst)))))
                  (t (append (list prev)
                              (rec-list (rest lst) (list (first lst))))))))
    (rec-list n nil)))

;; TODO
(defun p10 ()
  "Run-length encoding of a list")

;; TODO
(defun p11 ()
  "Modified run-length encoding")

;; TODO
(defun p12 ()
  "Decode a run-legnth encoded list.")

;; TODO
(defun p13 ()
  "Run-legnth encoding of a list (direct solution).")

(defun p14 (n)
  "Duplicate the elements of a list."
  (cond ((null n) '())
        (t (append (list (first n) (first n)) (rest n)))))

(defun p15 (n x)
  "Duplicate the elements of a list a given number of times."
  (labels ((dup-element (ele times)
            (when (= times 1) (return-from dup-element (list ele)))
            (append (list ele) (dup-element ele (1- times)))))
    (flatten-list (loop for char in n
                        collect (dup-element char x)))))

(defun p16 (lst n)
  "Drop every N'th element from a list.
 N starts at 1, therefore (p16 (list 1 2 3 4) 1) := NIL"
  (labels ((drop-list (lst n curr)
              (when (null lst) (return-from drop-list lst))
              (cond ((= curr n) (drop-list (rest lst) n 1))
                    (t (append (list (first lst))
                                     (drop-list (rest lst) n (1+ curr)))))))
    (drop-list lst n 1)))

(defun p17 (lst n)
  "Split a list into two parts; the length of the first part is given.
 N starts at 1. So (p17 (list 1 2 3 4) 0) := (1 2 3 4) while (p17 (list 1 2 3 4)
 1) := ((1) (2 3 4))."
  (let ((split-list (loop for value in lst
                          for at from 1
                          if (<= at n) collect value into left
                          else collect value into right
                          finally (return (list left right)))))
    (cond ((null (first split-list)) (first (last split-list)))
          ((null (first (last split-list))) (first split-list))
          (t split-list))))

(defun p18 (lst start end)
  "Extract a slice from a list."
  (loop for count from 1
        for value in lst
        when (and (>= count start) (<= count end))
        collect value))

;; TODO
(defun p19 (lst n)
  "Rotate a list N places to the left.")

;; TODO
(defun p20 (lst k)
  "Remove the K'th element from a list.")

;; TODO
(defun p21 (lst n ele)
  "Insert an ELE at a given N position into a list LST.")

;; TODO
(defun p22 (range)
  "Create a list containing all integers within a given range.")

;; TODO
(defun p23 (lst n)
  "Extract a given number of randomly selected elements from a list.")

;; TODO
(defun p24 (m n)
  "Lotto: Draw N different random numbers from the set 1..M")

;; TODO
(defun p25 ()
  "Generate a random permuation of the elements of a list.")

;; TODO
(defun p26 ()
  "Generate the combiations of K distinct objects chosen from the N elements of a
 list.")

;; TODO
(defun p27 ()
  "Generate the elemnts of a set into disjoint subsets.")

;; TODO
(defun p28 ()
  "Sorting a list of list according to length of sublists.")
