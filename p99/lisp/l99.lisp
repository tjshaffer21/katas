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

(defun rotate (list n)
  "Rotate LIST by N elements in given DIRECTION."
  (destructuring-bind (left right) (split-list list (if (plusp n)
                                                        n
                                                        (+ (length list) n)))
    (append right left)))

(defun split-list (list n)
  "Split a list into two parts; the length of the first part is given. N starts
 at 1."
  (loop for value in list
        for at from 1
        if (<= at n) collect value into left
        else collect value into right
        finally (return (funcall 'list left right))))

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

(defun p10 (list)
  "Run-length encoding of a list"
  (loop for value in (p09 list)
        collect (list (length value) (first value))))

(defun p11 (list)
  "Modified run-length encoding"
  (loop for value in (p09 list)
        for ele = (list (length value) (first value))
        if (= (first ele) 1) collect (first (last ele)) else collect ele))

(defun p12 (list)
  "Decode a run-length encoded list."
  (cond ((null list) '())
        (t (if (atom (first list))
               (append (funcall 'list (first list)) (p12 (rest list)))
               (append (loop for x from (first (first list)) downto 1
                       while (>= x 0) collect (first (last (first list))))
                       (p12 (rest list)))))))

(defun p13 (list)
  "Run-length encoding of a list (direct solution)."
  (let ((counter 1))
    (loop for ele in list
          for prev = curr
          for curr = ele
          if (equal prev curr)
              do (incf counter)
          else
              collect (list counter prev) into results and
              do (setf counter 1)
          end
          finally
            ;; Need to remove NIL and include final element(s).
            (return (append (rest results) (list (list counter curr)))))))

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
  (let ((split-list (funcall 'split-list lst n)))
    (cond ((null (first split-list)) (first (last split-list)))
          ((null (first (last split-list))) (first split-list))
          (t split-list))))

(defun p18 (lst start end)
  "Extract a slice from a list."
  (loop for count from 1
        for value in lst
        when (and (>= count start) (<= count end))
        collect value))

(defun p19 (lst n)
  "Rotate a list N places to the left."
  (labels ((rec-list (lst n stack)
            (cond ((null lst) stack)
                  ((= n 1) (append (rest lst) stack (list (first lst))))
                  (t (rec-list (rest lst)
                               (1- n)
                               (append stack (list (first lst))))))))
    (rec-list lst (if (>= n 0) n (+ (length lst) n)) '())))

(defun p20 (lst k)
  "Remove the K'th element from a list."
  (cond ((null lst) '())
        ((= k 1) (rest lst))
        (t (append (list (first lst)) (p20 (rest lst) (1- k))))))

(defun p21 (lst n ele)
  "Insert an ELE at a given N position into a list LST. N starts at 1."
  (cond ((null lst) '())
        ((= n 1) (append (list ele)
                         (list (first lst))
                         (rest lst)))
        (t (append (list (first lst))
                   (p21 (rest lst) (1- n) ele)))))

(defun p22 (range)
  "Create a list containing all integers within a given range."
  (loop for x from (first range)
        while (<= x (first (last range)))
        collect x))

(defun p23 (lst n)
  "Extract a given number of randomly selected elements from a list."
  (let ((l (length lst)))
    (loop for x from 0 below n collect (nth (random l) lst))))

(defun p24 (m n)
  "Lotto: Draw N different random numbers from the set 1..M"
  (loop for x from 0 below n collect (1+ (random m))))

;; TODO
(defun p25 ()
  "Generate a random permuation of the elements of a list.")

;; TODO
(defun p26 ()
  "Generate the combiations of K distinct objects chosen from the N elements of
 a list.")

;; TODO
(defun p27 ()
  "Generate the elemnts of a set into disjoint subsets.")

;; TODO
(defun p28 ()
  "Sorting a list of list according to length of sublists.")
