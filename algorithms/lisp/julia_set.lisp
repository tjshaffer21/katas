;;;; julia_set.lisp
;;;;
;;;; https://en.wikipedia.org/wiki/Julia_set

(ql:quickload :imago)
(ql:quickload :iterate)

(defun julia-set (image-size x-range y-range c threshold)
  "Implementation of the Julia set.
 Args
   image-size - width-height list for the image.
   x-range    - Min-max list for x.
   y-range    - Min-max list for y.
   c          - Complex number. Used for the julia set equation z=z^2 + c
   threshold  - Integer. Limit of julia set iteration.
 Return
   None
 Result
   Image 'julia.png' is created in the current working directory."
  (declare (type list image-size x-range y-range) (type integer threshold)
           (type complex c))
  
  (let* ((w (first image-size))
         (h (first (last image-size)))
         (dx (/ (- (first (last x-range)) (first x-range)) w))
         (dy (/ (- (first (last y-range)) (first y-range)) h))
         (image (make-instance 'imago:rgb-image :height h :width w)))
    (iterate:iter
     (iterate:with y = (first y-range))
     (iterate:for j iterate::from 0 iterate::to (1- h))
     (iterate:iter
      (iterate:with x = (first x-range))
      (iterate:for i iterate::from 0 iterate::to (1- w))
      (let ((z (complex x y))
            (n 0))
        (iterate:iter
         (iterate:while (and (< (abs z) 2.0) (< n threshold)))
         (setf z (+ (* z z) c))
         (incf n))

        (if (< (abs z) 2.0)
            (setf (imago:image-pixel image i j) imago:+blue+)
            (setf (imago:image-pixel image i j) imago:+red+)))
      (setf x (+ x dx)))
     (setf y (+ y dy)))

    (imago:write-png image "julia.png")))

(julia-set (list 1000 1000) (list -2.0 0.5) (list -1.0 1.0)
           (complex 0.36237 0.32) 100)
