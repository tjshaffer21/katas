;;;; mandelbrot.lisp
;;;;
;;;; https://en.wikipedia.org/wiki/Mandelbrot_set

(ql:quickload :imago)
(ql:quickload :iterate)

(defun mandelbrot (image-size x-range y-range &optional (max-iterations 100))
  "Implementation of the Mandelbrot set using Escape-Time algorithm.
 Args
   image-size     - width-height list.
   x-range        - Min-max list for x.
   y-range        - Min-max list for y.
   max-iterations - Integer for maximum number of iterations.
 Return
   None
 Result
   Image 'mandelbrot.png' is created in the current working directory."
  (declare (type list x-range y-range image-size) (type integer max-iterations))

  (let* ((w (first image-size))
         (h (first (last image-size)))
         (dx (/ (- (first (last x-range)) (first x-range)) w))
         (dy (/ (- (first (last y-range)) (first y-range)) h))
         (image (make-instance 'imago:rgb-image :height h :width w)))
    (iterate:iter
     (iterate:with y = (first y-range))
     (iterate::for j iterate::from 0 iterate::to (1- h))
     (iterate:iter
      (iterate:with x = (first x-range))
      (iterate::for i iterate::from 0 iterate::to (1- w))
      (let ((z 0)
            (n 0))
        (iterate::iter
         (iterate::while (and (< (abs z) 2.0) (<= n max-iterations)))
         (setf z (+ (* z z) (complex x y)))
         (incf n))

        (cond ((and (>= n 0) (< n (* max-iterations 0.10)))
               (setf (imago:image-pixel image i j) imago:+white+))
              ((and (>= n (* max-iterations 0.10))
                    (< n (* max-iterations 0.25)))
               (setf (imago:image-pixel image i j) imago:+yellow+))
              ((and (>= n (* max-iterations 0.25))
                    (< n (* max-iterations 0.50)))
               (setf (imago:image-pixel image i j) (imago:make-color 255 69 0)))
              ((and (>= n (* max-iterations 0.50)) (< n max-iterations))
               (setf (imago:image-pixel image i j) imago:+red+))
              ((>= n max-iterations)
               (setf (imago:image-pixel image i j) imago:+black+))))
      (setf x (+ x dx)))
     (setf y (+ y dy)))
    
    (imago:write-png image "mandelbrot.png")))

(mandelbrot (list 1000 1000) (list -2.0 0.5) (list -1.0 1.0) 100)
