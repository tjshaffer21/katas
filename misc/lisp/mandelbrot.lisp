;;;; mandelbrot.lisp
;;;;
;;;; https://en.wikipedia.org/wiki/Mandelbrot_set

(ql:quickload '(:imago :iterate))

(defun mandelbrot (image-size x-range y-range &optional (max-iterations 100))
  "Implementation of the Mandelbrot set using Escape-Time algorithm.
  Args
    image-size : list : (width, height) for the image.
    x-range : list : (min, max) for x.
    y-range : list : (min, max) for y.
    max-iterations : int : Max iterations.
  Return
    None
  Result
    Image 'mandelbrot.png' is created in the current working directory.
  Error
    type-error : if paramters are incorrect."
  (declare (type list x-range y-range image-size)
           (type integer max-iterations)
           (optimize (speed 3) (safety 0)))

  (iterate:iter
    (iterate:with orange = (imago:make-color 255 69 0))
    (iterate:with w-bounds = (* max-iterations 0.10))
    (iterate:with y-bounds = (* max-iterations 0.25))
    (iterate:with o-bounds = (* max-iterations 0.50))
    (iterate:with w = (first image-size))
    (iterate:with h = (first (last image-size)))
    (iterate:with image = (make-instance 'imago:rgb-image :height h :width w))

    (iterate:with dx = (/ (- (first (last x-range)) (first x-range)) w))
    (iterate:with dy = (/ (- (first (last y-range)) (first y-range)) h))
    (iterate:for y iterate::from (first y-range) iterate::by dy)
    (iterate:for j iterate::from 0 iterate::below h)
    (iterate:iter
      (iterate:for x iterate::from (first x-range) iterate::by dx)
      (iterate:for i iterate::from 0 iterate::below w)

      (iterate:iter
        (iterate:for z iterate::initially 0
                        iterate::then (+ (* z z) (complex x y)))
        (iterate:for n iterate::from 0)
        (iterate:while (and (< (abs z) 2.0) (<= n max-iterations)))
        (iterate:finally
          (cond ((and (>= n 0) (< n w-bounds))
                 (setf (imago:image-pixel image i j) imago:+white+))
                ((and (>= n w-bounds) (< n y-bounds))
                 (setf (imago:image-pixel image i j) imago:+yellow+))
                ((and (>= n y-bounds) (< n o-bounds))
                 (setf (imago:image-pixel image i j) orange))
                ((and (>= n o-bounds) (< n max-iterations))
                 (setf (imago:image-pixel image i j) imago:+red+))
                ((>= n max-iterations)
                 (setf (imago:image-pixel image i j) imago:+black+))))))
    (iterate:finally (imago:write-png image "mandelbrot.png"))))

(mandelbrot (list 1000 1000) (list -2.0 0.5) (list -1.0 1.0) 100)
