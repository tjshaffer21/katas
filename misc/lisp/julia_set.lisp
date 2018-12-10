;;;; julia_set.lisp
;;;;
;;;; https://en.wikipedia.org/wiki/Julia_set

(ql:quickload '(:imago :iterate))

(defun julia-set (image-size x-range y-range c threshold)
  "Implementation of the Julia set.

  Parameters
    image-size : list : (width, height) of image.
    x-range : list : (min, max) for x.
    y-range : list : (min, max) for y.
    c : complex : Complex number used for julia set equation.
    threshold : int : Iteration threshold.
  Return
    None
  Result
    Image 'julia.png' is created in the current working directory.
  Error
    type-error : Incorrect types for parameters."
  (declare (type list image-size x-range y-range)
           (type integer threshold)
           (type complex c)
           (optimize (speed 3) (safety 0)))

  (iterate:iter
    (iterate:with w = (first image-size))
    (iterate:with h = (first (last image-size)))
    (iterate:with image = (make-instance 'imago:rgb-image :height h :width w))

    (iterate:with dy = (/ (- (first (last y-range)) (first y-range)) h))
    (iterate:with dx = (/ (- (first (last x-range)) (first x-range)) w))
    (iterate:for y iterate::from (first y-range) iterate::by dy)
    (iterate:for j iterate::from 0 iterate::below h)

    (iterate:iter
      (iterate:for x iterate::from (first x-range) iterate::by dx)
      (iterate:for i iterate::from 0 iterate::below w)
      (iterate:iter
        (iterate:for z iterate::initially (complex x y) iterate::then (+ (* z z) c))
        (iterate:for n iterate::from 0)
        (iterate:while (and (< (abs z) 2.0) (< n threshold)))
        (iterate:finally
            (if (< (abs z) 2.0)
                (setf (imago:image-pixel image i j) imago:+blue+)
                (setf (imago:image-pixel image i j) imago:+red+)))))
    (iterate::finally (imago:write-png image "julia.png"))))

(julia-set (list 1000 1000) (list -2.0 0.5) (list -1.0 1.0)
           (complex 0.36237 0.32) 100)
