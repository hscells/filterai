;;; Filterai - The AI that created image filters based on paintings
;;; Uses the Opticl common lisp library - https://github.com/slyrus/opticl
;;; Harry Scells 2015

;; Load the image processing library
(load "quicklisp/setup.lisp")
(ql:quickload "opticl")

;; Define packages to use
(use-package 'opticl)

;; Convert an image into RGB pixels
(defun load-painting (name)
   (let ((img (read-png-file name))) img))

;; Do some basic processing on the image (adjust colour values)
(defun write-image (img name)
   (write-png-file name img))

;; Perform a very simple modification to the pixels in an image to greyscale it
(defun greyscale-image (img)
   (typecase img
      (8-bit-rgb-image
         (locally (declare (type 8-bit-rgb-image img))
            (with-image-bounds (height width) img
               (loop for i below height do
                  (loop for j below width do
                     (multiple-value-bind (r g b)
                        (pixel img i j)
                        (declare (type (unsigned-byte 8) r g b))
                        (setf grey (floor (/ (+ r g b) 3)))
                        (setf (pixel img i j)
                           (values grey grey grey))))))))))

;; Sums an array
; https://faculty.washington.edu/ikalet/courses/lisp/code/arrays.cl
(defun add-all-elements (arr)
   (let ((dims (array-dimensions arr)) (sum 0))
   (if (= (length dims) 1)
	  (dotimes (i (first dims) sum)
	   (setf sum (+ sum (aref arr i))))
         (if (= (length dims) 2)
   	      (dotimes (i (first dims) sum)
	            (dotimes (j (second dims))
	               (setf sum (+ sum (aref arr i j)))))
	         (dotimes (i (first dims) sum)
	            (dotimes (j (second dims))
	               (dotimes (k (third dims))
	                  (setf sum (+ sum (aref arr i j k))))))))))

;; Apply a noise estimation algorithm
; http://www.kyxk.net/att.php?p.490.43283.358.pdf
(defun estimate-noise (img output)
   (greyscale-image img)
   (setf m (make-array '(3 3)
      :initial-contents
         '((1 -2 1)
            (-2 4 -2)
            (1 -2 1))))
   (setf img (discrete-convolve img m))
   (setf sigma (add-all-elements img))
   (write-image img output) ; output the image for now
   (typecase img
      (8-bit-rgb-image
         (locally (declare (type 8-bit-rgb-image img))
            (with-image-bounds (height width) img
               (setf sigma (* sigma (sqrt (* 0.5 pi)) (/ 1 (* 6 (- width 2) (- height 2)))))))))sigma)

;; Shorthand function to run the noise estimation
(defun noise (input output)
   (estimate-noise (load-painting input) output))
