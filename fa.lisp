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

(defun estimate-noise (img output)
   (greyscale-image img)
   (setf m (make-array '(3 3)
      :initial-contents
         '((1 -2 1)
            (-2 4 -2)
            (1 -2 1))))
   (setf sigma 0l0)

   (write-image img output)
   (typecase img
      (8-bit-rgb-image
         (locally (declare (type 8-bit-rgb-image img))
            (with-image-bounds (height width) img
               (loop for i below height do
                  (loop for j below width do
                     (multiple-value-bind (r g b)
                        (pixel img i j)
                        (declare (type (unsigned-byte 8) r g b))
                        (setf sigma (+ r b g sigma))
                        ;(if (> r 0)
                           ;(setf sigma (* sigma (sqrt (* 0.5 pi)) (/ 1 (* 6 (- width 2) (- height 2)))))
                           (setf sigma (* sigma ()))
                        ;)
                        (format t "~100$~%" sigma)) ))))))sigma)

(defun noise (input output)
   (estimate-noise (load-painting input) output))
   ;(write-image (estimate-noise (load-painting "images/the_scream.png")) "output/noise_scream.png"))
