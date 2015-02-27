;;; Filterai - The AI that created image filters based on paintings
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
(defun process-painting (img name)
   (typecase img
      (8-bit-rgb-image
         (locally (declare (type 8-bit-rgb-image img))
            (with-image-bounds (height width) img
            (time
               (loop for i below height do
                  (loop for j below width do
                     (multiple-value-bind (r g b)
                        (pixel img i j)
                        (declare (type (unsigned-byte 8) r g b))
                        (setf (pixel img i j)
                           (values (- 255 r) g b))))))))))
   (write-png-file name img))
