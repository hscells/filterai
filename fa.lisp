;;; Filterai - The AI that created image filters based on paintings
;;; Uses the Opticl common lisp library - https://github.com/slyrus/opticl
;;; Harry Scells 2015

;; Load the image processing library
(load "quicklisp/setup.lisp")
(ql:quickload "opticl")

;; Define packages to use
(use-package 'opticl)

(defparameter *edge-kernel* #2A((0 1 0) (1 -4 1) (0 1 0)))
(defparameter *dilate-map* #3A(((-1 -1) (0 -1) (1 -1)) ((-1 0) (0 0) (1 0)) ((-1 1) (0 1) (1 1))))
;; Convert an image into RGB pixels
(defun load-painting (name)
   (let ((img (read-png-file name))) img))

;; Do some basic processing on the image (adjust colour values)
(defun write-image (img name)
   (write-png-file name img))

(defun 4-neighbors (img i j)
  (declare (type fixnum i j))
  (with-image-bounds (height width)
      img
    (values (when (> i 0) (list (1- i) j))             ; top
            (when (> j 0) (list i (1- j)))             ; left
            (when (< i (1- height)) (list (1+ i) j))   ; bottom
            (when (< j (1- width)) (list i (1+ j)))     ; right
            )))

(defun 8-neighbors (img i j)
  (declare (type fixnum i j))
  (with-image-bounds (height width)
      img
    (values (when (and (> i 0) (> j 0)) (list (1- i) (1- j))) ; top-left
            (when (> j 0) (list i (1- j)))                    ; left
            (when (and (< i (1- height)) (> j 0)) (list (1+ i) (1- j))) ; bottom-left
            (when (< i (1- height)) (list (1+ i) j)) ; bottom
            (when (and (< i (1- height)) (< j (1- width))) (list (1+ i) (1+ j))) ; bottom-right
            (when (< j (1- width)) (list i (1+ j))) ; right
            (when (and (> i 0) (< j (1- width))) (list (1- i) (1+ j))) ; top-right
            (when (> i 0) (list (1- i) j)) ; top
            )))

(defmacro multiple-value-list-remove-nulls (values)
  `(remove-if #'null (multiple-value-list ,values)))

(defun label-components (img &key (neighbor-function #'4-neighbors))
  (with-image-bounds (height width)
      img
    (let ((label-array (make-array (list height width)
                                   :element-type 'fixnum
                                   :initial-element 0))
          (stack)
          (label-value 0))
      (dotimes (i height)
        (dotimes (j width)
          (when (= 0 (aref label-array i j))
            (let ((current-label-value (multiple-value-list (pixel img i j))))
              (incf label-value)
              (setf (aref label-array i j) label-value)
              (mapcar (lambda (p)
                        (destructuring-bind (ni nj) p
                          (when (equalp current-label-value
                                        (multiple-value-list
                                         (pixel img ni nj)))
                            (push p stack)
                            (setf (aref label-array ni nj) label-value))))
                      (multiple-value-list-remove-nulls
                       (funcall neighbor-function img i j)))
              ;; now we walk through the list....
              (do ((k (pop stack) (pop stack)))
                  ((null k))
                (mapcar (lambda (p)
                          (destructuring-bind (ni nj) p
                            (when (and (equalp current-label-value
                                               (multiple-value-list
                                                (pixel img ni nj)))
                                       (= 0 (aref label-array ni nj)))
                              (push p stack)
                              (setf (aref label-array ni nj) label-value))))
                        (multiple-value-list-remove-nulls
                         (funcall neighbor-function img (car k) (cadr k)))))))))
      (map-array #'1- label-array))))

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

;; Perform an edge thinning algorithm;
;; http://homepages.inf.ed.ac.uk/rbf/HIPR2/thin.htm
(defun edge-thin (img)
   (typecase img
      (8-bit-rgb-image
         (locally (declare (type 8-bit-rgb-image img))
            (with-image-bounds (height width) img
               (loop for i below height do
                  (loop for j below width do
                     (setf c 0)
                     (setf w 0)
                     (setf l 0)
                     (setf v 255)
                     (setf empty nil)
                     (dolist (f (multiple-value-list-remove-nulls (8-neighbors img i j)))
                           (multiple-value-bind (_r _g _b)
                              (pixel img i j)
                              (declare (type (unsigned-byte 8) _r _g _b))
                              (multiple-value-bind (r1 g1 b1)
                                 (pixel img (first f) (second f))
                                 (declare (type (unsigned-byte 8) r1 g1 b1))
                                    ;((and (eq _r 0) (eq r1 255)) (setf empty nil))
                                    (if (and (eq _r 255) (eq r1 255)) (setf w (+ w 1)))
                                    (if (and (eq _r 255) (eq r1 0)) (setf c (+ c 1)))
                                    (if (and (eq _r 0) (eq r1 255)) (setf l (+ l 1)))
                                    ;(if (and (eq _r 0) (eq r1 255)) (setf w (+ w 1)))
                                    ;(setf c 6)
                                 )))
                     (multiple-value-bind (r g b)
                        (pixel img i j)
                        (declare (type (unsigned-byte 8) r g b))
                           (setf v r)
                           (if (eq c 8) (setf v 0))
                           ;(if (eq c 2) (setf v 255))
                           ;(if (eq w 3) (setf v 0))
                           ;(if (eq l 2) (setf v 100))
                           ;(if (eq c 5) (setf v 100))
                           (setf (pixel img i j)
                              (values v v v))))))))))


(defun threashold-image (img amount)
   (typecase img
      (8-bit-rgb-image
         (locally (declare (type 8-bit-rgb-image img))
            (with-image-bounds (height width) img
               (loop for i below height do
                  (loop for j below width do
                     (multiple-value-bind (r g b)
                        (pixel img i j)
                        (declare (type (unsigned-byte 8) r g b))
                        (if (> r amount) (setf v 255) (setf v 0))
                        (setf (pixel img i j)
                           (values v v v))))))))))
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
         '(( 1 -2  1)
           (-2  4 -2)
           ( 1 -2  1))))
   (setf img (discrete-convolve img m))
   (setf sigma (add-all-elements img))
   (write-image img output) ; output the image for now
   (typecase img
      (8-bit-rgb-image
         (locally (declare (type 8-bit-rgb-image img))
            (with-image-bounds (height width) img
               (setf sigma (* sigma (sqrt (* 0.5 pi)) (/ 1 (* 6 (- width 2) (- height 2)))))))))sigma)

;; Edge detect
; http://academypublisher.com/ijrte/vol01/no02/ijrte0102250254.pdf
(defun edge-detect (img output)
   (format t "Converting to greyscale~%")
   (greyscale-image img)
   
   (format t "Bluring image~%")
   (setf img (blur-image img))

   (format t "Dilate the image~%")
   (setf img (dilate (discrete-convolve img *edge-kernel*)
      (make-8-bit-rgb-image 3 3 :initial-element 3)))

   (format t "Applying edge detect~%")
   (setf img (discrete-convolve img *edge-kernel*))

   (format t "Threashold the image~%")
   (threashold-image img 30)

   (format t "Edge thinning (long process)~%")
   (edge-thin img)

   (format t "Writing to file~%")
   (write-image img output)) ; output the image for now

;; Shorthand function to run the noise estimation
(defun noise (input output)
   (estimate-noise (load-painting input) output))

;; Shorthand function to run the edge detection
(defun edge (input output)
   (edge-detect (load-painting input) output))

(defun e ()
   (load 'fa.lisp)
   (edge "images/the_scream.png" "output/scream_edge.png")
   ;(edge "images/starry_night.png" "output/starry_night_edge.png")
   )
