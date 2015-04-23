;;; Filterai - The AI that created image filters based on paintings
;;; Uses the Opticl common lisp library - https://github.com/slyrus/opticl
;;; Harry Scells 2015

;; Load the image processing library
(load "quicklisp/setup.lisp")
(ql:quickload "opticl")

;; Define packages to use
(use-package 'opticl)

(defparameter *edge-kernel* #2A((0 1 0) (1 -4 1) (0 1 0)))
(defparameter *dilate* #2A((0 1 0) (-1 4 1) (0 1 0)))
(defparameter *dilate-map* #3A(((-1 -1) (0 -1) (1 -1)) ((-1 0) (0 0) (1 0)) ((-1 1) (0 1) (1 1))))
(defparameter *gaussian* #2A((1 2 1) (2 4 2) (1 2 1)))

(declaim (ftype (function (fixnum fixnum fixnum fixnum fixnum fixnum) fixnum) l2-distance-3))


(defmacro multiple-value-list-remove-nulls (values)
  `(remove-if #'null (multiple-value-list ,values)))

(defun label-components (img &key (neighbor-function #'4-neighbors))
  (with-image-bounds (height width) img
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

(defun copy-array (src &key (element-type (array-element-type src))
									 (fill-pointer (and (array-has-fill-pointer-p src)
															  (fill-pointer src)))
									 (adjustable (adjustable-array-p src)))
  (let ((dims (array-dimensions src)))
	 ;; Dictionary entry for ADJUST-ARRAY requires adjusting a
	 ;; displaced array to a non-displaced one to make a copy.
	(let* ((src-displaced (make-array (reduce #'* dims)
												:displaced-to src
												:element-type element-type))
			  (dest (make-array dims :element-type element-type
											 :fill-pointer fill-pointer
											 :adjustable adjustable))
			  (dest-displaced (make-array (reduce #'* dims)
													:displaced-to dest
													:element-type element-type)))
		(replace dest-displaced src-displaced)
		dest)))

(defun l2-distance-3 (pixel1a pixel1b pixel1c pixel2a pixel2b pixel2c)
  (declare (type fixnum pixel1a pixel1b pixel1c pixel2a pixel2b pixel2c)
			  (optimize (speed 3) (safety 0)))
			  (let ((d1 (- pixel1a pixel2a))
				  (d2 (- pixel1b pixel2b))
				 (d3 (- pixel1c pixel2c)))
	 (declare (type fixnum d1 d2 d3))
	 (the fixnum (+ (the fixnum (* d1 d1))
						 (the fixnum (* d2 d2))
						 (the fixnum (* d3 d3))))))

(defun k-means (image k &key (max-iterations 20))
  (declare (type fixnum k))
  (typecase image
	 (8-bit-rgb-image
	  (with-image-bounds (height width channels) image
		 (let ((means (make-array (list k 1 3)
										  :element-type '(unsigned-byte 32)))
				 (counts (make-array k :element-type 'fixnum))
				 (z (make-array (list height width) :element-type 'fixnum)))
			(declare (type (simple-array fixnum (* *)) z)
						(type 8-bit-rgb-image image)
						(type (simple-array (unsigned-byte 32) (* * 3)) means))
			(flet (
				(recompute-means ()
						(declare (type 8-bit-rgb-image image)
									(type (simple-array fixnum (* *)) z)
									(type (simple-array fixnum (*)) counts)
									(optimize (speed 3)))
						;; clear out the old values
						(dotimes (q k)
						  (setf (pixel means q 0) (values 0 0 0))
						  (setf (aref counts q) 0))

						;; use the means vector first as an accumulator to hold
						;; the sums for each channel, later we'll scale by (/
						;; num-pixels)
						(do-pixels (i j) image
						  (let ((m (aref z i j)))
							 (multiple-value-bind (v1 v2 v3)
								  (pixel image i j)
								(multiple-value-bind (m1 m2 m3)
									 (pixel means m 0)
								  (setf (pixel means m 0)
										  (values
											(+ v1 m1)
											(+ v2 m2)
											(+ v3 m3)))))
							 (let* ((cluster (aref z i j))
									  (cluster-count (aref counts cluster)))
								(setf (aref counts cluster)
										(logand #xffffffff (1+ cluster-count))))))
						(dotimes (q k)
						  (when (plusp (aref counts q))
							 (multiple-value-bind (m1 m2 m3)
								  (pixel means q 0)
								(let ((factor (aref counts q)))
								  (setf (pixel means q 0)
										  (values (truncate (/ m1 factor))
													 (truncate (/ m2 factor))
													 (truncate (/ m3 factor))))))))
						(let ((new-means-list
								 (loop for count across counts
									 for i below k
									 collect (list count (pixel* means i 0)))))
						  (loop for i fixnum below k
							  for (count mean) in (sort new-means-list #'> :key #'first)
							  do
								 (setf (pixel* means i 0) mean)
								 (setf (aref counts i) count))))

					 (assign-to-means ()
						(declare (type 8-bit-rgb-image image)
									(optimize (speed 3)))
						(do-pixels (i j) image
						  (setf (aref z i j)
								  (let (min-val nearest-mean)
									 (loop for q below k
										 do (let ((dist (multiple-value-call #'l2-distance-3
																(pixel image i j)
																(pixel means q 0))))
												(when (or (null min-val) (< dist min-val))
												  (setf min-val dist
														  nearest-mean q))))
									 nearest-mean)))))

			  ;; randomly assign pixel values to the k means
			  (loop for i below k
				  for y = (random height)
				  for x = (random width)
				  do (setf (pixel means i 0)
							  (pixel image y x)))

			  (loop for iter below max-iterations
				  with stop = nil
				  with oldz
				  until stop
				  do
				  (assign-to-means)
				  (recompute-means)
				  (when (and oldz (equalp oldz z))
					 (setf stop t))
				  (setf oldz (copy-array z)))


			  (typecase image
				  (8-bit-rgb-image
					  (locally (declare (type 8-bit-rgb-image image))
						  (with-image-bounds (height width) image
							  (loop for i below height do
								  (loop for j below width do
									  (multiple-value-bind (r g b)
										  (pixel image i j)
										  (declare (type (unsigned-byte 8) r g b))
										  (setf v (aref z i j))
										  (setf (pixel image i j)
											(values (aref means v 0 0) (aref means v 0 1) (aref means v 0 2))))))))))))))))
			  ;(values means z)))))))

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
	 (values (when (> i 0) (list (1- i) j))				 ; top
				(when (> j 0) (list i (1- j)))				 ; left
				(when (< i (1- height)) (list (1+ i) j))	; bottom
				(when (< j (1- width)) (list i (1+ j)))	  ; right
				)))

(defun 8-neighbors (img i j)
  (declare (type fixnum i j))
  (with-image-bounds (height width)
		img
	 (values (when (and (> i 0) (> j 0)) (list (1- i) (1- j))) ; top-left
				(when (> j 0) (list i (1- j)))						  ; left
				(when (and (< i (1- height)) (> j 0)) (list (1+ i) (1- j))) ; bottom-left
				(when (< i (1- height)) (list (1+ i) j)) ; bottom
				(when (and (< i (1- height)) (< j (1- width))) (list (1+ i) (1+ j))) ; bottom-right
				(when (< j (1- width)) (list i (1+ j))) ; right
				(when (and (> i 0) (< j (1- width))) (list (1- i) (1+ j))) ; top-right
				(when (> i 0) (list (1- i) j)) ; top
				)))

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
							(setf neighbors 0)
							(dolist (f (multiple-value-list-remove-nulls (8-neighbors img i j)))
									(multiple-value-bind (_r _g _b)
										(pixel img i j)
										(declare (type (unsigned-byte 8) _r _g _b))
										(multiple-value-bind (r1 g1 b1)
											(pixel img (first f) (second f))
											(declare (type (unsigned-byte 8) r1 g1 b1))
												(if (eq r1 255) (setf neighbors (+ neighbors 1))))))
							(multiple-value-bind (r g b)
								(pixel img i j)
								(declare (type (unsigned-byte 8) r g b))
									(if (= neighbors 0) (setf v 10) (setf v r))
									;(if (= neighbors 1) (setf v 10) (setf v r))
									(setf (pixel img i j)
										(values v v v))))))))))

;; Perform an edge thinning algorithm;
;; http://homepages.inf.ed.ac.uk/rbf/HIPR2/thin.htm
;; Would like to get this to work to remove all the single pixels
(defun edge-thin-means (img img2)
	(typecase img
		(8-bit-rgb-image
			(locally (declare (type 8-bit-rgb-image img))
				(with-image-bounds (height width) img
					(loop for i below height do
						(loop for j below width do
							(setf neighbors 0)
							(dolist (f (multiple-value-list-remove-nulls (8-neighbors img i j)))
									(multiple-value-bind (_r _g _b)
										(pixel img i j)
										(declare (type (unsigned-byte 8) _r _g _b))
										(multiple-value-bind (r1 g1 b1)
											(pixel img (first f) (second f))
											(declare (type (unsigned-byte 8) r1 g1 b1))
                                    (setf v r1)
												(if (not (eq r1 _r)) (setf neighbors (+ neighbors 1))))))
							(multiple-value-bind (r g b)
								(pixel img2 i j)
								(declare (type (unsigned-byte 8) r g b))
                           (if (> neighbors 0)
   									(setf (pixel img2 i j)
   										(values v v v)))))))))))

;; Perform a threasholding operation on the image
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

;; Perform a threasholding operation on the image
(defun threashold-image-colour (img amount)
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
								(if (>= grey amount)
   								(setf (pixel img i j)
   									(values 0 0 0)))))))))))



;; overlay an egde over the top of another image
(defun overlay (skeleton origional)
	(typecase origional
		(8-bit-rgb-image
			(locally (declare (type 8-bit-rgb-image origional))
				(with-image-bounds (height width) origional
					(loop for i below height do
						(loop for j below width do
							(multiple-value-bind (r g b)
								(pixel skeleton i j)
								(declare (type (unsigned-byte 8) r g b))
								(multiple-value-bind (r1 g1 b1)
									(pixel origional i j)
									(declare (type (unsigned-byte 8) r1 g1 b1))
									(setf grey (floor (/ (+ r1 g1 b1) 3)))
									(if (= r 255)
										(setf (pixel origional i j)
											(values grey grey grey))
										(setf (pixel origional i j)
											(values r1 g1 b1))))))))))))

(defun overlay-stroke (source strokes)
   (typecase source
      (8-bit-rgb-image
         (locally (declare (type 8-bit-rgb-image source))
            (with-image-bounds (height width) source
               (loop for i below height do
                  (loop for j below width do
                     (multiple-value-bind (r g b)
                        (pixel strokes i j)
                        (declare (type (unsigned-byte 8) r g b))
                        (multiple-value-bind (r1 g1 b1)
                           (pixel source i j)
                           (declare (type (unsigned-byte 8) r1 g1 b1))
                           (if (< r 64) ;; 64 used for now, needs to be something later
                              (setf (pixel source i j)
                                 (values r1 g1 b1))
                              (setf (pixel source i j)
                                 (values r g b))))))))))))

;; unused
(defun colourise-components (img)
	(setf comp (label-components img))
	(setf c 0)
	(setf l 0)
	(setf col (list (random 255) (random 255) (random 255)))
	(typecase img
		(8-bit-rgb-image
			(locally (declare (type 8-bit-rgb-image img))
				(with-image-bounds (height width) img
					(loop for i below height do
						(loop for j below width do
							(setf c (nth i (nth j comp)))
							(multiple-value-bind (r g b)
								(pixel img i j)
								(declare (type (unsigned-byte 8) r g b))
								(if (and (not (eq l ))))
								(setf (pixel img i j)
									(values (first col) (second col) (third col))))
							(setf l c)
							(if (and (not (eq c l)) (not (eq c 0)))
								(setf col (list (random 255) (random 255) (random 255)))))))))))

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
(defun blob-detect (input output)
	(format t "~S~%" input)
	(setf img (load-painting input))
	(setf org (load-painting input))

	(format t "Converting to greyscale~%")
	(greyscale-image img)
	(greyscale-image org)

	(format t "Bluring image~%")
	(setf img (blur-image img))

	(format t "Dilate the image~%")
	(setf img (dilate (discrete-convolve img *edge-kernel*)
		(make-8-bit-rgb-image 3 3 :initial-element 3)))

	(format t "Applying edge detect~%")
	(setf img (discrete-convolve img *edge-kernel*))

	(format t "K-means clustering~%")
	(k-means img 2)

	(format t "Threashold the image~%")
	(threashold-image img 10)

	(format t "Edge thinning~%")
	(edge-thin img)

	(format t "overlaying image~%")
	(overlay img org)

	;(format t "Dilate the image~%")
	;(setf org (dilate (discrete-convolve org *edge-kernel*)
	;	(make-8-bit-rgb-image 2 2 :initial-element 2)))
	;(format t "labelling ~%")
	;(setf img (colourise-components img))
	(format t "Bluring image~%")
	(setf org (blur-image org))

	(format t "K-means clustering~%")
	(k-means org 5)


	(format t "Writing to file~%")
	(write-image org output)) ; output the image for now

;; Shorthand function to run the noise estimation
(defun noise (input output)
	(estimate-noise (load-painting input) output))

;; Shorthand function to run the edge detection
(defun blob (input output)
	(blob-detect input output))

(defun in (key haystack)
	(not (not (member key haystack))))

(defun sum-components (array img)
	(setf blobs '())
	(typecase img
		(8-bit-rgb-image
			(locally (declare (type 8-bit-rgb-image img))
				(with-image-bounds (height width) img
					(loop for i below height do
						(loop for j below width do
							(if (>= (aref array i j) (length blobs))
								(setf blobs (append blobs (list 1)))
								(setf (nth (aref array i j) blobs) (+ (nth (aref array i j) blobs) 1))))))))) blobs)

(defun sum-l (l)
   (apply '+ l))

(defun avg-l (l)
   (float (/ (sum-l l) (length l))))

(defun max-l (l)
   (apply 'max l))

(defun min-l (l)
   (apply 'min l))

(defun label (image)
	(setf img (load-painting image))
	(sort (sum-components (label-components img) img) #'>))

(defun stroke (source reference stroke)
   (format t "painting ~S sized strokes~%" stroke)
   (setf canvas (copy-image reference))
   (threashold-image-colour canvas stroke)
   (setf canvas (blur-image canvas))
   (overlay-stroke reference canvas)
   (write-image reference "output/test.png") reference)

(defun paint (source r)
   (format t "~S layers to paint~%" (length r))
   (setf r (remove-duplicates r))
   (setf source (load-painting source))
   (setf reference (copy-image source))
   (loop for i in r do
      (setf source (stroke source reference i))) source)
      ;(setf source (blur-image source))) source)

(defun filter (filter input output)
   (format t "Labelling components~%")
   (setf blobs (label filter))
   ;(setf blobs '(140 100 60 20))
   (format t "Painting photo~%")
   (setf painting (paint input blobs))
   (write-image painting output))

(defun e ()
	(load 'fa.lisp)
	(edge "images/no5.png" "output/no5.png")
	;(edge "images/odetojoy.png" "output/odetojoy_edge.png")
	;(edge "images/the_scream.png" "output/scream_edge.png")
	;(edge "images/odetojoy.png" "output/odetojoy_edge.png")
	;(edge "images/im_blauen.png" "output/im_blauen_edge.png")
	;(edge "images/pacman_game.png" "output/pacman_game_edge.png")
	;(edge "images/stardust.png" "output/stardust_edge.png")
	;(edge "images/starry_night.png" "output/starry_night_edge.png")
	)

(defun f ()
   (filter "output/odetojoy_edge.png" "images/lenna.png" "output/lenna_painting_ode_to_joy.png")
   (filter "output/scream.png" "images/lenna.png" "output/lenna_painting_scream.png"))

(defun stats ()
   (setf blobs (label "output/scream.png"))
   (format t "Average size stroke ~S~%" (avg-l blobs))
   (format t "Minimum size stroke ~S~%" (min-l blobs))
   (format t "Maximum size stroke ~S~%" (max-l blobs))
   )
