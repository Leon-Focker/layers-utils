;; ** show-duration-layers

(in-package :layers-utils)

(defun show-duration-layers (duration-layers-data
			     &optional file (size-factor 1))
  (let* ((h (round (* 100 size-factor)))
	 (w (round (* 1000 size-factor)))
	 (ls (loop for l in duration-layers-data
		   collect (mapcar (lambda (x) (rescale x 0 (apply #'+ l) 0 1)) l)))
	 (length (length ls))
	 (height (* h length))
	 (array (make-array `(,height ,w)))
	 (n 0)
	 (sublist '())
	 (sublength 0)
	 (index 0)
	 (color 0)
	 (element 0)
	 (name (or file (layers-utils-path "duration-layers.png"))))
    (loop for y from 0 to (1- height) do
      (setf n (floor y h))
      (setf sublist (nth n ls))
      (setf sublength (length sublist))
      (loop for x from 0 to (1- w) do
	(setf index (decider (/ x w) sublist))
	(setf element (nth index sublist))
	(setf color
	      (imago:make-color (floor (* (expt (- 1 (/ element 1)) 15) 255))
				(floor (* (/ n length) 100))
				(floor (* (/ index sublength) 100)))) ;(/ n length)
	(setf (aref array y x) color)))
    (imago:write-png
     (make-instance 'imago:rgb-image
		    :pixels array)
     name)
    name))

(defpubmethod visualize-duration-layers ((dl duration-layers)
					 &optional file (size-factor 1/2))
  (format t "~&visualizing duration-layers: ~a..." (id dl))
  (show-duration-layers (data dl) file size-factor))

#|
(ql:quickload :sketch)

(in-package :layers)

(defun show-structure (structure-list)
  ;(sketch::defsketch structure ())
  ;(sketch::make-instance 'structure)
  (sketch::defsketch structure ((title "structure")
				(width 1000)
				(height 600))
    (loop for ls in structure-list and k from 0 do
	 (setf ls (mapcar (lambda (x) (sc::rescale x
						   0
						   (loop for j in ls sum j)
						   0
						   1000))
			  ls))
	 (loop for val in ls and last = 0 then (+ last val) and i from 0 do
	      (sketch::with-pen
		  (sketch::make-pen :fill
				    (sketch::lerp-color sketch::+black+ sketch::+white+
						    (/ val (apply #'max ls))))
		(sketch::rect last (* k 100) val 100))))))
|#

;; EOF show-duration-layers.lsp
