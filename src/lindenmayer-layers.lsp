;; ** lindenmayer-layers

(in-package :layers-utils)

;; *** lindenmayer-layers class
(defclass lindenmayer-layers (duration-layers)
  ((seed :accessor seed :initarg :seed)
   (rules :accessor rules :initarg :rules)
   (ratios :accessor ratios :initarg :ratios)
   (smallest :accessor smallest :initarg :smallest :type number :initform 0.1)))

;; *** regenerate
(defpubmethod regenerate ((ll lindenmayer-layers))
  (setf (data ll)
	(case (generation-method ll)
	  (lindenmayer
	   (lindenmayer
	    (total-duration ll) (seed ll) (rules ll) (ratios ll) (smallest ll)))
	  (compartmentalise
	   (compartmentalise
	    (total-duration ll) (seed ll) (rules ll) (ratios ll) (smallest ll)))
	  (simple (scale ll (total-duration ll)))
	  (t (error "re-gen-structure does not know how to gen structure of~
                       generation-method ~a" (generation-method ll))))
	(how-many ll)
	(length (data ll)))
  (format t "lindenmayer-layers ~a was regenerated" (id ll)))

;; *** make-lindenmayer-layers
;;; See lindenmayer-ratios.lsp to make sense of this.
;;; EXAMPLE
#|
(make-lindenmayer-layers '(2)
			 '((1 ((2 1)))
			   (2 ((3 1 3)))
			   (3 ((2))))
			 '((1 1)
			   (2 5)
			   (3 .2)))
|#
(defpublic make-lindenmayer-layers (seed rules ratios
					 &key id
					 (duration 10)
					 (generation-method 'lindenmayer)
					 (smallest 0.01))
  ;; make-instance
  (make-instance 'lindenmayer-layers
		 :id (or id seed)
		 :data (funcall generation-method duration seed rules ratios smallest)
		 :seed seed
		 :generation-method generation-method
		 :rules rules
		 :ratios ratios
		 :total-duration duration
		 :smallest smallest))

(export '(lindenmayer-layers seed rules ratios smallest) :layers-utils)

;; EOF lindenmayer-layers.lsp
