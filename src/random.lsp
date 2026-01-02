;; ** random

(in-package :layers-utils)

;; *** random-number
;;; stores a fixed-seed pseudo random number and can generate a new one.
(defclass random-number (thingy)
  ((data :accessor data :initarg :data :type float)
   (state :accessor state :initform 0 :type integer)
   (pcg :accessor pcg :initarg :pcg :initform (pcg::make-pcg) :type pcg:pcg)))

(defmethod print-object ((rn random-number) stream)
  (format stream "<Random Number ~a, Data: ~a>" (id rn) (data rn)))

;; *** random-number
;;; make an instance of random-number
(defpublic make-random-number (&optional (seed (random 100)))
  (make-instance 'random-number
		 :id (gensym)
		 :data seed
		 :pcg (pcg::make-pcg :seed seed)))

;;; get current value of a random-number object
(defpubmethod get-number ((rn random-number))
  (data rn))

;;; get current state of a random-number object
(defpubmethod get-state ((rn random-number))
  (state rn))

;; *** get-next
;;; get a random-number object to the next pseudo random number
(defpubmethod get-next ((rn random-number))
  (setf (state rn)
	(pcg::pcg-state (pcg rn))
	(data rn)
	(pcg::pcg-random (pcg rn) 1.0)))

;; *** rewind
;;; go to the nth last number
(defpubmethod rewind ((rn random-number) n)
  (pcg::pcg-rewind (pcg rn) (1+ n))
  (get-next rn))

;; *** set-state
;;; set the random-state of the prng
(defpubmethod set-state ((rn random-number) state)
  (prog1 (setf (pcg::pcg-state (pcg rn))
	       state)
    (rewind rn 0)))

(export '(random) :layers-utils)

;;;; EOF random.lsp
