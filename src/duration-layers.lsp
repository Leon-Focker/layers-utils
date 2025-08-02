;; ** duration-layers

(in-package :layers-utils)

;; *** duration-layers class
;;; a list of lists of durations and some information about it - for example
;;; how it was generated.
(defclass duration-layers (list-thingy)
  ;; depth: how many different lists of same duration (sublists).
  ((how-many :accessor how-many :initarg :how-many :type integer :initform 0)
   (generation-method :accessor generation-method :initarg :generation-method
		      :initform 'simple)
   (total-duration :accessor total-duration :initarg :total-duration
		   :type number :initform 0)))

(defmethod update-total-duration ((dl duration-layers))
  (setf (total-duration dl)
	(loop for ls in (data dl) maximize (apply #'+ ls))))

(defmethod initialize-instance :after ((dl duration-layers) &rest initargs)
  (declare (ignore initargs))
  ;; sanity-checks
  (unless (listp (data dl))
    (error "data in duration-layers (~a) must be a list: ~a" (id dl) (data dl)))
  (unless (and (car (data dl))
	       (loop for ls in (data dl)
		     always (loop for i in ls always (numberp i))))
    (error "data in duration-layers (~a) seems to be faulty: ~a"
	   (id dl) (data dl)))
  (setf (how-many dl)
	(length (data dl)))
  (unless (total-duration dl)
    (update-total-duration dl)))

;; *** print-object
(defmethod print-object ((dl duration-layers) stream)
  (format stream "~&Duration-layers:        ~a ~
                  ~&with this many layers:  ~a"
	  (id dl)
	  (how-many dl)))

;; *** scale-smallest-value-to
(defpubmethod scale-smallest-value-to ((dl duration-layers) new-smallest-value)
  (let* ((data (data dl))
	 (minimum (loop for ls in data minimize (apply #'min ls)))
	 (scaler (/ new-smallest-value minimum)))
    (setf (data dl)
	  (loop for ls in data
		collect (if (atom ls) (* ls scaler 1.0)
			    (loop for i in ls collect (* i scaler)))))
    (update-total-duration dl)
    dl))

;; *** scale-biggest-value-to
(defpubmethod scale-biggest-value-to ((dl duration-layers) new-biggest-value)
  (let* ((data (data dl))
	 (maximum (loop for ls in data maximize (apply #'max ls)))
	 (scaler (/ new-biggest-value maximum)))
    (setf (data dl)
	  (loop for ls in data
		collect (if (atom ls) (* ls scaler 1.0)
			    (loop for i in ls collect (* i scaler)))))
    (update-total-duration dl)
    dl))

;; *** scale
;;; loop through all lists in the data of a duration-layers object
;;; and scale the duration of each to the target-duration.
(defpubmethod scale ((dl duration-layers) target-duration)
  (update-total-duration dl)
  (let ((scaler (/ target-duration (total-duration dl))))
    (setf (data dl)
	  (loop for ls in (data dl)
		collect (loop for i in ls collect (* i scaler))))
    (update-total-duration dl)
    dl))

;; *** make-duration-layers
(defpublic make-duration-layers (id data)
  (make-instance 'duration-layers
		 :id id
		 :data data))

(export '(duration-layers how-many generation-method total-duration) :layers-utils)

;;;; EOF duration-layers.lsp
