;; ** list-thingy

(in-package :layers-utils)

;; *** list-thingy
;;; just a very basic list object with basic methods
(defclass list-thingy (thingy)
  ;; the index of the current element in data
  ((current :accessor current :initarg :current :type integer :initform 0)))

(defmethod print-object ((lt list-thingy) stream)
  (format stream "<List-thingy ~a>" (id lt)))

;; *** get-ids
;;; get the ids of all elements in the list
(defpubmethod get-ids ((lt list-thingy))
  (loop for element in (data lt)
	when (typep element 'thingy) collect (id element)))

;; *** get-current
;;; get current element of list
(defpubmethod get-current ((lt list-thingy))
  (when (data lt)
    (nth (current lt) (data lt))))

;;; alias for compatability
(defpubmethod see-current ((lt list-thingy))
  (get-current lt))

;; *** get-next
;;; proceed to next element of the list
(defpubmethod get-next ((lt list-thingy))
  (when (data lt)
    (let* ((data (data lt)))
      (progn
	(incf (current lt))
	(when (= (current lt) (length data))
	  (setf (current lt) 0))
	(nth (current lt) data)))))

;; *** see-next
;;; get next element in a list but don't change the current-slot
(defpubmethod see-next ((lt list-thingy))
  (when (data lt)
    (let* ((data (data lt))
	   (n (current lt)))
      (progn (incf n)
	     (when (= n (length (data lt)))
	       (setf n 0))
	     (nth n data)))))

;; *** add-to-list
(defpubmethod add-to-list ((lt list-thingy) &rest elements)
  (setf (data lt)
	(append (data lt) elements)))

;; *** see-with-relative-index
;;; get an element from the list with index (+ current relative)
;;; but don't change the current-slot
(defpubmethod see-with-relative-index ((lt list-thingy) relative-index)
  (when (data lt)
    (let* ((data (data lt))
	   (n (current lt)))
      (progn (incf n relative-index)
	     (setf n (mod n (length (data lt))))
	     (nth n data)))))

;; *** check-sanity
(defpubmethod check-sanity ((lt list-thingy) &optional (error-fun #'warn))
  ;; data should be a list
  (unless (listp (data lt))
    (funcall error-fun "check-sanity: weird data for list-thingy ~a: ~a"
	     (id lt) (data lt)))
  t)

(export '(list-thingy) :layers-utils)

;;;; EOF list-thingy.lsp
