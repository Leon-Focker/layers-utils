;; ** soundpile

(in-package :layers-utils)

;; *** soundpile
(defclass soundpile (list-thingy)
  ())

(defmethod initialize-instance :after ((sp soundpile) &rest initargs)
  (declare (ignore initargs))
  (unless (listp (data sp))
    (error "data in soundpile is not a list: ~a" (data sp)))
  (unless
      (loop for sf in (data sp)
	    always (and (typep sf 'soundfile)
			(check-sanity sf #'error)))
    (error "not all elements in this soundpiles data (~a) are sane soundfiles!"
	   (id sp))))

(defmethod print-object ((sp soundpile) stream)
  (format stream "<Soundpile ~a>" (id sp)))

;; *** make-soundpile
(defpublic make-soundpile (id list-of-soundfiles)
  (make-instance 'soundpile :id id :data list-of-soundfiles))

;; *** add-to-pile
;;; stores a soundfile in a soundpile. When there already is a soundfile with
;;; the same id on the pile, remove it.
(defpubmethod add-to-pile ((sf soundfile) (sp soundpile)
			&key (warn-if-double t))
  (if (null (data sp))
      (setf (data sp) (list sf))
      (let* ((double (member (id sf)
			     (get-ids sp))))
	(if double
	    (progn
	      (when warn-if-double
		(warn "the id ~a is already in the soundpile ~a ~
                       and will be replaced!"
		      (id sf) (id sp)))
	      (setf (data sp)
		    (remove-nth (- (length (data sp))
				   (length double))
				(data sp)))
	      (push sf (data sp)))
	    (push sf (data sp))))))

;; *** soundpile-from-folder
;;; add all soundfiles found in folder into one soundpile.
;;; - fft-size must be a power of 2 and starts at the beginning of the sample
;;;   when fft-size is nil, the biggest one possible is chosen
(defpubmethod soundpile-from-folder (id path-to-folder
				  &key analyse fft-size id-uniquifier)
  (let* ((sp (make-soundpile id '()))
	 (files (get-sndfiles path-to-folder))
	 (names (loop for i in files collect (pathname-name i))))    
    (when (null files) (warn "no soundfiles found in ~a" path-to-folder))
    (loop for file in files and name in names
	  for id = (read-from-string
		    (if id-uniquifier
			(format nil "~a-~a" id-uniquifier name)
			name))
	  do
	     (handler-case
		 (add-to-pile
		  (prog1
		      (let ((sf (make-soundfile
				 id
				 file
				 :directory "")))
			(when analyse
			  (analyse-soundfile sf :fft-size fft-size))
			sf)
		    (format t "~&storing file: ~a" id))
		  sp)
	       (t (error)
		 ;; This block will be executed for any type of error
		 (format t "~&An error occurred while storing ~a: ~&~A~%"
			 file error))))
    sp))

;; *** combine-soundpiles
;;; combine two ssoundpiles into a new one
(defpubmethod combine-soundpiles ((sp1 soundpile)
			       (sp2 soundpile)
			       new-id)
  (make-soundpile
   new-id
   (append (data sp1) (data sp2))))

;; *** get-paths
;;; show all paths to all sounds on the soundpile
(defpubmethod get-paths ((sp soundpile))
  (loop for sf in (data sp) collect (path sf)))

;; *** get-all
;;; gets list of all values in a certain slot of the soundfiles in the soundpile
(defpubmethod get-all (slot (sp soundpile) &optional error)
  (loop for sf in (data sp)
	do (when (and (not (funcall slot sf)) error)
	     (error "get-all found no value for ~a in sf ~a" slot (id sf)))
	collect (funcall slot sf)))

;; *** check-sanity
(defpubmethod check-sanity ((sp soundpile) &optional (error-fun #'warn))
  (unless
      (loop for sf in (data sp)
	    always (and (typep sf 'soundfile)
			(check-sanity sf error-fun)))
    (funcall error-fun "check-sanity: not all elements of soundpile ~a are ~
                        sane soundfiles!"
	     (id sp)))
  t)

;; *** make-load-form
(defmethod make-load-form ((sp soundpile) &optional environment)
  (declare (ignore environment))
  `(make-instance 'soundpile
		  :id ',(id sp)
		  :data ,(cons 'list
			       (loop for sf in (data sp)
				     collect (make-load-form sf)))))

;; *** store-in-text-file
;;; store a soundpile in a text file, so the analysis can be skipped by reading
;;; in the soundfiles.
(defpubmethod store-in-text-file ((sp soundpile) &optional file)
  (let* ((file (or file
		   (format nil "~a~a-load-file.txt"
			   *layers-utils-src-dir* (id sp)))))
    (write-to-file file (make-load-form sp))
    (format t "~&wrote ~a into ~a" (id sp) file)))

(export '(soundpile) :layers-utils)

;; EOF soundpile.lsp


