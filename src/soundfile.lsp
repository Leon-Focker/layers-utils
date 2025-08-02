;; ** soundfile

(in-package :layers-utils)

;; *** soundfile

(defclass soundfile (thingy)
  ((name :accessor name :initarg :name :initform nil)
   (path :accessor path :initarg :path :initform nil)
   ;; duration in seconds
   (duration :accessor duration :initarg :duration :initform 0)
   ;; duration in samples
   (total-samples :accessor total-samples :initarg :total-samples :initform 0)
   ;; samplerate of the soundfile
   (samplerate :accessor samplerate :initarg :samplerate :initform 48000)
   ;; loudest sample in the soundfile
   (peak :accessor peak :initarg :peak :initform nil)
   (peak-index :accessor peak-index :initarg :peak-index :initform 0)
   ;; analysed, not user set data:
   ;; spectral centroid
   (centroid :accessor centroid :initarg :centroid :initform nil)
   ;; spectral spread
   (spread :accessor spread :initarg :spread :initform nil)
   ;; spectral flatness
   (flatness :accessor flatness :initarg :flatness :initform nil)
   ;; dominant frequency
   (dominant-frequency :accessor dominant-frequency :initarg :dominant-frequency
		       :initform nil)
   ;; envelope smoothness
   (smoothness :accessor smoothness :initarg :smoothness :initform nil)
   ;; biggest jump up in the envelope
   (transient :accessor transient :initarg :transient :initform nil)))

(defmethod print-object ((sf soundfile) stream)
  (format stream "<SOUNDFILE ~a>" (id sf)))

;; *** spectral-analysis
;;; dominant frequency, centroid, flatness and spread of soundfile.
;;; file can either be an array or the path to a file.
;;; TODO would be more fun to get more than one fft frame...
;;;   also getting a dominant-frequency is kinda wonky when the base-freq
;;;   doesn't match the root harmonic of the file.
#+clm
(defpublic spectral-analysis (file
			      &key
			      fft-size
			      (srate 48000)
			      (window clm::hanning-window))
  (let* ((ar (not (stringp file)))
	 (srate (if ar srate (clm::sound-srate file)))
	 (sample-len (if ar (length file) (soundfile-duration file)))
	 ;; if not supplied, fft-size is set to the hightes possible power of two
	 ;; (up until ca. a second)
	 (fft-size (or fft-size
		       (loop with num = 1 for i from 1 until (or (> num srate)
								 (> num sample-len))
			     finally (return (expt 2 (- i 2)))
			     do (setf num (* num 2)))))
	 ;; by repeating fft-size/4 times we take into account freqs up to 12kHz
	 (repeats (/ fft-size 4))
	 (base-freq (/ srate fft-size))
	 (arithmetic-mean 0)
	 (geometric-mean 0)
	 (a 0)
	 (b 0)
	 (c 0)
	 (d 0)
	 (e '(0 0))
	 (m '()))
    (setf m (clm::fft-from-file file
				:fft-size fft-size
				:window window))
    (loop for i from 0 and j in m repeat repeats do
      (when (> j (car e)) (setf e (list j (* i base-freq 1.0))))
      (incf a (* i base-freq (expt j 2)))
      (incf b (expt j 2))
      (incf arithmetic-mean j)
      (incf geometric-mean (log j)))
    (setf arithmetic-mean (/ arithmetic-mean repeats))
    (setf geometric-mean (exp (/ geometric-mean repeats)))
    (setf c (/ a b))
    (loop for i from 0 and j in m repeat repeats do
      (incf d (* (expt (- (* i base-freq) c) 2)
		 (expt j 2))))
    (values c				; centroid
	    (sqrt (/ d b))		; spread
	    (/ geometric-mean		; flatness
	       arithmetic-mean)
	    (cadr e)			; dominant frequency
	    )))

;; *** analyse-soundfile
;;; analyse some basic parameters and write them into the sf-object
#+clm
(defpubmethod analyse-soundfile ((sf soundfile) &key fft-size (srate 48000))
  (let* ((array (clm::table-from-file (path sf)))
	 (envelope (clm::envelope-follower array))
	 (max (max-of-array-with-index array t)))
    (multiple-value-bind (c s f d)
	     (spectral-analysis array :fft-size fft-size :srate srate)
	   (setf (centroid sf) c
		 (spread sf) s
		 (flatness sf) f
		 (dominant-frequency sf) d))
    (setf (peak sf) (first max))
    (setf (peak-index sf) (second max))
    (setf (smoothness sf) (flatness-of-list envelope))
    (setf (transient sf) (biggest-jump (reduce-by envelope 10)))) ; was 50
  sf)
#-clm
(defpubmethod analyse-soundfile ((sf soundfile) &key fft-size (srate 48000))
  (error "You need to have the CLM library loaded to analyse soundfiles! ~
          After loading it, recompile layers-utils."))

;; *** make-soundfile
;;; create an instance of a soundfile.
(defpublic make-soundfile (id
			   relative-path
			   &key
			   (directory *layers-utils-home-dir*)
			   analyse)
  (let* ((path (format nil "~a~a" directory relative-path)))
    (unless (probe-file path)
      (warn "~&the file with path ~a does not exist" path))
    (let ((sf (make-instance 'soundfile
			     :id id
			     :name (pathname-name relative-path)
			     :path path
			     :duration #+clm(soundfile-duration path) #-clm 0
			     :total-samples #+clm(soundfile-framples path) #-clm 0
			     :samplerate #+clm(soundfile-samplerate path) #-clm 48000)))
      (if analyse (analyse-soundfile sf) sf))))

;; *** check-sanity
(defpubmethod check-sanity ((sf soundfile) &optional (error-fun #'warn))
  ;; These should be strings
  (loop for slot in '(path) do
    (unless (stringp (funcall slot sf))
      (funcall error-fun "check-sanity: weird ~a for soundfile ~a: ~a"
	       slot (id sf) (funcall slot sf))))
  ;; These should be nil, a number or 'random
  (loop for slot in '(duration total-samples samplerate peak-index
		      peak centroid spread flatness dominant-frequency
		      smoothness transient)
	do (unless (or (not (funcall slot sf)) (numberp (funcall slot sf))
		       (equal (funcall slot sf) 'random))
	     (funcall error-fun "check-sanity: weird ~a for soundfile ~a: ~a"
		      slot (id sf) (funcall slot sf))))
  t)

;; *** make-load-form
(defmethod make-load-form ((sf soundfile) &optional environment)
  (declare (ignore environment))
  `(make-instance 'soundfile
		  :id ',(id sf)
		  :data ',(data sf)
		  :name ',(name sf)
		  :path ',(path sf)
		  :duration ',(duration sf)
		  :total-samples ',(total-samples sf)
		  :samplerate ',(samplerate sf)
		  :peak ',(peak sf)
		  :peak-index ',(peak-index sf)
		  :centroid ',(centroid sf)
		  :spread ',(spread sf)
		  :flatness ',(flatness sf)
		  :dominant-frequency ',(dominant-frequency sf)
		  :smoothness ',(smoothness sf)
		  :transient ',(transient sf)))

;; *** store-in-text-file
;;; store a soundfile-instance in a text file, so the analysis can be skipped
;;; by reading from the text file.
(defpubmethod store-in-text-file ((sf soundfile) &optional file)
  (let* ((file (or file
		   (format nil "~a~a-load-file.txt"
			   *layers-utils-src-dir* (id sf)))))
    (write-to-file file (make-load-form sf))
    (format t "~&wrote ~a into ~a" (id sf) file)))

(export '(name path duration total-samples samplerate peak peak-index
	  centroid spread flatness dominant-frequency smoothness transient)
	:layers-utils)

;;;; EOF soundfile.lsp
