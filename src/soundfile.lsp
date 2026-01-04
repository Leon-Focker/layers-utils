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
   ;; (estimated) pitch, fundamental frequency 
   (fundamental-frequency :accessor fundamental-frequency
			  :initarg :fundamental-frequency
			  :initform nil)
   ;; envelope smoothness
   (smoothness :accessor smoothness :initarg :smoothness :initform nil)
   ;; biggest jump up in the envelope
   (transient :accessor transient :initarg :transient :initform nil)))

(defmethod print-object ((sf soundfile) stream)
  (format stream "<SOUNDFILE ~a>" (id sf)))

;; *** spectral-analysis
;;; dominant frequency, centroid, flatness, spread and estimated pitch of
;;; soundfile.
;;; - file: can either be an array or the path to a file.
;;; - fft-size: size of the fft window, if nil, one is chosen for you.
;;; - multiple-ffts: when t, run multiple overlapping ffts for the entire length
;;;   of the file.
#+clm
(defpublic spectral-analysis (file
			      &key
			      fft-size
			      (multiple-ffts t)
			      (srate 48000)
			      (window clm::hanning-window)
			      &aux (is-array (arrayp file)))
  (unless (or is-array (stringp file))
    (error "spectral-analysis: file should be a string or array!"))
  (let* ((srate (if is-array srate (clm::sound-srate file)))
	 (sample-len (if is-array (length file) (soundfile-duration file)))
	 (fft-size (or fft-size
		       (ash 1 (1- (integer-length (min sample-len srate))))))
	 (base-freq (/ srate fft-size))
	 (nr-of-frames (if multiple-ffts
			   (1- (* (floor sample-len fft-size) 2))
			   1))
	 (centroid '())
	 (spread '())
	 (flatness '())
	 (dominant-freq '())
	 (fundamental-freq '()))
    (unless (<= fft-size sample-len)
      (error "spectral-analysis: fft-size ~a too big for sample of length ~a"
	     fft-size sample-len))
    ;; do as many ffts as nr-of-frames
    (loop for i from 0 below nr-of-frames
	  for sample = (* i (floor fft-size 2))
	  with nr-of-harmonics = (/ fft-size 2)
	  for mags = (subseq (clm::fft-from-file
			      file
			      :start-sample sample
			      :fft-size fft-size
			      :window window)
			     0
			     nr-of-harmonics)
	  ;; only use half of the bins, rest is above nyquist
	  for arithmetic-mean = 0
	  for geometric-mean = 0
	  for a = 0
	  for b = 0
	  for c = 0
	  for d = 0
	  for dom-freq = '(0 0)
	  do (loop for j from 0 and m in mags
		   for freq = (* j base-freq)
		   do (when (> m (car dom-freq)) (setf dom-freq (list m j)))
		      (incf a (* freq (expt m 2)))
		      (incf b (expt m 2))
		      (incf arithmetic-mean m)
		      (incf geometric-mean (log m)))
	     (setf arithmetic-mean (/ arithmetic-mean nr-of-harmonics))
	     (setf geometric-mean (exp (/ geometric-mean nr-of-harmonics)))
	     (setf c (/ a b))
	     (loop for j from 0 and m in mags repeat nr-of-harmonics
		   do (incf d (* (expt (- (* j base-freq) c) 2)
				 (expt m 2))))
	     (push c centroid)
	     (push (sqrt (/ d b)) spread)
	     (push (/ geometric-mean arithmetic-mean) flatness)
	     ;; peak interpolation for more accurate dom-freq
	     (let* ((peak-idx (cadr dom-freq))
		    (new-peak-idx
		      (if (or (= peak-idx 0) (= peak-idx (1- nr-of-harmonics)))
			  peak-idx
			  (+ (parabolic-peak-offset
			      (nth (1- peak-idx) mags)
			      (nth peak-idx mags)
			      (nth (1+ peak-idx) mags))
			     peak-idx)))
		    (new-dom-freq (* new-peak-idx base-freq 1.0)))
	       (push new-dom-freq dominant-freq)
	       (push (when (> new-dom-freq (* 4 base-freq))
		       (harmonic-product-spectrum mags base-freq))
		     fundamental-freq)))
    (values (reverse centroid)
	    (reverse spread)
	    (reverse flatness)
	    (reverse dominant-freq)
	    (reverse fundamental-freq))))

;; *** analyse-soundfile
;;; analyse some basic parameters and write them into the sf-object
#+clm
(defpubmethod analyse-soundfile ((sf soundfile)
				 &key fft-size (srate 48000) (multiple-ffts t))
  (let* ((array (clm::table-from-file (path sf)))
	 (envelope (clm::envelope-follower array))
	 (max (max-of-array-with-index array t)))
    (multiple-value-bind (c s f d p)
	(spectral-analysis array
			   :fft-size fft-size
			   :srate srate
			   :multiple-ffts multiple-ffts)
      (setf (centroid sf) c
	    (spread sf) s
	    (flatness sf) f
	    (dominant-frequency sf) d
	    (fundamental-frequency sf) p))
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
		      peak smoothness transient)
	do (unless (or (not (funcall slot sf)) (numberp (funcall slot sf))
		       (equal (funcall slot sf) 'random))
	     (funcall error-fun "check-sanity: weird ~a for soundfile ~a: ~a"
		      slot (id sf) (funcall slot sf))))
  ;; These should be lists of numbers
  (loop for slot in '(centroid spread flatness dominant-frequency
		      fundamental-frequency)
	do (unless (and (listp (funcall slot sf))
			(every #'numberp (funcall slot sf)))
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
		  :fundamental-frequency ',(fundamental-frequency sf)
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

(export '(soundfile name path duration total-samples samplerate peak peak-index
	  centroid spread flatness dominant-frequency fundamental-frequency
	  smoothness transient)
	:layers-utils)

;;;; EOF soundfile.lsp
