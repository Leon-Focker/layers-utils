;; ** basic-instruments
;;; some usefull clm instruments

(in-package :clm)

;; *** simple-fine
(definstrument simple-sine (start dur freq amp &optional phase)
  (let* ((st (floor (* start *srate*)))
	 (nd (+ st (floor (* *srate* dur))))
	 (osc (make-oscil :frequency freq :initial-phase (if phase phase 0)))
	 #|(loc (make-locsig))|#)
    (Run
     (loop for i from st to nd do
	   (outa i (* amp (oscil osc)))))))

;; *** simple-src
(definstrument simple-src (file start srt 
				&key
				duration
				(pan 45)
				(amp 1)
				(sample-start 0)
				(width *clm-src-width*)
				(envelope '(0 1  100 1))
				(srt-env '(0 0  100 0))
				(fmfreq 0)
				(fmamp 0)
				fmfile
				(fmfile-start 0)
				(fmamp-file 0)
				(env-base 1))
  (let ((f1 (open-input* file))
	(f2 (when fmfile (open-input* fmfile))))
    (unwind-protect
	(let* ((s-start (* sample-start *srate*))
	       (dur (if duration duration (/ (sound-duration file) srt)))
	       (senv (make-env srt-env :duration dur :base env-base))
	       (aenv (make-env envelope :duration dur))
	       (beg (floor (* start *srate*)))
	       (end (+ beg (floor (* dur *srate*))))
	       (os (make-oscil :frequency fmfreq))
	       (fm-file (when fmfile (make-readin f2 :start fmfile-start)))
	       (rd (make-readin f1 :start s-start))
	       (src-gen (make-src :srate srt :width width))
	       (loc (make-locsig :degree pan :distance 1)))
	  (run
	   (loop for i from beg below end do
		 (locsig loc i (* amp
				  (env aenv)
				  (src src-gen
				       (+ (env senv)
					  (* fmamp (oscil os))
					  (if fmfile
					      (* fmamp-file (readin fm-file))
					      0))
				       #'(lambda (dir) (readin rd))))))))
      (close-input f1)
      (close-input f2))))

;; *** read-from-audio
(defparameter *read-from-audio-buffer* '())
(definstrument read-from-audio (file &key (start 0) ; in samples
				     (end 0)
				     length
				     (list t)
				     (step 1)
				     (channel 0)
				     (scaler 1))
  (let ((f (open-input* file :channel channel)))
    (unwind-protect 
	(let* ((st start)
	       (len (file-frample-count file))
	       (nd (let* ((n (if length
				 (+ start length)
			       end)))
		     (if (or (<= n 0) (>= n len))
			 len
		       n)))
	       ;; (warn "Requested duration longer than available ~
               ;;             duration (using latter): ~a ~a"
	       ;;			     (* end *srate*) len)
	       (dur (floor (- nd st) step))
	       (readin0 (make-readin f :start st)))
	  (setf *read-from-audio-buffer*
		(make-double-array dur :initial-element 0.0))
	  (run*
	   (*read-from-audio-buffer*)
	   (loop for i from st to nd do
		 (if (= (mod i step) 0)
		     (setf (aref *read-from-audio-buffer* (floor (- i st) step))
			   (* (readin readin0) scaler))
		   (readin readin0)))))
      (close-input f))
    (if list
	(loop for i below (length *read-from-audio-buffer*) collect
	      (aref *read-from-audio-buffer* i))
      *read-from-audio-buffer*)))

;; *** array-into-audio
(definstrument array-into-audio (array)
  (let ((nd (length array)))
    (run
     (loop for i below nd do
	   (outa i (aref array i))))))

;; *** snippet-from-file
(definstrument snippet-from-file (file &optional beg dur
				       &key
				       (start 0)
				       (env '(0 0  1 1  99 1  100 0))
				       (pan 45)
				       (amp 0.5)
				       (percent? nil))
  ;;beg should be between 0 and 1 if percent? is true
  (let ((len (file-frample-count file))
	(f (open-input* file)))
    (unwind-protect
	(let* ((dur (if dur dur (random (mus-sound-duration file))))
	       (st (if beg
		       (if percent?
			   (* beg (- len (* dur *srate*)))
			 (* beg *srate*))
		     (* (random 1.0) (- len (* dur *srate*)))))
	       (nd (if (and dur (not (= 0 dur)))
		       (let* ((n (+ st (* dur *srate*))))
			 (if (>= n len) (- len 1) n))
		     (+ st
			(* (random 1.0)
			   (- len st)))))
	       (dr (- nd st))
	       (readin0 (make-readin f :start st))
	       (amp-env (make-env env
				  :duration (/ dr *srate*)))
	       (loc (make-locsig :degree pan :distance 1 )))
	  (run
	   (loop for i below dr do
		 (locsig loc
			 (+ i (floor (* *srate* start)))
			 (* (readin readin0)
			    amp
			    (env amp-env)
			    )))))
      (close-input f))))

;; *** snippet-from-file-out-any
(definstrument snippet-from-file-out-any (file &optional beg dur
					       &key
					       (start 0)
					       (env '(0 0  1 1  99 1  100 0))
					       (channel 0)
					       (amp 0.5)
					       (percent? nil))
  ;;beg should be between 0 and 1 if percent? is true
  (let ((len (file-frample-count file))
	(f (open-input* file)))
    (unwind-protect
	(let* ((st (if beg
		       (if percent?
			   (* beg (- len (* dur *srate*)))
			 (* beg *srate*))
		     (* (random 1.0) (- len (* dur *srate*)))))
	       (nd (if (and dur (not (= 0 dur)))
		       (let* ((n (+ st (* dur *srate*))))
			 (if (>= n len) (- len 1) n))
		     (+ st
			(* (random 1.0)
			   (- len st)))))
	       (dr (- nd st))
	       (readin0 (make-readin f :start st))
	       (amp-env (make-env :envelope env
				  :scaler 1
				  :duration dr)))
	  (run
	   (loop for i below dr do
		 (out-any (+ i (floor (* *srate* start))) (* amp (readin readin0)) channel))))
      (close-input f))))

;; *** simple-ring-mod
(definstrument simple-ring-mod (file1
				&key
				(file2 file1)
				(beg 0)
				dur
				(start1 0)
				(start2 0)
				(mod-amt 1)
				(env '(0 0  1 1  99 1  100 0))
				(pan 45)
				(amp 0.5))
  (let ((f1 (open-input* file1))
	(f2 (open-input* file2)))
    (unwind-protect
	(let* ((dur (if dur
			dur
		      (min (mus-sound-duration file1)
			   (mus-sound-duration file2))))
	       (st (floor (* beg *srate*)))
	       (nd (+ st (floor (* dur *srate*))))
	       (readin1 (make-readin f1 :start start1))
	       (readin2 (make-readin f2 :start start2))
	       (amp-env (make-env env :duration dur))
	       (mod-env (make-env (if (listp mod-amt)
				      mod-amt
				    (list 0 mod-amt  100 mod-amt))
				  :duration dur))
	       (loc (make-locsig :degree pan :distance 1)))
	  (run
	   (loop for i from st to nd do
		 (locsig loc
			 i
			 (* (let ((mod-a (env mod-env))
				  (rd1 (readin readin1))
				  (rd2 (readin readin2)))
			      (+ (* (- 1 mod-a)
				    rd1)
				 (* mod-a
				    rd1
				    rd2)))
			    amp
			    (env amp-env))))))
      (close-input f1)
      (close-input f2))))

;; *** cross-synthesis
(definstrument cross-synthesis (beg dur file1 file2 amp &optional (fftsize 128) (r two-pi) (lo 2) (hi nil))
  ;; file1: input sound, file2: gives spectral shape
  ;; r: controls width of formants (1.0 is another good value here)
  ;; lo and hi: which of the formants are active (a sort of filter on top of the filter)
  ;; we use the on-going spectrum of file2 to scale the outputs of the formant array
  (let* ((fil1 (open-input* file1))
	 (fil2 (and fil1 (open-input* file2))))
    (when fil1
      (if (not fil2)
          (close-input fil1)
        (unwind-protect
	    (let* ((start (floor (* beg *srate*)))
	           (end (+ start (floor (* dur *srate*))))
	           (freq-inc (floor fftsize 2))
	           (fdr (make-double-float-array fftsize))
	           (fdi (make-double-float-array fftsize))
	           (diffs (make-double-float-array freq-inc))
	           (spectrum (make-double-float-array freq-inc))
	           (filptr 0)
	           (ctr freq-inc)
	           (radius (- 1.0 (/ r fftsize)))
	           (bin (float (/ *srate* fftsize)))
	           (fs (make-array freq-inc)))
	      (if (null hi) (setf hi freq-inc))
	      (loop for k from lo below hi do 
		    (setf (aref fs k) (make-formant (* k bin) radius)))
	      (run
	       (loop for i from start below end do
		     (when (= ctr freq-inc)
	               (dotimes (k fftsize)
			 (setf (aref fdr k) (ina filptr fil2))
			 (incf filptr))
	               (clear-array fdi)
	               (decf filptr freq-inc)
	               (fft fdr fdi fftsize 1)
	               (rectangular->magnitudes fdr fdi)
	               (dotimes (k freq-inc) 
			 (setf (aref diffs k) 
			       (/ (- (aref fdr k) (aref spectrum k)) freq-inc)))
	               (setf ctr 0))
		     (incf ctr)
		     (dotimes (k freq-inc) 
                       (incf (aref spectrum k) (aref diffs k)))
		     (let ((outval 0.0)
			   (inval (ina i fil1)))
	               (loop for k from lo below hi do 
			     (incf outval (* (aref spectrum k) (formant (aref fs k) inval))))
	               (outa i (* amp outval))))))
          (progn
	    (close-input fil1)
	    (close-input fil2)))))))

;; EOF basic-instruments.lsp

