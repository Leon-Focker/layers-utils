;; ** Midi
;;;; Collection of some useful midi input, output and transformation functions

(in-package :layers-utils)

;; *** lists-to-midi
;;; Generate a MIDI file from pitch, duration and start-time data in lists.
;;; This function will generate as many midi notes, as the number of elements
;;; of the longest argument-list. If for example you only supply one pitch value
;;; but three durations, three notes with the same pitch will be generated.
;;; Shorter lists are 'looped' with mod. Other than slippery chickens 
;;; #'event-list-to-midi-file, this skipps the generation of a sc::event, which
;;; might be favorable in some cases.
;;;
;;; Arguments:
;;; - pitch-list: A list of either sc-pitches or midi-key-numbers. Can also be a
;;;   list of chords, ie. lists of pitches.
;;; - durations-list: A list of durations, either in seconds (when tempo = 60 or
;;;   scale-durs-to-tempo = t) or beats.
;;; - start-time-list: An optional list of start-times, either in seconds (when
;;;   tempo = 60 or scale-durs-to-tempo = t) or beats. If nil, notes are placed
;;;   one after another.
;;; Keywords:
;;; - velocity-list: Optional number or list of velocity values. Default is 0.7.
;;; - channel-list: Optional integer or list of channels to which to write the
;;;   note to. Default is 0.
;;; - pitch-bend-list: Optional number or list of pitch bend values for each
;;;   note. Default is 0.
;;; - scale-durs-to-tempo: When true, Interpret durations in durations-list as
;;;   durations in seconds, when nil, they are interpreted in durations in
;;;   numbers of beats in a tempo. Default is nil.
;;; - tempo: Tempo encoded in the file. Default is 60.
;;; - file: Full path for the resulting MIDI file. Default is "midi-output.mid"
;;;   in the *layers-utils-home-dir*.
(defpublic lists-to-midi (pitch-list
			  duration-list
			  start-time-list
			  &key
			  (velocity-list 0.7)
			  (channel-list 0)
			  (pitch-bend-list 0)
			  (tempo 60)
			  scale-durs-and-starts-to-tempo
			  (file (format nil "~a~a" *layers-utils-home-dir*
					"midi-output.mid")))
  (setf channel-list (force-list channel-list))
  (setf velocity-list (force-list velocity-list))
  (setf pitch-bend-list (force-list pitch-bend-list))
  (check-all-types list pitch-list duration-list start-time-list)
  (unless (and pitch-list duration-list)
    (error
     "Lists-to-midi: Please provide at least one value for pitch and duration!"))
  (unless start-time-list
    (setf start-time-list (get-start-times duration-list)))
  (let* ((all-lists (list pitch-list duration-list start-time-list velocity-list
			  channel-list pitch-bend-list))
	 (max-len (apply #'max (mapcar #'length all-lists)))
	 (events '()))
    ;; maybe scale durations
    (when scale-durs-and-starts-to-tempo
      (flet ((scale-time (time)
	       (* time (/ tempo 60))))
	(setf duration-list (mapcar #'scale-time duration-list)
	      start-time-list (mapcar #'scale-time start-time-list))))
    ;; collect events
    (loop for i below max-len
	  for pitches = (force-list (nth-mod i pitch-list))
	  do (loop for pitch in pitches
		   do (push (list
			     (if (numberp pitch) pitch (note-to-midi pitch))
			     (nth-mod i pitch-bend-list)
			     (nth-mod i start-time-list)
			     (nth-mod i velocity-list)
			     (nth-mod i duration-list)
			     (nth-mod i channel-list))
			    events)))
    ;; sort them by start-time
    (setf events (sort events #'(lambda (x y) (< (third x) (third y)))))
    ;; create the MIDI file with cm
    (cm::events
     (cm::new cm::seq
       :name (gensym)
       :time 0.0
       :subobjects (loop for event in events
			 append (apply #'cm::output-midi-note event)))
     file
     :tempo tempo)))

;; *** midi-file-to-list
;;; slippery chickens midi-file-to-events seems to make things more complicated
;;; than need be, so this function just reads the file without making events
;;; !!! notice, that currently there is no tempo argument - so be careful,
;;; that the imported file is already in the right tempo
;;; Also with longer midi files there seems to be some minor timing errors that
;;; build up? Probably stems from the cm::import-events funktion
(defpublic midi-file-to-list (file &optional track)
  (unless (probe-file file)
    (error "midi-file-to-list: file not found: ~a" file))
  (let* ((cm-midi (cm::parse-midi-file file track))
	 (result
	   (remove nil
		   (loop for m in cm-midi
			 collect (typecase m
				   (cm::midi (list (cm::object-time m)
						   (cm::midi-keynum m)
						   (cm::midi-duration m)
						   (cm::midi-amplitude m)
						   (cm::midi-channel m)
						   (+ (cm::object-time m)
						      (cm::midi-duration m)))))))))
    ;; sort by start-time and keynum
    (sort result #'(lambda (a b)
                     (if (= (first a) (first b))
			 (< (second a) (second b))
			 (< (first a) (first b)))))))

(setf (symbol-function 'midi-to-list) #'midi-file-to-list)


;; *** find-notes-with-cursor
;;; return all notes (lists with (start key duration...),
;;; that play during the time of cursor.
(defpublic find-notes-with-cursor (cursor list-of-notes)
  (loop for note in list-of-notes
	when (<= (first note) cursor (+ (first note) (nth 2 note)))
	  collect note))

;; *** harmony-filter
;;; takes a midi file, which will then be filtered by a second midi file.
;;; you can define the probability of a note being changed and the detection
;;; range for notes, ie. how close a note in the filter file has to be,
;;; to consider changing the original note to it.
;;; probability can be an envelope of style '(0 1  [...]  1 1)
;;; as can be the detection range.
(defpublic harmony-filter (midi-file midi-filter probability detection-range
		       &key track1 track2
			 (file (format nil "~afiltered_file.mid"
				       (get-pathname-dir midi-file)))
			 (seed 1))
  (let* ((file1 (midi-file-to-list midi-file track1))
	 (file2 (midi-file-to-list midi-filter track2))
	 (probability-env (if (atom probability)
			      (list 0 probability 1 probability)
			      probability))
	 (detection-range-env (if (atom detection-range)
				  (list 0 detection-range 1 detection-range)
				  detection-range))
	 (ran-nb (make-random-number seed))
	 duration
	 (harmony '())
	 (result '())
	 (events '()))
    ;; make sure both files are sorted by start-time
    ;; then loop through each note1 in file1
    (setf duration (first (car (last file1))))
    (loop for note in file1
	  for start = (nth 0 note)
	  for keyname = (nth 1 note)
	  with last-start2 = 0
	  for prob = (interpolate (/ start duration) probability-env)
	  for detection = (interpolate (/ start duration) detection-range-env)
	  ;; collect all notes in file2 that areplayed at notes start time
	  do (loop for i from 0 and note2 in (subseq file2 last-start2)
		   for start2 = (first note2)
		   for end2 = (nth 5 note2)
		   while (< start2 start)
		   do
		      (when (> end2 start)
			(push note2 harmony))
		   finally (incf last-start2 i))
	     (setf harmony (remove start harmony
				   :test (lambda (x y) (>= x (nth 5 y)))))
	     ;; decide wheter note is changed, then collect
	     (push (if (and harmony
			    (< (get-next ran-nb) prob))
		       ;; select closest pitch from harmony
		       (let* ((harm (loop for i in harmony collect (nth 1 i)))
			      (intervals (mapcar (lambda (x) (abs (- x keyname)))
						 harm))
			      (min (apply #'min intervals))
			      (closest (nth 1 (nth (position min intervals)
						   harmony))))
			 ;; if closest is in range, change
			 (if (<= min detection)
			     (replace note (list closest) :start1 1)
			     note))
		       ;; use original pitch
		       note)
		   result))
    ;; sort for time (for cm), then write into midi
    (setf result
	  (sort result #'(lambda (x y) (< (first x) (first y))))
	  events
	  (loop for note in result appending
				   (cm::output-midi-note
				    (nth 1 note)
				    0
				    (nth 0 note)
				    (nth 3 note)
				    (nth 2 note)
				    (nth 4 note))))
    (cm::events
     (cm::new cm::seq :name (gensym) :time 0.0 :subobjects events)
     file
     :tempo 60)))

;; *** deconstruct-chords
;;; Takes a midi file as input, which should consist of chords or some kind of
;;; polyphonie. This function then deconstructs that file, using the
;;; rhythmic-pattern.
;;; file - midi file with chords to deconstruct
;;; rhythmic-pattern - list of durations als rhythms, ie. '(2 1 (2)). Durations
;;;  in parens are rests.
;;; longest-duration - if any duration in the final file would be longer that
;;;   this, it will be replaced with a nested version of the pattern.
;;;   (see #'nested-pattern)
;;; density-env - an envelope with x and y values from 0 to 1. 0 Meaning only
;;;   one note will be chose, 1 meaning, all notes will be chosen.
;;; stay-on-note - when true, try to use notes that appeared in the last chord.
;;; TODO: chosing the pitches is a bit trivial and might need a way to vary it.
(defpublic deconstruct-chords
    (midi-file rhythmic-pattern longest-duration density-env stay-on-note
     &key midi-track)
  (let* ((chords (midi-file-to-list midi-file midi-track))
	 first-note
	 last-note
	 (duration 1)
	 (new-notes '()))
    ;; sort and set first and last note
    (setf chords (sort chords #'(lambda (x y) (< (car x) (car y))))
	  first-note (first chords)
	  last-note (car (last chords))
	  duration (- (+ (car last-note) (nth 2 last-note)) (car first-note))
	  rhythmic-pattern (scale-pattern rhythmic-pattern duration)
	  rhythmic-pattern (nested-pattern rhythmic-pattern longest-duration))
    
    ;; divide chords into the rhythmic pattern and look which notes are available
    (flet ((pred (last)
	     "This sorts items to the front, if they are found in 'last' "
	     (lambda (x y) (or (find x last)
			  (and (not (find x last)) (not (find y last))))))
	   (density-aux (d len) (1+ (round (* (max (min d 1) 0) (1- len))))))
      (loop for rhythm in rhythmic-pattern
	    with time = (car first-note)
	    for duration = (if (listp rhythm) (car rhythm) rhythm)
	    for i from 0
	    for notes = (find-notes-with-cursor time chords)
	    for how-many-pitches = (density-aux (interpolate
						 (/ i (length rhythmic-pattern))
						 density-env)
						(length notes))
	    do (when (and notes (atom rhythm))
		 ;; sort notes for priorities:
		 (setf notes (sort notes (pred (third (first new-notes)))))
		 (when stay-on-note (setf notes (reverse notes)))
		 ;; collect notes
		 (push `(,time ,duration ,(loop repeat how-many-pitches
						collect (second (pop notes))))
		       new-notes))
	       (setf time (+ time duration))
	    finally (setf new-notes (reverse new-notes))))
    ;; write to midi fil
    (lists-to-midi (loop for i in new-notes collect (nth 2 i))
		   (loop for i in new-notes collect (second i))
		   (loop for i in new-notes collect (first i))
		   :file (format nil "~a~a~a"
				 (directory-namestring midi-file)
				 (pathname-name midi-file)
				 "-deconstructed.mid"))))

;; *** midi-file-to-env

;;; convert a midi file into a pair of x-y breakpoints. The start-time of the
;;; first note corresponds to the first x-value (0). The start-time of the last
;;; note corresponds to the last x-value (100).
;;; The midi keynum values correspond to the y-values. The lowest one will be 0,
;;; the highest 1.
(defpublic midi-file-to-env (file &optional track)
  (let ((lists (midi-file-to-list file track))
	first
	last
	lowest
	highest
	(x-values '())
	(y-values '())
	(result-x '()))
    ;; init all missing variables
    (loop for event in lists
	  for time = (first event)
	  for key = (second event)
	  minimize time into t-min
	  maximize time into t-max
	  minimize key into k-min
	  maximize key into k-max
	  collect time into x
	  collect key into y
	  finally (setf first t-min
			last t-max
			lowest k-min
			highest k-max
			x-values x
			y-values y))
    (loop for x in x-values and y in y-values
	  for x-already-found = (find x result-x)
	  do (push x result-x)
	  unless x-already-found collect (rescale x first last 0 100)
	    unless x-already-found collect (rescale y lowest highest 0 1))))

(setf (symbol-function 'midi-to-env) #'midi-file-to-env)
(export 'midi-to-env :layers-utils)

;; *** duration-layers-to-midi
(defpublic duration-layers-to-midi (duration-layers
				    &key n (dir *layers-utils-home-dir*))
  (let* ((rhythm-blocks (data duration-layers))
	 (len (length rhythm-blocks))
	 (pitches (if n
		      (loop repeat (length (nth n rhythm-blocks))
			    collect 60)
		      (loop for i below len 
			    append (loop repeat (length (nth i rhythm-blocks))
					 collect (- 60 i)))))
	 (start-times (if n
			  (get-start-times
			   (loop for i in (nth n rhythm-blocks)
				 collect (float i)))
			  (loop for i below len 
				append (get-start-times
					(loop for i in
						    (nth i rhythm-blocks)
					      collect (float i))))))
	 (durations (if n
			(nth n rhythm-blocks)
			(loop for i below len append (nth i rhythm-blocks)))))
    (lists-to-midi pitches durations start-times
		   :file (if n
			     (format nil "~a~a~a~a~a~a" dir "duration-layers-"
				     (id duration-layers) "-n-" n ".mid")
			     (format nil "~a~a~a~a" dir "duration-layers-"
				     (id duration-layers)
				     ".mid")))))

;; EOF midi.lsp
