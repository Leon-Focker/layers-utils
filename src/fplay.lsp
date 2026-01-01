;; ** fplay

(in-package :layers-utils)

#|
;; *** dynamic-collect
;;; give any amount of arguments to a collect in a loop in a macro :)
;;; naybe useless?
(defmacro dynamic-collect (&rest rest)
  `(loop for i in (quote ,rest) collect 'collect collect i))

(defmacro dynamic-collect-ls (rest)
  `(loop for i in ,rest collect 'collect collect i))
|#

;; *** name-var
;;; name a variable after the following scheme:
;;; i = 1 => name, i = 2 => name2, i = 3 => name3 ...
(defmacro name-var (name i)
  `(progn (unless (symbolp ,name)
	    (error "name must be a symbol in name-var: ~a" ,name))
	  (unless (numberp ,i)
	    (error "i must be an number in name-var: ~a" ,i))
	  (if (<= ,i 1) ,name (read-from-string (format nil "~a~a" ,name ,i)))))

;; *** name-var-highest
(defmacro name-var-highest (name i arg-list)
  `(let* ((ass (assoc ,name ,arg-list))
	  (len (1- (length ass)))
	  (n (min ,i len)))
     (name-var ,name n)))
  
;; *** get-loop-vars
;;; return a list with statements of type 'for var-name = var-def'
;;; arg-list - list of lists, these contain a name for a variable
;;;  as first element, then how those varable is defined in the loop
;;;  if there is more than one definition (eg. the sublist is longer
;;;  than 2 elements, (1- length) variables will be created. The first
;;;  variable is always called var-name, after that: var-name2, var-name3...
;;;  The definition can also be a string and will.
;;; There is one exception: when var-name = time, 'with is used, not 'for.
;;; EXAMPLE
#|
(get-loop-vars '((time 5) (duration) (rhythm (nth i ls)) (sound 2 "4 then 5")))
=> (WITH TIME = 5 FOR RHYTHM = (NTH I LS) FOR SOUND = 2 FOR SOUND2 = 4 THEN 5)
|#
(defmacro get-loop-vars (arg-list)
  `(progn
     (unless (listp ,arg-list)
       (error "arg-list in get-loop-vars must be a list: ~a" ,arg-list))
     (loop for el in
	  (loop for var in ,arg-list
	     for len = (length var)
	     for var-name = (first var)
	     for substring = (when (> (length (string var-name)) 3)
			       (subseq (string var-name) 0 4))
	     for flag = (or (equal "TIME" substring))
	     do (unless (symbolp var-name)
		  (error "invalid name for a variable: ~a" var-name))
	       (when (> len 10)
		 (warn "are you sure about ~a different instances of ~a?"
		       len var-name))
	     append (when (> len 1)
		      (loop for i from 1 for var-def in (cdr var)
			    collect (if flag 'with 'for)
			 collect (name-var var-name i)
			 collect '=
			 collect var-def)))
	append (if (stringp el) (string-to-list el) (list el)))))

;; *** merge-var-lists
;;; push elements from list 'from into list 'into, but only if no sublist with
;;; the same first element is already in 'into.
(defmacro merge-var-lists (from into)
  `(progn (unless (and (listp ,from) (listp ,into))
	    (error "from and into in merge-var-lists must be lists: ~a, ~a"
		   ,from ,into))
	  (loop for el in ,from
		;; do this to intern all symbols
		for key = (intern (string (first el)))
		for ls = (cons key (cdr el))
		unless (assoc key ,into) do (push ls ,into))))

;; *** fplay-get-loop-vars
;;; this is where the fplay magic happens:
;;; collects all variable names and definitions that are needed for fplay
;;; and then generates them using #'get-loop-vars. There's probably still a few
;;; bugs that I haven't discovered yet.
;;; no pretty error message if sound is nil and file doesnt get anything useful
(defmacro fplay-get-loop-vars (start-time end-time arg-list)
  `(progn
     (loop for i in ,arg-list unless (listp i) do
       (error "fplay: argument ~a is malformed! Should be a list starting ~
               with a symbol, followed by definitions for that symbol, i.e. ~
               (rhythm 0.5)" i))
     (let* ((max-len (if ,arg-list
			 (1- (apply #'max (mapcar #'length ,arg-list)))
			 1))
	    ;; to render only some voices, enable these and replace max-len 
	    ;(from (or (second (assoc 'from ,arg-list)) 1))
	    ;(to (or (second (assoc 'to ,arg-list)) max-len))
	    (rthm (assoc (intern (string 'rhythm)) ,arg-list))
	    (tim (assoc (intern (string 'time)) ,arg-list))
	    (con (assoc (intern (string 'condition)) ,arg-list))
	    (snd (assoc (intern (string 'sound)) ,arg-list))
	    (sndp (assoc (intern (string 'soundpile)) ,arg-list))
	    (print (assoc (intern (string 'printing)) ,arg-list))
	    (total-times (max 2 (length rthm) (length tim)))
	    (all-vars '()))
       ;; add as many 'time, 'condition, 'line, 'soundpile and 'sound variables
       ;; as needed
       (merge-var-lists
	(list (loop for i from 0 below total-times
		    collect (if (= 0 i)
				(intern (string 'time))
				(if tim
				    (nth (1- (min i (1- (length tim)))) (cdr tim))
				    ,start-time)))
	      (loop for i from 0 below total-times
		    collect (if (= 0 i)
				(intern (string 'condition))
				(if con
				    (nth (1- (min i (1- (length con))))
					 (cdr con))
				    `(<= ,(name-var 'time i)
					 ,,end-time))))
	      (loop for i from 0 below total-times
		    collect (if (= 0 i)
				(intern (string 'line))
				`(/ (- ,(name-var 'time i) ,,start-time)
				    (- ,,end-time ,,start-time))))
	      (loop for i from 0 below (max 2 (length sndp))
		    collect (if (= 0 i)
				(intern (string 'soundpile))
				(if sndp
				    (nth (1- (min i (1- (length sndp))))
					 (cdr sndp))
				    (make-soundpile
				     'fplay-init
				     (list (make-soundfile '220 "tests/220.wav")
					   (make-soundfile '440 "tests/440.wav"))))))
	      (loop for i from 0 below (max 2 (length sndp))
		    collect (if (= 0 i)
				(intern (string 'sound))
				(if snd
				    (nth (1- (min i (1- (length snd))))
					 (cdr snd))
				    `(first (data ,(intern (string 'soundpile))))))))
	all-vars)
       ;; get user-defined variables in the mix
       (merge-var-lists ,arg-list all-vars)
       ;; starting with some stubs for error messages, not sure yet if this is
       ;; usefull, since more often than not it will first run into an error and
       ;; then eval this.
       #+nil(merge-var-lists
	(list (loop for i from 0 below (max 2 (length snd)) collect
		   (if (= 0 i) 'errors `(unless ,(name-var 'sound i)
					  (error "~a is nil at i = ~a"
						 ',(name-var 'sound i)
						 i)))))
	all-vars)
       ;; add sufficient 'file variables
       (merge-var-lists
	(list (loop for i from 0 below (max 2 (length snd)) collect
		   (if (= 0 i) (intern (string 'file)) `(path ,(name-var (intern (string 'sound)) i)))))
	all-vars)
       ;; add all other variables that are not set through arg-list
       (merge-var-lists
	`((rhythm (1+ ,,end-time))
	  (duration nil)
	  (reflect nil)
	  (reverse nil)
	  (start 0)
	  (end 0)
	  (srt 1)
	  (width 5)
	  (srt-env '(0 0 100 0))
	  (srt-scaler 1.0)
	  (amp 1.0)
	  (amp-env '(0 1 100 1))
	  (degree 45)
	  (distance 0)
	  (rev-env '(0 1 100 1))
	  (rev-amt 0)
	  (channel 0)
	  (out-channels nil)
	  (printing nil))
	all-vars)
       ;; get all neccessary variables:
       (append
	(get-loop-vars (reverse all-vars))
	;; while:
	(append '(while)
		(list (loop for i from 0 below (max 2 (length rthm)) collect
			   (if (= 0 i) 'or (name-var 'condition i)))))
	;; error cases:
	`(do ,@(loop for i in `(,(intern (string 'rhythm)) ,(intern (string 'file))) collect
		    `(unless ,i
		       (error "~&~a returned with nil in fplay" ',i)))
	     ,@(loop for i from 1 to max-len collect
		    `(when (and ,(name-var-highest (intern (string 'duration)) i all-vars)
				(<= ,(name-var-highest (intern (string 'duration)) i all-vars) 0.0001))
		       (setf ,(name-var-highest 'condition i all-vars) nil))))
	;; instrument calls:
	;; (loop for i from 1 to max-len append
	;;      (list 'when (name-var-highest 'condition i all-vars) 'collect
	;; 	   `(funcall (lambda ()
	;;         (samp0 ,(name-var-highest (intern (string 'file)) i all-vars)
	;; 	       ,(name-var-highest (intern (string 'time)) i all-vars)
	;; 	       :duration ,(name-var-highest 'duration i all-vars)
	;; 	       :reflect ,(name-var-highest 'reflect i all-vars)
	;; 	       :reverse ,(name-var-highest 'reverse i all-vars)
	;; 	       :start ,(name-var-highest 'start i all-vars)
	;; 	       :end ,(name-var-highest 'end i all-vars)
	;; 	       :srt ,(name-var-highest 'srt i all-vars)
	;; 	       :width ,(name-var-highest 'width i all-vars)
	;; 	       :srt-env ,(name-var-highest 'srt-env i all-vars)
	;; 	       :srt-scaler ,(name-var-highest 'srt-scaler i all-vars)
	;; 	       :amp ,(name-var-highest 'amp i all-vars)
	;; 	       :amp-env ,(name-var-highest 'amp-env i all-vars)
	;; 	       :degree ,(name-var-highest 'degree i all-vars)
	;; 	       :distance ,(name-var-highest 'distance i all-vars)
	;; 	       :rev-env ,(name-var-highest 'rev-env i all-vars)
	;; 	       :rev-amt ,(name-var-highest 'rev-amt i all-vars)
	;; 	       :channel ,(name-var-highest 'channel i all-vars)
	;; 	       :out-channels ,(name-var-highest 'out-channels i all-vars)
	;; 	       :printing nil
	;; 	       )))))

	(loop for i from 1 to max-len append
	     (list 'when (name-var-highest 'condition i all-vars) 'collect
		   `(funcall (lambda ()
	        (samp0 ,(name-var-highest (intern (string 'file)) i all-vars)
		       ,(name-var-highest (intern (string 'time)) i all-vars)
		       :duration ,(name-var-highest (intern (string 'duration)) i all-vars)
		       :reflect ,(name-var-highest (intern (string 'reflect)) i all-vars)
		       :reverse ,(name-var-highest (intern (string 'reverse)) i all-vars)
		       :start ,(name-var-highest (intern (string 'start)) i all-vars)
		       :end ,(name-var-highest (intern (string 'end)) i all-vars)
		       :srt ,(name-var-highest (intern (string 'srt)) i all-vars)
		       :width ,(name-var-highest (intern (string 'width)) i all-vars)
		       :srt-env ,(name-var-highest (intern (string 'srt-env)) i all-vars)
		       :srt-scaler ,(name-var-highest (intern (string 'srt-scaler)) i all-vars)
		       :amp ,(name-var-highest (intern (string 'amp)) i all-vars)
		       :amp-env ,(name-var-highest (intern (string 'amp-env)) i all-vars)
		       :degree ,(name-var-highest (intern (string 'degree)) i all-vars)
		       :distance ,(name-var-highest (intern (string 'distance)) i all-vars)
		       :rev-env ,(name-var-highest (intern (string 'rev-env)) i all-vars)
		       :rev-amt ,(name-var-highest (intern (string 'rev-amt)) i all-vars)
		       :channel ,(name-var-highest (intern (string 'channel)) i all-vars)
		       :out-channels ,(name-var-highest (intern (string 'out-channels)) i all-vars)
		       :printing nil
		       )))))
	;; printing:
	`(do 
	  ,@(loop for i from 1 to max-len 
	       when (nth (max 0 (1- (min i (1- (length print))))) (cdr print))
	       collect
		 `(when ,(name-var-highest (intern (string 'condition)) i all-vars)
		    (format t "~&time: ~a"
			    ,(name-var-highest 'time i all-vars))
		    (when (equal (type-of ,(name-var-highest (intern (string 'sound)) i all-vars))
				 'stored-file)
			   (format t "~&sound: ~a"
				   (id ,(name-var-highest (intern (string 'sound)) i
							      all-vars))))
		    (format t "~&duration: ~a"
			    ,(name-var-highest 'duration i all-vars))
		    (format t "~&srate-conversion: ~a"
			    ,(name-var-highest 'srt i all-vars))
		    (format t "~&reverse: ~a"
			    ,(name-var-highest 'reverse i all-vars))
		    (format t "~&degree: ~a"
			    ,(name-var-highest 'degree i all-vars))))
	  ;; increasing the different times:
	  ,@(loop for i from 1 to (1- (length (assoc 'time all-vars)))
	       collect `(incf ,(name-var 'time i)
			      ,(name-var-highest (intern (string 'rhythm)) i all-vars))))))))

;; *** fplay
;;; This is a somewhat stupid but very handy Macro for calling the samp0
;;; instrument. It introduces its own syntax, but once you understand it, this
;;; makes it very comfortable to abstract complex calls to a sampler.
;;; Under the hood it starts a loop that calls samp0. In fplays body you define
;;; variables that influence the loop and the calls to the sampler. Some
;;; variable names are keywords, that do something specific. Other names can
;;; just be used like you would variables in a loop.
;;;
;;; Arguments:
;;; - start-time: A time in seconds. This will be the initial value for the
;;;   'time variable (see below).
;;; - end-time: A time in seconds, no sample will be played later than this,
;;;   as the loop stops when 'time is > end-time.
;;; - body: Within the body you can define variable bindings like you would in a
;;;   #'let. But there is a few key differences: The body contains ONLY these
;;;   bindings, some symbols have a special meaning and you can provide more
;;;   than one init-form per symbol. For example (fplay 0 10 (foo 1 2))
;;;   expands to something like this (simplified):
;;;
;;;   (loop for i from 0
;;;         with time = 0
;;;         for condition = (<= time 10)
;;;         for foo = 1
;;;         for foo2 = 2
;;;         ... calls to samp0)
;;;
;;;   #'fplay will automatically generate as many voices (independant calls to
;;;   samp0) as there is init-forms per variable. In this example there is a
;;;   second voice because of foo, but if other variables only have one
;;;   init-form that will be used for all other voices.
;;;
;;;   Some variable-names function as keywords, which have special meaning. Some
;;;   of them also have a default definition:
;;;   - time: the current time in seconds. This is given to samp0 as time
;;;     argument and will be increased by the corresponding 'rhythm variable at
;;;     the end of each iteration. Initially defined to start at start-time.
;;;   - i: counts up from 0 with each iteration, cannot be changed.
;;;   - line: defined as (/ passed-time (- end-time start-time)), not changeable
;;;   - condition: loop stops when this is no longer true, initially defined as
;;;     (<= time end-time)
;;;   - rhythm: the time between this call to samp0 and the next one. 'time will
;;;     be increased by 'rhythm at the end of each iteration. However this is 
;;;     not neccessarily the duration of the sample. If you don't specify a 
;;;     rhythm value, the sample should only play once.
;;;   - duration: how long the sample will be played. This is passed to samp0 as
;;;     the :duration key-argument. Initially defined as nil - the sample will
;;;     be played entirely.
;;;   - file: this is given to samp0 as its file argument and should be a path
;;;     pointing to a soundfile. Its initial definition is (path sound). The
;;;     'sound and 'soundpile variables can be ignored when just providing 'file
;;;     with a path.
;;;   - sound: A soundfile-object. Initially defined as (first (data soundpile))
;;;   - soundpile: A soundpile-object. Initially defined with a 220Hz and 440Hz
;;;     sine sample.
;;;   - printing: when t, print sound, duration and time for instrument call.
;;;   - out-channels: how many channels the soundfile will have. If not 
;;;     specified this will be 2. For more than two channels locsig assumes them
;;;     to be arranged in an equally spaced circle.
;;;   - reflect, reverse, start, end, srt, srt-env, srt-scaler, amp, amp-env,
;;;     degree, distance, rev-env, rev-amt, channel:
;;;     These are all keys to samp0 which can be used as keywords in #'fplay.
;;;
;;;   The init-form of a variable can also be a function, referencing one of the
;;;   other variables. You can always use 'time, 'i, 'line, 'condition, 'sound and
;;;   'soundpile here. Every other variable needs to be defined before using it.
;;;   Thus, this is valid:
;;;
;;;   (fplay 0 10
;;;     (rhythm (nth (mod i 2) '(3 4)))
;;;     (duration (/ rhythm 2)))
;;;
;;; Examples:
#|
(fplay-to-wav (layers-utils-path "test.wav")
  (fplay 0 10))

(fplay-to-wav (layers-utils-path "test.wav")
  (fplay 0 5
    (file (layers-utils-path "tests/1000.wav"))
    (rhythm "1 then .5")
    (duration (/ rhythm 4))
    (amp-env `(0 1  1 ,(- 1 (expt line 0.5))))
    (srt 1 1.1)))
|#
;;; For more exemplary uses see https://github.com/Leon-Focker/feedback
(defmacro fplay (start-time end-time &body body)
  ;;(if (not body)
  ;;(warn "fplay: body is empty! Doing nothing.")
  `(progn (unless (and (numberp ,start-time) (numberp ,end-time))
	    (error "fplay: start-time and end-time must be numbers: ~a, ~a"
		   ,start-time ,end-time))
	  (loop for ,(intern (string 'i)) from 0 ,@(fplay-get-loop-vars start-time end-time body))))

;; *** fplay-to-wav
;;; A withsound macro that automatically sets the number of channels for the
;;; output file.
(defmacro fplay-to-wav (file &body body)
  (let* ((body-string (write-to-string body))
	 (index1 (search "OUT-CHANNELS" body-string))
	 (index2 (when index1 (search ")" body-string :start2 index1)))
	 (out-channels 2))
    (when (and index1 index2)
      (setf out-channels
	    (parse-integer (subseq body-string (+ index1 13) index2)))
      (format t "~&set number of channels for ~a to ~a" (pathname-name file)
	      out-channels))
    `(with-sound (:header-type clm::mus-riff :sampling-rate 48000
		  :output ,file
		  :channels ,out-channels :play nil :scaled-to 0.98
		  :force-recomputation nil)
       ,@body
       (format t "~&"))))

(export '(fplay fplay-to-wav) :layers-utils)

;; EOF fplay.lsp
