;; * slippery-utils
;;; Utilities copied from Michael Edwards' Slippery Chicken (GPL3)
;;; Layers-util depends on Slippery Chicken only because of a few functions. So
;;; I decided to copy those rathner than add SC as a dependency. Some of the
;;; following functions were written by me. If no author is specified, it was
;;; likely written by Michael.
;;;
;;; Code copied on July 28th 2025

(in-package :layers-utils)

;; ** utils

;; *** read-from-file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/read-from-file
;;; DESCRIPTION
;;; Read a Lisp expression from a file. This is determined by the Lisp
;;; parenthetical syntax.
;;; 
;;; ARGUMENTS
;;; - A string that is a file name including directory path and extension. 
;;; 
;;; RETURN VALUE
;;; The Lisp expression contained in the file.
;;; 
;;; EXAMPLE
#|
(read-from-file "/path/to/lisp-lorem-ipsum.txt")

=> (LOREM IPSUM DOLOR SIT AMET CONSECTETUR ADIPISCING ELIT CRAS CONSEQUAT
    CONVALLIS JUSTO VITAE CONSECTETUR MAURIS IN NIBH VEL EST TEMPUS LOBORTIS
    SUSPENDISSE POTENTI SED MAURIS MASSA ADIPISCING VITAE DIGNISSIM CONDIMENTUM
    VOLUTPAT VEL FELIS FUSCE AUGUE DUI PULVINAR ULTRICIES IMPERDIET SED
    PHARETRA EU QUAM INTEGER IN VULPUTATE VELIT ALIQUAM ERAT VOLUTPAT VIVAMUS
    SIT AMET ORCI EGET EROS CONSEQUAT TINCIDUNT NUNC ELEMENTUM ADIPISCING
    LOBORTIS MORBI AT LOREM EST EGET MATTIS ERAT DONEC AC RISUS A DUI MALESUADA
    LOBORTIS AC AT EST INTEGER AT INTERDUM TORTOR VIVAMUS HENDRERIT CONSEQUAT
    AUGUE QUISQUE ALIQUAM TELLUS NEC VESTIBULUM LOBORTIS RISUS TURPIS LUCTUS
    LIGULA IN BIBENDUM FELIS SEM PULVINAR DOLOR VIVAMUS RHONCUS NISI GRAVIDA
    PORTA VULPUTATE IPSUM LACUS PORTA RISUS A VULPUTATE MAGNA JUSTO A EST)

|#
;;; SYNOPSIS
(defun read-from-file (file)
;;; ****
  (with-open-file
      (stream file :direction :input :if-does-not-exist :error)
    (read stream)))

(defun write-to-file (file data)
;;; ****
  (with-open-file
      (stream file :direction :output :if-exists :supersede
              :if-does-not-exist :create)
    (print data stream)))

;; *** force-list

(defun force-list (thing)
  ;; MDE Thu Oct 1 16:47:36 2015 -- added (when thing ...) so that we don't
  ;; force NIL into a list
  (when thing
    (if (listp thing)
        thing
        (list thing))))

;; *** simple-listp

;;; Returns whether all the elements in the list are atoms (i.e. no
;;; sublists). NB passing NIL will result in T 
(defun simple-listp (list)
  (when (listp list)
    (loop for i in list unless (atom i) do (return nil) finally (return t))))

;; *** string-to-list

;;; DJR Thu 6 Feb 2020 17:41:19 GMT
;;; Taken from Stackoverflow, many thanks Barmar
;;; stackoverflow.com/questions/42867749/reading-lisp-objects-from-a-string 
(defun string-to-list (string)
  (with-input-from-string (stream string)
    (loop with eof-marker = '#:eof
       for object = (read stream nil eof-marker)
       until (eq object eof-marker)
       collect object)))

;; *** flatten

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/flatten
;;; DESCRIPTION
;;; Return a list of nested lists of any depth as a flat list.
;;; 
;;; ARGUMENTS
;;; - A list of nested lists.
;;; 
;;; RETURN VALUE
;;; A flat list.
;;; 
;;; EXAMPLE
#|
(flatten '((1 (2 3 4) (5 (6 7) (8 9 10 (11) 12)) 13) 14 15 (16 17)))

=> (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)

|#
;;; SYNOPSIS
(defun flatten (nested-list)
;;; ****
  (cond ((null nested-list) nil)
        ((atom nested-list) (list nested-list))
        (t (append (flatten (first nested-list))
                   (flatten (rest nested-list))))))

;; *** almost-flatten

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/almost-flatten
;;; DATE
;;; September 4th 2013
;;; 
;;; DESCRIPTION
;;; Similar to flatten but allows one level of nesting
;;; 
;;; ARGUMENTS
;;; A list with an arbitrary level of nesting.
;;; 
;;; 
;;; RETURN VALUE
;;; A list with a maximum of one level of nesting
;;; 
;;; EXAMPLE
#|

(almost-flatten '((1 (2 3 4) (5 (6 7) (8 9 10 (11) 12)) 13) 14 15 (16 17)))
=> (1 (2 3 4) 5 (6 7) 8 9 10 (11) (12) (13) 14 15 (16 17))

|#
;;; SYNOPSIS
(defun almost-flatten (nested-list)
;;; ****
;;; ****
  (cond ((null nested-list) nil)
        ((or (simple-listp nested-list) (atom nested-list)) (list nested-list))
        (t (append (almost-flatten (first nested-list))
                   (almost-flatten (rest nested-list))))))

;; *** rescale

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/rescale
;;; DATE
;;; June 8th 2016, Edinburgh
;;; 
;;; DESCRIPTION
;;; Given a value within an original range, return its value withing a new range
;;; 
;;; ARGUMENTS
;;; - the value we want to rescale
;;; - the original minimum
;;; - the original maximum
;;; - the new minimum
;;; - the new maximum
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; :out-of-range. The function to call when the first argument is not within
;;; the range of arguments two and three. This would normally be #'error,
;;; #'warn or NIL. If #'warn or NIL, argument 1 will be hard-limited to the
;;; original range. Default = #'error
;;; :type-of-result. Usually this function uses float precision, but by setting
;;; type-of-result to #'double-float or #'rationalize, it is more precise.
;;; 
;;; RETURN VALUE
;;; The value within the new range (a number)
;;; 
;;; EXAMPLE
#|
(rescale .5 0 1 0 100)
==> 50.0
|#
;;; SYNOPSIS
(defun rescale (val min max new-min new-max &optional (out-of-range #'error)
                                              (type-of-result #'float))
;;; ****
  (flet ((oor () ; in case we need to call it on more than one occasion...
           (when (functionp out-of-range)
             (funcall out-of-range
                      "utilities::rescale: first argument (~a) should be ~
                       within the ~%original range (~a to ~a)" val min max))))
    (when (or (>= min max)
              (>= new-min new-max))
      (error "utilities::rescale: argument 2 (~a) must be < argument 3 (~a) ~
              ~%and sim. for argument 4 (~a) and 5 (~a)"
             min max new-min new-max))
    (unless (and (>= val min)
                 (<= val max))
      (oor)
      (setf val (if (> val max) max min)))
    (let* ((range1 (funcall type-of-result (- max min)))
           (range2 (funcall type-of-result (- new-max new-min)))
           (prop (funcall type-of-result (/ (- val min) range1))))
      (+ new-min (* prop range2)))))

;; *** decider
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/decider
;;; AUTHOR
;;; Leon Focker: leon@leonfocker.de
;;;
;;; DATE
;;; February 23rd 2023
;;;
;;; DESCRIPTION
;;; Return an index, which can be used to select an element from a sequence,
;;; when provided with a list of weights (see the example). It does that by
;;; scaling the selector argument relative to the sum of all weights, using
;;; rescale. Then it goes through all the weights and as soon as the selector
;;; is smaller than the sum of the weights so far, the index of the current
;;; weight is returned. So when given a list of weights '(2 1), the following
;;; selectors will return:
;;; 0     => 0
;;; 1/3   => 0
;;; 19/30 => 0
;;; 2/3   => 1
;;; 1     => 1
;;; This process is thus deterministic. By providing a random number as a
;;; selector, you can make random choices etc.
;;; 
;;; ARGUMENTS
;;; - A number between 0 and 1
;;; - A list of numbers, representing weights 
;;; 
;;; RETURN VALUE
;;; index of chosen element
;;;
;;; EXAMPLE
#|
;;; simple example, choosing from a list:
(let* ((ls '(c4 d4 e4 f4 g4 a4 b4))
       (weights '(1 1 2 2 3 1 2)))
  (nth (decider 0.1 weights) ls))
=> d4

;;; making a simple melody, following a sine wave:
(let* ((ls '(c4 d4 e4 f4 g4 a4 b4))
       (weights '(1 1 2 2 3 1 2)))
  (loop for i from 0 to pi by 0.25 collect
       (nth (decider (abs (sin i)) weights) ls)))
=> (C4 E4 F4 G4 B4 B4 B4 B4 B4 A4 G4 F4 D4)

;;; make a random melody with 10 pitches:
(let* ((ls '(c4 d4 e4 f4 g4 a4 b4))
       (weights '(1 1 2 2 3 1 2)))
  (loop repeat 10 collect
       (nth (decider (random 1.0) weights) ls)))
|#
;;; SYNOPSIS
(defun decider (selector weights)
;;; ****  
  (labels ((helper (selector ls1 index sum)
             (cond ((null ls1) (1- (length weights)))
                   ((< selector sum) index)
                   (t (helper selector
                              (cdr ls1)
                              (+ index 1)
                              (+ sum (rationalize (car ls1))))))))
    (helper (rescale (rationalize selector) 0 1 0 (loop for i in weights sum
                                                       (rationalize i)))
            (cdr weights) 0 (rationalize (car weights)))))

;; *** equal-within-tolerance

;;; ****f* utilities/equal-within-tolerance
;;; DESCRIPTION
;;; Test whether the difference between two decimal numbers falls within a
;;; specified tolerance.
;;;
;;; This test is designed to compensate for calculation discrepancies caused by
;;; floating-point errors (such as 2.0 vs. 1.9999997), in which the equations
;;; should yield equal numbers. It is intended to be used in place of = in such
;;; circumstances.
;;; 
;;; ARGUMENTS
;;; - A first number.
;;; - A second number.
;;;
;;; OPTIONAL ARGUMENTS
;;; - A decimal value that is the maximum difference allowed between the two
;;;   numbers that will still return T. Default = 0.000001d0.
;;; 
;;; RETURN VALUE
;;; T if the two tested numbers are equal within the specified tolerance,
;;; otherwise NIL.
;;; 
;;; EXAMPLE
#|
;; An example of floating-point error
(loop for i from 0.0 below 1.1 by 0.1 collect i)

=> (0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.70000005 0.8000001 0.9000001 1.0000001) 

;; Using =
(loop for i from 0.0 below 1.1 by 0.1 
   for j in '(0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
   collect (= i j))

=> (T T T T T T T NIL NIL NIL NIL)

;; Using equal-within-tolerance
(loop for i from 0.0 below 1.1 by 0.1 
   for j in '(0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
   collect (equal-within-tolerance i j))

=> (T T T T T T T T T T T)

|#
;;; SYNOPSIS
(defun equal-within-tolerance (a b &optional (tolerance 0.000001d0))
;;; ****
  (when (and (numberp a) (numberp b))
    ;; BTW coercing a and b to 'double-float doesn't help if they are of
    ;; different float types to begin with.
    (<= (abs (- a b)) tolerance)))

;; *** visualize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/visualize
;;; AUTHOR
;;; Leon Focker: leon@leonfocker.de
;;;
;;; DATE
;;; February 23rd 2023
;;; 
;;; DESCRIPTION
;;; Print a Visualization of an array or a list into the repl with 64 values
;;; -> *ascii art* <-
;;; 
;;; ARGUMENTS
;;; - An array or a list
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; :y-range. Maximum value that the y-axis display. When nil, the graph will be
;;; normalized.
;;; :start. Where to start reading the sequence from. Default = 0 
;;; :abs. When t, the absolute value of all numbers is visualized
;;; :scale. If the sequence is shorter than 64 and scale is t, the graph will
;;; be scaled to 64 values
;;; 
;;; RETURN VALUE
;;; ":)"
;;;
;;; EXAMPLE
#|
(visualize (loop repeat 64 for i from 0 by 0.1 collect (sin i)))
(visualize (loop repeat 128 for i from 0 by 0.1 collect (sin i)) :start 64)
(visualize (loop repeat 128 for i from 0 by 0.1 collect (* (sin i) 2))
   :scale nil :start 96)
(visualize (loop repeat 55 for i from 0 by 0.1 collect (* (sin i) 2))
   :scale nil :abs t :y-range 1)
|#
;;; SYNOPSIS
(defun visualize (ls &key y-range (start 0) abs (scale t))
;;; ****
  (when (arrayp ls)
    (setf ls (loop for i across ls collect i)))
  (when abs (setf ls (loop for i in ls collect (abs i))))
  (let* ((matrix (make-array '(64 17) :initial-element 0.0))
         (maxi (apply #'max (mapcar #'abs ls)))
         (y-range (if y-range y-range
                      (if (= maxi 0) 1 maxi)))
         (len (length ls))
         (size (if (or scale (>= (- len start) 64)) 64 (- len start))))
    (loop for i from start below (+ size start) do
         (loop for j below 17 do
              (if (= (round (+ (* (/ (nth (mod (floor
                                                (+ start
                                                   (if scale
                                                       (* (/ i size)
                                                          (- len start))
                                                       i)))
                                               len)
                                          ls)
                                     y-range)
                                  8 (if abs 2 1))
                               (* 8 (if abs 0 1))))
                     j)
                  (setf (aref matrix (- i start) j) 1)
                  (setf (aref matrix (- i start) j) 0))))
    (loop for j downfrom 16 to 0 do
         (print (apply 'concatenate 'string
                       (loop for i below 64 collect
                            (if (= (aref matrix i j)  1)
                                "_"
                                " ")))))
    "=)"))


;; *** agnostic-directory-pathname

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/agnostic-directory-pathname
;;; AUTHOR
;;; Leon Focker: leon@leonfocker.de
;;;
;;; DATE
;;; March 17th 2024.
;;;
;;; DESCRIPTION
;;; A replacement for directory-namestring, as that function does not return
;;; device names when used on windows.
;;;
;;; ARGUMENTS
;;; a string representing a pathname.
;;;
;;; RETURN VALUE
;;; a pathname - to a directory without filename and type
;;;
;;; SYNOPSIS
(defun agnostic-directory-pathname (namestring)
;;; ****
  (make-pathname :directory  (pathname-directory namestring)
                 :device (pathname-device namestring)))

;; *** get-all-files

(defun trailing-slash (path)
  (when (> (length path) 0)
    (if (char= #\/ (elt path (1- (length path))))
        path
      (format nil "~a/" path))))

(defun is-directory (path)
  (append (directory (starify path)) (directory (starify path t))))

(defun starify (path &optional (extension))
  (concatenate 'string (trailing-slash path) (if extension "*.*" "*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; skip is a list of directories to not include--it's the last directory
;;; name only we're interested in. pattern is a string that the file name must
;;; have, or a list of such strings
(defun get-all-files (dir &optional skip pattern force-quotes)
  (setq skip (force-list skip))
  (let ((files (loop for file in (directory (starify dir t))
                  for files = (namestring file)
                  for isdir = (is-directory files)
                  if (and isdir
                          (not (member (first (last (pathname-directory files)))
                                       skip :test #'string=)))
                  append (get-all-files files)
                  else if (not isdir) collect files)))
    ;; MDE Wed Dec 22 13:07:13 2021, Heidhausen
    (loop for pattn in (force-list pattern) do
      (setq files (remove-if-not #'(lambda (x) (search pattn x)) files)))
    ;; MDE Tue Sep 27 12:21:36 2022, Heidhausen -- for files with spaces etc. we
    ;; might want to put "" marks around the string
    (when force-quotes
      (setq files (mapcar #'(lambda (f) (format nil "\"~a\"" f)) files)))
    ;; MDE Mon Feb 28 15:12:25 2022, Heidhausen -- there's no way we could want
    ;; .DS_Store ... famous last words?
    (remove-if #'(lambda (f) (search ".DS_Store" f)) files)))

;; *** get-sndfiles

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Oct 1 16:23:43 2015 -- return T or NIL depending on whether
;;; sequence contains _all_ of the sub sequences in patterns
;;; e.g. (seq-has-all '("rests" "sdf" "kjr") "sdflkjrestsdf" ) --> T
(defun seq-has-all (patterns sequence)
  (loop with count = 0
     for p in patterns do
       (when (search p sequence)
         (incf count))
     finally (return (= count (length patterns)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun seq-has-some (patterns sequence)
  (loop for p in patterns do
    (when (search p sequence)
      (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Oct  1 16:29:34 2015 -- T or NIL depending on whether sequence has
;;; _any_ of the patterns.
;;; (seq-has-none '("rerts" "sadf" "kjrm") "sdflkjrestsdf" ) --> T
(defun seq-has-none (patterns sequence)
  (loop for p in patterns do
       (when (search p sequence)
         (return nil))
     finally (return t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* sndfile-palette/get-sndfiles
;;; DATE
;;; 5th September 2015, Edinburgh
;;; 
;;; DESCRIPTION
;;; Return a list of the full paths of sound files in the given path. Files
;;; without the extensions aif, wav, aiff, and snd are ignored.
;;; 
;;; ARGUMENTS
;;; The full path to the folder where the sound files are, as a string
;;; 
;;; OPTIONAL ARGUMENTS
;;; - a list of folders to skip i.e. just the last folder name, not the
;;;   complete path
;;; - A single pattern (string) or list of patterns that the sound
;;;   file name must have--just the filename, excluding path/folders and
;;;   extension. If a list then all patterns must be in the file name, not
;;;   just one of them, unless (hack alert!) the first element of the list is
;;;   the symbol or, in which case only one of the patterns must be
;;;   present. Default = NIL.  
;;; - Sim. to :insist except this/these are patterns none of which can
;;;   be in the file name. Default = NIL.
;;; 
;;; RETURN VALUE
;;; A list of full paths as strings.
;;; 
;;; EXAMPLE
#|
(get-sndfiles "/music/hyperboles/snd/cello/samples/"
              '("short-percussive" "weird"))
-->
("/music/hyperboles/snd/cello/samples/1/g4-III-4-001.aif"
 "/music/hyperboles/snd/cello/samples/1/g4-III-4-002.aif"
 "/music/hyperboles/snd/cello/samples/1/g4-III-4-003.aif"
 "/music/hyperboles/snd/cello/samples/1/g4-III-4-004.aif"
 "/music/hyperboles/snd/cello/samples/10/cs5-I-5-9-13-4-001.aif"
 ... 
|#
;;; SYNOPSIS
(defun get-sndfiles (folder &optional skip insist resist force-quotes)
;;; ****
  ;; MDE Wed Apr 15 11:14:59 2020 -- handle pathnames if we've used
  ;; e.g. merge-pathnames  
  (when (typep folder 'pathname)
    (setq folder (agnostic-directory-pathname folder)))
  (setq insist (force-list insist)
        resist (force-list resist))
  (loop for file in (get-all-files folder skip nil force-quotes)
        for name = (pathname-name file)
        when (and (member (pathname-type file) '("aif" "wav" "aiff" "snd"
                                                 "aif\"" "wav\"" "aiff\""
                                                 "snd\"")
                          :test #'string=)
                  ;; MDE Sat Jun 29 17:08:17 2024, Heidhausen -- this is a bit
                  ;; of a hack but I don't want to destroy existing
                  ;; functionality and it is somehow fitting
                  (if (eq (first insist) 'or)
                    (seq-has-some (rest insist) name)
                    (seq-has-all insist name))
                  (seq-has-none resist name))
          collect file))

;; ** envs

;; *** lastx

(defun lastx (env)
  "lastx returns the last x value in the given envelope.
   e.g. (lastx '(0 0 20 4 30 5 100 0)) => 100"
  (unless env
    (warn "utilities::lastx: env should not be nil."))
  (let ((len (length env)))
    (when (oddp len) 
        (error "utilities::lastx: Wrong number of elements in ~a." env))
    (when env (first (last env 2)))))

;; *** interpolate

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/interpolate
;;; DESCRIPTION
;;; Get the interpolated value at a specified point within an envelope. The
;;; envelope must be specified in the form of a list of break-point pairs.
;;; 
;;; ARGUMENTS
;;; - A number that is the point within the specified envelope for which to
;;;   return the interpolated value.
;;; - A list of break-point pairs.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :scaler. A number that is the factor by which to scale the values of
;;;   the break-point pairs in the given envelope before retrieving the
;;;   interpolated value. Default = 1.
;;; - :exp. A number that is the exponent to which the result should be
;;;   raised. Default = 1.
;;; - :warn. T or NIL to indicate whether the method should print a warning if
;;;   the specified point is outside of the bounds of the x-axis specified in
;;;   the list of break-point pairs. T = warn. Default = T.
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|
;;; Using the defaults
(interpolate 50 '(0 0 100 1))

=> 0.5

;;; Specifying a different scaler
(interpolate 50 '(0 0 100 1) :scaler 2)

=> 1.0

;;; Specifying a different exponent by which the result is to be raised
(interpolate 50 '(0 0 100 1) :exp 2)

=> 0.25

|#
;;; SYNOPSIS

(defun get-interpd-y (point x1 y1 x2 y2 exp)
  "The arguments are the point we want interpolated,
   the x,y coordinates of the line the point lies within, and an exponent.  
   The calculation is how far away point is from x1 divided by how far x2 is 
   from x1 raised to the exponent exp, multiplied by the difference of y2 and 
   y1 all added to y1."
  (float (+ y1 (* (expt (/ (- point x1) 
                           (- x2 x1)) exp)
                  (- y2 y1)))))

(defun ascending-xs (env &optional (error #'error))
  (let* ((xs (loop for x in env by #'cddr collect x))
         (ok (apply #'< xs)))
    (when (and (not ok) (functionp error))
      (funcall error "~&x values are not ascending in envelope ~a" env))
    ok))

(defun interp-aux (point env scaler exp)
  (ascending-xs env)
  (let ((here (loop for i in env by #'cddr and j from 1 do
    (if (<= point i) (return (+ j j -2))))))
    ;; rounding in making the new-env with new-lastx can cause the very last
    ;; event to be just a little bigger than the last x value in the new-env.
    ;; If this is the case, <here> will be nil so we better deal with this:
    (unless here 
      (setq here (- (length env) 2)))
    (when (zerop here)
      (setq here 2))
    ;; MDE Mon Apr  9 13:08:16 2012 -- catch divide by zero error
    (let ((x1 (nth (- here 2) env))
          (x2 (nth here env)))
      ;; MDE Mon May 14 12:26:16 2012 
      (unless (and (numberp x1) (numberp x2))
        (error "utilities::interp-aux: y values in envelope must be ~
                numbers: ~a"
               env))
      (if (= x1 x2)
        (progn
          (warn "utilities::interp-aux: weird values for interp-aux: ~a in ~a."
                point env)
          (nth (1- here) env))
        (get-interpd-y point 
                       x1 
                       (* scaler (nth (- here 1) env))
                       x2
                       (* scaler (nth (+ here 1) env))
                       exp)))))

(defun interpolate (point env &key (scaler 1) (exp 1) (warn t))
;;; ****
  "e.g. (interpolate 50 '(0 0 100 1) :scaler .5 :exp 2)
   => 0.0625
   The :EXP arg is the exponent that the interpolation result should
   be raised to."
  ;; MDE Thu Jul 14 21:29:59 2016 -- could happen...
  (if (not env)
    point
    (let ((lastx (lastx env))
          (lasty (first (last env))))
      (cond ((> point lastx)
             (when warn
               (warn "interpolate: ~a is off the x axis of ~%~a~
                      ~%returning last y value: ~a"
                     point env lasty))
             lasty)
            ((< point (car env))
             (let (y1)
               ;; (error "interpolate: Can't interpolate ~a in ~a" point env))
               ;; MDE Thu Apr 23 09:48:57 2020, Heidhausen -- if our x values
               ;; start > the point we're looking for, return the first y value
               (warn "utilities::interp-aux: envelope starts later than point!~
                      ~%Returning first y value: ~a" (setq y1 (second env)))
               y1))
            (t (interp-aux point env scaler exp))))))

;; *** scale-env

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/scale-env
;;; DESCRIPTION
;;; Scale either the x-axis values, the data values, or both of a list of
;;; break-point pairs by specified factors.
;;; 
;;; ARGUMENTS
;;; - An envelope in the form of a list of break-point pairs.
;;; - A number that is the factor by which the y values (data segment of the
;;;   break-point pairs) are to be scaled.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :y-min. A number that is the minimum value for all y values after
;;;   scaling.  NB The -min/-max arguments are hard-limits only; they do not
;;;   factor into the arithmetic.
;;; - :y-max. A number that is the maximum value for all y values after
;;;   scaling.
;;; - :x-scaler. A number that is the factor by which to scale the x-axis
;;;   values of the break-point pairs.
;;; - :x-min. A number that is the minimum value for all x values after
;;;   scaling. NB: This optional argument can only be used if a value has been
;;;   specified for the :x-scaler. 
;;; - :x-max. A number that is the maximum value for all x values after
;;;   scaling. NB: This optional argument can only be used if a value has been
;;;   specified for the :x-scaler.
;;; - :first-x. If a number, scale the x-axis so that this is the first x-value.
;;;   This then ignores x-scaler.
;;; - :last-x. If a number, scale the x-axis so that this is the last x-value.
;;;   This then ignores x-scaler.
;;; 
;;; RETURN VALUE
;;; An envelope in the form of a list of break-point pairs.
;;; 
;;; EXAMPLE
#|

;;; Scaling only the y values.
(scale-env '(0 53 25 189 50 7 75 200 100 3) 0.5)

=> (0 26.5 25 94.5 50 3.5 75 100.0 100 1.5)

;;; Scaling the y values and setting a min and max for those values
(scale-env '(0 53 25 189 50 7 75 200 100 3) 0.5 :y-min 20 :y-max 100)

=> (0 26.5 25 94.5 50 20 75 100 100 20)

;;; Scaling only the x-axis values
(scale-env '(0 53 25 189 50 7 75 200 100 3) 1.0 :x-scaler 2)

=> (0 53.0 50 189.0 100 7.0 150 200.0 200 3.0)

;;; Scaling the x values and setting a min and max for those values
(scale-env '(0 53 25 189 50 7 75 200 100 3) 1.0 :x-scaler 2 :x-min 9 :x-max 90)

=> (9 53.0 50 189.0 90 7.0 90 200.0 90 3.0)

;;; 'Stretching' the envelope by providing first-x and last-x values
(scale-env '(1 0 5 1 20 0) 1 :first-x 0 :last-x 100)

=> (0.0 0 21.052631 1 100.0 0)

|#
;;; SYNOPSIS
(defun scale-env (env y-scaler &key x-scaler first-x last-x
                                 (x-min most-negative-double-float)
                                 (y-min most-negative-double-float)
                                 (x-max most-positive-double-float)
                                 (y-max most-positive-double-float))
;;; ****
  (loop for x in env by #'cddr and y in (cdr env) by #'cddr 
     collect (cond ((or first-x last-x)
                    (let* ((old-first (first env))
                           (old-last (lastx env)))
                      (min x-max
                           (max x-min
                                (rescale x old-first old-last
                                         (or first-x old-first)
                                         (or last-x old-last))))))
                   (x-scaler (min x-max (max x-min (* x x-scaler))))
                   (t x))
     collect (min y-max (max y-min (* y y-scaler)))))


;; ** notes

#+cm
(defun find-scale (scale)
  (if (typep scale 'cm::tuning)
      scale
      (let ((sc (cm::find-object scale)))
        (unless sc 
          (warn "cm.lsp::find-scale: Can't find scale ~a." scale))
        sc)))

;; *** note-to-midi

#+cm
(defun degrees-per-octave (&optional (scale cm::*scale*))
;;; ****
  (- (cm::keynum 'cm::c5 :in scale) (cm::keynum 'cm::c4 :in scale)))

#+cm
(defun degree-to-note (degree &optional (scale cm::*scale*))
;;; ****
  ;; MDE Mon May 14 21:01:10 2012 -- 
  (when (and scale (symbolp scale))
    (setf scale (find-scale scale)))
  (intern (string (cm::note (round degree) :in scale)) :lyu))

 #+cm
(defun midi-to-degree (midi-note &optional (scale cm::*scale*))
;;; ****
  (* midi-note (/ (degrees-per-octave scale) 12)))

#+cm
(defun midi-to-note (midi-note &optional (scale cm::*scale*))
;;; ****
  ;; (degree-to-note midi-note cm::*chromatic-scale*))
  (degree-to-note (midi-to-degree midi-note scale) scale))

#+cm
(defun note-to-degree (note &optional (scale cm::*scale*))
;;; ****
  ;; MDE Mon May 14 21:01:10 2012 -- 
  (when (and scale (symbolp scale))
    (setf scale (find-scale scale)))
  (cm::keynum (intern (string note) :cm) :in scale))

#+cm
(defun note-to-midi (note)
;;; ****
  (note-to-degree note cm::*chromatic-scale*))

;; ** cm

#+cm(in-package :cm)

;; *** set-pitch-bend
#+cm
(defun set-pitch-bend (time channel bend)
  ;; (declare (special midimsg msg))
  ;; (output (new midimsg time time msg 
  ;; (format t "~&set-pitch-bend: time ~a channel ~a bend ~a" time channel bend)
  (new midi-pitch-bend :time time :channel channel
       :bend (if (zerop bend) 0 (round (rescale bend -2 2 -8192 8191)))))

;; *** cm:parse-midi-file

;;; ****f* cm/parse-midi-file
;;; DESCRIPTION
;;; Return Common Music MIDI events in the specified file as a list.
;;;
;;; NB: This is a Common Music function and as such must be called with the
;;;     package qualifier cm:: if used within slippery chicken.
;;; 
;;; ARGUMENTS 
;;; - The path (including the file name) to the MIDI file.
;;;
;;; OPTIONAL ARGUMENTS
;;; - An integer or NIL to indicate which track in the specified MIDI file is
;;;   to be accessed. If NIL, all tracks will be accessed. NB: CM (and
;;;   therefore slippery-chicken too) generates some MIDI files by writing each
;;;   channel to a different track, so the "track" would seem synonymous with
;;;   "channel" here. 
;;;
;;; RETURN VALUE  
;;; The CM data for the MIDI events in the specified file, and the number of
;;; events. 
;;;
;;; EXAMPLE
#|
(cm::parse-midi-file "/tmp/multi-ps.mid")

=>
(#i(midi-tempo-change time 0.0 usecs 357142)
 #i(midi-time-signature time 0.0 numerator 2 denominator 4 clocks 24 32nds 8)
 #i(midi time 0.0 keynum 36 duration 0.357142 amplitude 0.09448819 channel 15)
 #i(midi-tempo-change time 0.0 usecs 357142)
 #i(midi-time-signature time 0.0 numerator 2 denominator 4 clocks 24 32nds 8)
 #i(midi-tempo-change time 0.0 usecs 357142)
 #i(midi time 0.178571 keynum 66 duration 0.178571 amplitude 0.09448819 channel 15)
 #i(midi time 0.357142 keynum 68 duration 0.0892855 amplitude 0.09448819 channel 15)
 #i(midi time 0.357142 keynum 40 duration 0.357142 amplitude 0.09448819 channel 15)
 #i(midi time 0.6249985 keynum 66 duration 0.0892855 amplitude 0.09448819 channel 15)
 #i(midi-time-signature time 0.714284 numerator 3 denominator 4 clocks 24 32nds 8)

|#
;;; 
;;; SYNOPSIS

;;; btw the time slot is cm::object-time, midi note number cm::midi-keynum
#+cm
(defun parse-midi-file-aux (file &optional track tempo)
  ;; MDE Sat Nov 25 13:31:12 2017 -- ignore MIDI meta messages
  ;; MDE Wed Jan 30 14:21:24 2019 -- TODO: allow track to be T whereupon we
  ;; would get them all  
  (let ((midi-stream (import-events file :meta-exclude nil :tempo tempo)))
    (when track
      (setf midi-stream (nth track midi-stream)))
    midi-stream))

#+cm
(defun parse-midi-file (file &optional track tempo)
;;; ****
  (let ((midi-stream (parse-midi-file-aux file track tempo))
        (num-events 0))
    (setf num-events (length (subobjects midi-stream)))
    ;; MDE Mon Jun 17 11:47:47 2013 -- not much point printing them when the
    ;; list is printed in pretty much the same way. 
    #|
    (map-subobjects (lambda (n) (format t "~&Event ~A" n)) 
                    midi-stream)
    (format t "~&~a events total" num-events)
    |#
    (values (subobjects midi-stream) num-events)))

;; *** cm:output-midi-note

#+cm
(defun output-midi-note (midi-note pitch-bend time amplitude duration
                         channel)  
  (declare (special midi keynum))
  (list 
   (set-pitch-bend time channel pitch-bend)
   (new midi 
     :time time 
     :keynum midi-note
     ;; CM dict says: "A floating-point logical amplitude 0.0-1.0, or an integer
     ;; velocity 0-127"  
     :amplitude amplitude
     :duration duration  
     :channel channel)))

;; ** fibonacci (without morph)

(in-package :layers-utils)

;;; SAR Mon Jan 16 13:58:39 GMT 2012: Edited robodoc info
;;; SAR Sat Jan 14 16:14:01 GMT 2012: Edited robodoc info

;;; ****f* l-for-lookup/fibonacci
;;; DESCRIPTION
;;; Return the longest possible list of sequential Fibonacci numbers whose
;;; combined sum is less than or equal to the specified value. The list is
;;; returned in descending sequential order, ending with 0.  
;;;
;;; The function also returns as a second individual value the first Fibonacci
;;; number that is greater than the sum of the list returned. 
;;;
;;; NB: The value of the second number returned will always be equal to the sum
;;;     of the list plus one. In most cases that number will be less than the
;;;     number specified as the argument to the fibonacci function, and
;;;     sometimes it will be equal to the number specified; but in cases where
;;;     the sum of the list returned is equal to the number specified, the
;;;     second number returned will be equal to the specified number plus one.
;;; 
;;; ARGUMENTS 
;;; A number that is to be the test number.
;;; 
;;; RETURN VALUE  
;;; A list of descending sequential Fibonacci numbers, of which list the last
;;; element is 0. 
;;;
;;; Also returns as a second individual value the first Fibonacci number that
;;; is greater than the sum of the list returned, which will always be the sum
;;; of that list plus one.
;;;
;;; EXAMPLE
#|
;; Returns a list of consecutive Fibonacci numbers from 0 whose sum is equal to
;; or less than the value specified. The second number returned is the first
;; Fibonacci number whose value is greater than the sum of the list, and will
;; always be the sum of the list plus one.
(fibonacci 5000)

 => (1597 987 610 377 233 144 89 55 34 21 13 8 5 3 2 1 1 0), 4181

;; The sum of the list
(+ 1597 987 610 377 233 144 89 55 34 21 13 8 5 3 2 1 1 0) 

=> 4180

|#
;;; 
;;; SYNOPSIS
(defun fibonacci (max-sum)
;;; ****
  (loop 
     ;; our result will be in descending order
     with result = '(1 0) 
     ;; the running total of sums
     with cumulative-sum = 1
     for x = 0 
     for y = 0 
     ;; the sum of our two most recent numbers.
     for sum = 0 
     do
     (setf x (first result)
           y (second result)
           sum (+ x y))
     (incf cumulative-sum sum)
     (when (> cumulative-sum max-sum)
       ;; (print cumulative-sum)
       ;; (print sum)
       ;; we're not using sum this time as we're over our limit.
       (return (values result (1+ (- cumulative-sum sum)))))
     (push sum result)));;; SAR Mon Jan 16 13:58:39 GMT 2012: Edited robodoc info
;;; SAR Sat Jan 14 16:14:01 GMT 2012: Edited robodoc info

;;; ****f* l-for-lookup/fibonacci
;;; DESCRIPTION
;;; Return the longest possible list of sequential Fibonacci numbers whose
;;; combined sum is less than or equal to the specified value. The list is
;;; returned in descending sequential order, ending with 0.  
;;;
;;; The function also returns as a second individual value the first Fibonacci
;;; number that is greater than the sum of the list returned. 
;;;
;;; NB: The value of the second number returned will always be equal to the sum
;;;     of the list plus one. In most cases that number will be less than the
;;;     number specified as the argument to the fibonacci function, and
;;;     sometimes it will be equal to the number specified; but in cases where
;;;     the sum of the list returned is equal to the number specified, the
;;;     second number returned will be equal to the specified number plus one.
;;; 
;;; ARGUMENTS 
;;; A number that is to be the test number.
;;; 
;;; RETURN VALUE  
;;; A list of descending sequential Fibonacci numbers, of which list the last
;;; element is 0. 
;;;
;;; Also returns as a second individual value the first Fibonacci number that
;;; is greater than the sum of the list returned, which will always be the sum
;;; of that list plus one.
;;;
;;; EXAMPLE
#|
;; Returns a list of consecutive Fibonacci numbers from 0 whose sum is equal to
;; or less than the value specified. The second number returned is the first
;; Fibonacci number whose value is greater than the sum of the list, and will
;; always be the sum of the list plus one.
(fibonacci 5000)

 => (1597 987 610 377 233 144 89 55 34 21 13 8 5 3 2 1 1 0), 4181

;; The sum of the list
(+ 1597 987 610 377 233 144 89 55 34 21 13 8 5 3 2 1 1 0) 

=> 4180

|#
;;; 
;;; SYNOPSIS
(defun fibonacci (max-sum)
;;; ****
  (loop 
     ;; our result will be in descending order
     with result = '(1 0) 
     ;; the running total of sums
     with cumulative-sum = 1
     for x = 0 
     for y = 0 
     ;; the sum of our two most recent numbers.
     for sum = 0 
     do
     (setf x (first result)
           y (second result)
           sum (+ x y))
     (incf cumulative-sum sum)
     (when (> cumulative-sum max-sum)
       ;; (print cumulative-sum)
       ;; (print sum)
       ;; we're not using sum this time as we're over our limit.
       (return (values result (1+ (- cumulative-sum sum)))))
     (push sum result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fibonacci-start-at-2 (max-sum)
;;; ****
  (multiple-value-bind
      (series sum)
      (fibonacci (+ 2 max-sum)) ; + 2 so we can hit max-sum if need be
    ;; subseq returns a sequence out of our list
    (values (subseq series 0 (- (length series) 3))
            (- sum 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Once we have the numbers e.g. (8 5 3 2 1) we convert into indices e.g.
;;; (0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1 0 1 1)
;;;                8         5     3   2 1
(defun fibonacci-transition-aux2 (list item1 item2)
  (let ((result '()))
    (loop for num in list do
       ;; so each time we have 'num' items, all but one of which are item1
         (loop repeat (1- num) do
              (push item1 result))
         (push item2 result))
    ;; we've used push so we need to reverse the list before returning
    (nreverse result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Say you want a transition between two repeating states over a period of x
;;; repetitions; this gives you a gradual break in of the second state using
;;; Fibonacci relationships.
;;; <item1> is the start item, <item2> the item we want to transition towards
;;; e.g. (fibonacci-transition-aux1 21 0 1) ->
;;; (0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1 0 1 0 1 1)   
(defun fibonacci-transition-aux1 (num-items &optional
                                  (item1 0)
                                  (item2 1))
  ;; local function: usually done with flet but you can't call flet functions
  ;; recursively...
  (labels ((ftar (num)
             ;; lisp functions can return more than one value (e.g. (floor
             ;; 3.24) usually you will only want the first value (as in the
             ;; case of floor) but we can get them all using
             ;; multiple-value-bind and friends.
             (multiple-value-bind
                   (series sum)
                 ;; returns a list of descending fib numbers and their sum--this
                 ;; will be < num-items
                 (fibonacci-start-at-2 num)
               (let ((remainder (- num sum)))
                 (if (> remainder 2)
                     ;; recursive call: what we're looking for is a descending
                     ;; list of fib numbers that total <num-items> exactly,
                     ;; hence we have to keep doing this until we've got
                     ;; num-items
                     (append series (ftar remainder))
                     ;; we're done so just store the remainder and return
                     (progn
                       (when (> remainder 0)
                         (push remainder series))
                       series))))))
    ;; we might have something like (2 5 3 2 8 5 3 2) so make sure we sort them
    ;; in descending order.  Note that our sort algorithm takes a function as
    ;; argument.
    (fibonacci-transition-aux2
     (stable-sort (ftar num-items) #'>)
     item1 item2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fibonacci-transition (num-items &optional (item1 0) (item2 1))
;;; ****
  ;; just some sanity checks
  (unless item1
    (setf item1 0))
  (unless item2
    (setf item2 1))
  ;; we use the aux1 function to first move towards more of item2, but then
  ;; again for less of item1.  The point at which this shift occurs is at the
  ;; golden section (where else?).
  (let* ((left-num (round (* num-items .618)))
         (right-num (- num-items left-num))
         ;; get the two transitions.
         (left (fibonacci-transition-aux1 left-num item1 item2))
         ;; this one will be reversed
         (right (fibonacci-transition-aux1 right-num item2 item1))
         result)
    ;; avoid two item1s at the crossover. we use equal as it can handle number
    ;; and symbol comparison
    (when (equal (first (last right))
                 item1)
      ;; butlast returns it's argument minus the last element
      ;; e.g. (butlast '(1 2 3 4)) -> (1 2 3)
      (setf right (butlast right))
      (push item2 right))
    ;; append the two lists and return.  we can use nreverse (which is more
    ;; efficient) rather than reverse as we won't need the original version of
    ;; result
    (setq result (append left (nreverse right))
          result result)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun items-per-transition (num-items num-groups)
  (if (= 1 num-groups)
      nil
    (let ((transitions (1- num-groups)))
      (multiple-value-bind
       (floor remainder)
       (floor num-items transitions)
       (loop repeat transitions
             with plus = 1
             do
             (if (> remainder 0)
                 (decf remainder)
               (setf plus 0))
             collect (+ plus floor))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fibonacci-transitions-aux (total-items levels)
  (let ((ipt (items-per-transition total-items levels)))
    (loop for num in ipt and i from 0 appending
         (fibonacci-transition num i (1+ i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE original thoughts:
;;;
;;; This allows multiple transitions 
;;; so <levels>=2 would just return 0s and 1; 3 would have 0,1,2; 1 just 0
;;; 
;;; When we use items-per-transition for generating the fibonacci transition,
;;; although the numbers returned by that function are about equal, the
;;; transition results in an uneven spread of numbers e.g.  (count-elements
;;; (fibonacci-transitions-aux 200 3)) --> (0 60 1 100 2 40) in fact we seem to
;;; get 3/5 of the first and 2/5 of the last element with the middle elements
;;; being equal.  Looking again, I see that the number of the first element +
;;; number of last is close to or the same as the number of the middle numbers,
;;; which are always about the same or equal and is total-items / 1- levels--so
;;; this is the number that will be missing from the first and last (in a 3:2
;;; proportion). Try and balance this out (won't be perfect but pretty near).

;;; SAR Sat Jan 14 17:50:08 GMT 2012: Edited robodoc info
;;; ****f* l-for-lookup/fibonacci-transitions
;;; DATE
;;; 18 Feb 2010
;;;
;;; DESCRIPTION
;;; This function builds on the concept of the function fibonacci-transition by
;;; allowing multiple consecutive transitions over a specified number of
;;; repetitions. The function either produces sequences consisting of
;;; transitions from each consecutive increasing number to its upper
;;; neighbor, starting from 0 and continuing through a specified number of
;;; integers, or it can be used to produce a sequence by transitioning between
;;; each element of a user-specified list of items.
;;;
;;; ARGUMENTS 
;;; - An integer indicating the number of repetitions over which the
;;;   transitions are to be performed.
;;; - Either:
;;;   - An integer indicating the number of consecutive values, starting from
;;;     0, the function is to transition (i.e. 3 will produce a sequence that
;;;     transitions from 0 to 1, then from 1 to 2 and finally from 2 to 3), or 
;;;   - A list of items of any type (including lists) through which the
;;;     function is to transition.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to make a morph from item1 to item2, 2 to 3, etc.. This means
;;;   morphing items will be a morph structure. This can also be an envelope
;;;   which thus allows morphing to be switched on and off over the course of
;;;   the transition. The envelope can have any x range but the y-range should
;;;   be from 0 to 1. Any value < 1 means the morph object will be ignored and
;;;   either the :i1 or :i2 slot will be used instead, depending on whether the
;;;   :proportion slot is > 0.5
;;;
;;; RETURN VALUE  
;;; A list.
;;; 
;;; EXAMPLE
#|
;; Using just an integer transitions from 0 to 1 below that integer
(fibonacci-transitions 76 4)

=> (0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1 0 1 0 1 1 0 1 0 1 1 1 1 1 1 1 2 1 1 2 1 2 1
    2 2 1 2 1 2 2 2 2 2 2 2 3 2 2 3 2 3 2 3 3 2 3 2 3 3 3 2 3 3 3 3 3 3 3 3 3 3)

;; Using a list transitions consecutively through that list
(fibonacci-transitions 152 '(1 2 3 4))

=> (1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 2 1 1 1 1 2 1 1 2 1 1 2 1 2 1 2
    2 1 2 1 2 2 1 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 3 2 2 3 2 2 3 2 3 2 3
    3 2 3 2 3 3 2 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 4 3 3 3 3 4 3 3 4 3 3 4 3 4 3 4
    4 3 4 3 4 4 3 4 4 3 4 4 4 4 4 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4)

;; A list of lists is also viable
(fibonacci-transitions 45 '((1 2 3) (4 5 4) (3 2 1)))

=> ((1 2 3) (1 2 3) (1 2 3) (1 2 3) (1 2 3) (4 5 4) (1 2 3) (1 2 3) (4 5 4)
    (1 2 3) (4 5 4) (1 2 3) (4 5 4) (1 2 3) (4 5 4) (1 2 3) (4 5 4) (1 2 3)
    (4 5 4) (4 5 4) (4 5 4) (4 5 4) (4 5 4) (3 2 1) (4 5 4) (3 2 1) (4 5 4)
    (3 2 1) (4 5 4) (3 2 1) (4 5 4) (3 2 1) (4 5 4) (3 2 1) (3 2 1) (3 2 1)
    (4 5 4) (3 2 1) (3 2 1) (3 2 1) (3 2 1) (3 2 1) (3 2 1) (3 2 1) (3 2 1))
|#
;;; SYNOPSIS
(defun fibonacci-transitions (total-items levels)
;;; ****
  (let ((len (typecase levels 
               (list (length levels))
               (integer levels)
               (t (error "l-for-lookup::fibonacci-transitions: ~
                          levels must be a list or an integer: ~a"
                         levels)))))
    (when (<= (floor total-items len) 2)
      (error "l-for-lookup::fibonacci-transitions: can't do ~a transitions ~
              over ~a items." total-items len))
    (if (= 1 len)
        (make-list total-items :initial-element 0)
        (let* ((lop-off (floor total-items len))
               (new-len (- total-items lop-off))
               (result (fibonacci-transitions-aux new-len len))
               ;; MDE Tue May 3 16:33:13 2016 -- (create the golden section
               ;; with two large fib nums and double-precision)
               (add-end (floor (* (/ 17711.0d0 28657) lop-off)))
               (add-beg (- total-items new-len add-end))
               (beg (append (make-list (1- add-beg) :initial-element 0) (list 1)))
               (end (make-list (1- add-end) :initial-element (1- len)))
               (transition (progn
                             (push (- len 2) end)
                             (append beg result end))))
          ;; convert references to the levels list elements, if given
          (setq transition
                (if (listp levels)
                    (loop for el in transition collect (nth el levels))
                    transition))
          transition))))

;; EOF slippery-utils.lsp
