
;; ** utilities

(in-package :layers-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                [ SIMPLE ]                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; *** mirrors
;;; formerly calles alternating-modulo
;;; mirrors input value between min and max value
(defpublic mirrors (value min max)
  (labels ((helper (val)
	     (cond ((> val max) (helper (- max (- val max))))
		   ((< val min) (helper (+ min (- min val))))
		   (t val))))
    (helper value)))

;; *** dry-wet
(defpublic dry-wet (dry wet mix)
  (let ((mix (min 1 (max mix 0))))
    (+ (* wet mix)
       (* dry (- 1 mix)))))

;; *** mod1
;;; (mod x 1), but return 1 if (and (= 0 (mod x 1)) (not (= 0 x)))
(defpublic mod1 (x)
  (let ((x (rational x)))
    (cond ((= 0 x) 0)
	  ((= 0 (mod x 1)) 1)
	  (t (mod x 1)))))

;; *** random-non-linear-distribution-density
;;; Random numbers between 0.0 and 1.0 but with a non-linear distribution
;;; density except for expt = 1.0.
;;; - expt: shape the distribution density curve:
;;;         expt < 1.0  ->  more likely to get 0.5 than 0 or 1
;;;         expt > 1.0  ->  more likely to get 0 or 1 than 0.5
;;; - input: use this number between 0.0 and 1.0 instead of a random one
(defpublic random-non-linear-distribution-density (&optional (expt 0.8) input)
  (when (and input (or (not (numberp input)) (not (<= 0 input 1.0))))
    (error "random-non-linear-distribution-density: input should be a number ~
            between 0.0 and 1.0 but is ~a." input))
  (let ((rand (or input (random 1.0))))
    (if (< rand 0.5)
	(* 0.5 (expt (* 2 rand) expt))
	(- 1.0 (* 0.5 (expt (* 2 (- 1.0 rand)) expt))))))

;; alias
(setf (symbol-function 'random-nldd) #'random-non-linear-distribution-density)
(export 'random-nldd :layers-utils)

;; *** equal-tempered-multiplier
;;; return the multiplier a frequency has to be multiplied with, to get the
;;; interval for the desired number of steps. The standard values for this
;;; function assume the 12 tone equal temperament with the division of the
;;; octave (2 times the frequency) in 12 equal parts.
(defpublic equal-tempered-multiplier (steps &optional (octave 2) (division 12))
  (expt (expt octave (/ 1 division)) steps))

;; *** load-from-file
;;; eval something read from a file.
(defpublic load-from-file (file)
  (eval (read-from-file file)))

;; *** check-all-types
(defmacro check-all-types (type &rest vars)
  `(progn
     ,@(loop for var in vars
             collect `(check-type ,var ,type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                [ LISTS ]                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; *** remove-nth
;;; remove nth element from list
(defpublic remove-nth (n list)
  (declare
   (type (integer 0) n)
   (type list list))
  (if (or (zerop n) (null list))
      (cdr list)
      (cons (car list) (remove-nth (1- n) (cdr list)))))

;; *** nth-mod
;;; nth element from a list but if n > (length list), loop contents of list.
(defpublic nth-mod (n ls)
  (unless (and (listp ls) (not (null ls)))
    (error "nth-mod: ls is either empty or not a list: ~a" ls))
  (nth (mod n (length ls)) ls))

;; *** rotate
;;; rotate a list so that it starts with the specified index.
(defpublic rotate (ls &optional (new-start-index 1))
  (let* ((len (length ls))
         (effective-index (mod new-start-index len)))
    (append (subseq ls effective-index)
            (subseq ls 0 effective-index))))

;; *** push-key-value
;;; push a key-value pair to a (p)list.
(defmacro push-key-value (key value place)
  `(progn (push ,value ,place)
	  (push ,key ,place)))

(export 'push-key-value :layers-utils)

;; *** setf-key-value
;;; set key to value in (p)list.
(defmacro setf-key-value (plist key value)
  `(setf (getf ,plist ,key) ,value))

(export 'setf-key-value :layers-utils)

;; *** re-order
;;; take a list and a new-order as input. The latter should be a list of
;;; indices, which will then be used to re-order the first list. If the 
;;; new-order list is not long enough, all elements that have not been used will
;;; be appended. If some index in new-order is too big, the mod will be taken.
;;; If the same index appears twice, the element following the already chosen 
;;; one will be taken.
;;; (re-order '(a b c d e) '(4 1 3 2 0)) => '(e b d c a)
;;; (re-order '(a b c d e) '(3 4 1)) => '(d e b a c)
;;; (re-order '(a b c d e) '(2 3 4 6 0 1)) => '(c d e b a)
;;; (re-order '(a b c d e) '(0 3 7 0 7 3 5)) => '(a d c b e)
;;; (re-order '(a b c d e) '(0 0 0)) => '(a b c d e)
(defpublic re-order (ls new-order)
  (let* ((len (length ls))
	 (all-i (loop for i from 0 below len collect i)))
    (append
     (loop repeat len for index in new-order for j from 0 collect
	  (do ((n (mod index len) (mod (+ n 1) len)))
	      ((member n all-i)
	       (progn (when (member n all-i) (setf all-i (remove n all-i)))
		      (nth n ls)))))
     (loop for n in all-i collect (nth n ls)))))

;; *** move-item
;;; move items from a certain index to a new index. This is not achieved
;;; by swapping items! The item is taken from the list and then placed into
;;; the list at the new position.
(defpublic move-item (list index new-index)
  (let ((index (mod index (length list)))
	(new-index (mod new-index (length list))))
    (unless (= index new-index)
      (let* ((min (min index new-index))
	     (max (max index new-index))
	     (diff (1+ (- max min)))
	     (list-head (subseq list 0 min))
	     (list-tail (nthcdr min list))
	     (mid '()))
	(setf mid (subseq list-tail 0 diff)
	      mid (if (> index new-index)
		      (append (last mid) (subseq mid 0 (1- diff)))
		      (append (cdr mid) (list (first mid))))
	      list-tail (nthcdr diff list-tail)
	      list (append list-head mid list-tail))))
    list))

;; *** depth
;;; the depth of a (possibly) nested list
(defpublic depth (ls)
  (labels ((iter (ls level)
	     (if (listp ls)
                 (let ((sublevels '()))
                   (dolist (s ls)
		     (push (iter s (1+ level)) sublevels))
                   (apply #'max (cons level sublevels)))
                 level)))
    (iter ls 0)))

;; *** rqq-depth
;; TODO is this used anywhere??
(defpublic rqq-depth (ls)
  (apply #'max (labels ((iter (ls level)
			  (loop for div in ls append
					      (if (listp div)
						  (iter (second div) (1+ level))
						  (list level)))))
		 (iter (second ls) 0))))

;; *** normalize-depth
;;; Modifies a list, wrapping parts of it in new lists,
;;; so that every element is at uniform depth.
(defpublic normalize-depth (ls &optional (in-place t))
  (unless in-place (setq ls (copy-seq ls)))
  (let ((max-depth (loop for element in ls maximize (depth element))))
    (loop for i below (length ls)
       do (loop while (< (depth (nth i ls)) max-depth)
             do (setf (nth i ls) (list (nth i ls)))))
    (loop for element in ls
       do (when (listp element)
            (normalize-depth element)))
    ls))

;; *** scale-list-to-sum
;;; scale the elements in a list so that the sum of these elements = target-sum
(defpublic scale-list-to-sum (ls &optional (target-sum 1))
  (let* ((sum (loop for i in ls sum (rationalize i)))
	 (scaler (/ target-sum sum)))
    (loop for i in ls collect (* i scaler))))

;; *** avoid-repetition
;;; in a list of elements, when an element appears twice in a row it is cut
(defpublic avoid-repetition (ls &optional within-tolerance?)
  (loop for i in ls with last unless (if within-tolerance?
					 (equal-within-tolerance last i 1.0d-5)
					 (equal last i))
	collect i do (setf last i)))

;; *** insert
;;; insert an element into list at index
(defpublic insert (ls index newelt)
  (if (= 0 index) (push newelt ls) (push newelt (cdr (nthcdr (1- index) ls))))
  ls)

;; *** insert-multiple
;;; insert multiple elements on multiple indices in a list
(defpublic insert-multiple (ls index-list newelt-list)
  (let* ((len (length newelt-list)))
    (unless (= (length index-list) len)
      (error "both index-list and newelt-list in insert-multiple should have ~
              the same length"))
    (loop for i from 0 and el in ls
       when (member i index-list) append
	 (loop for j in index-list and new in newelt-list
	    when (= i j) collect new)
       collect el)))

;; *** biggest-jump
;;; discrete derivation maximum i guess?
(defpublic biggest-jump (ls)
  (loop for i in (cdr ls)
     with last = (car ls)
     maximize (abs (- last i))
     do (setf last i)))

;; *** biggest-jump-up
(defpublic biggest-jump-up (ls)
  (loop for i in (cdr ls)
     with last = (car ls)
     maximize (- i last)
     do (setf last i)))

;; *** biggest-jump-down
(defpublic biggest-jump-down (ls)
  (loop for i in (cdr ls)
     with last = (car ls)
     maximize (- last i)
     do (setf last i)))

;; *** reduce-by
;;; reduces a list by factor (100 elements to 10 elements, factor 10)
;;; using averages
(defpublic reduce-by (ls &optional (factor 10))
  (unless (integerp factor)
    (error "reduce-by needs a factor that is an integer, not ~a" factor))
  (let* ((new '()))
    (loop for i from 1 and e in ls with sum = 0 do
	 (incf sum (/ e factor))
	 (when (= (mod i factor) 0)
	   (push sum new)
	   (setf sum 0)))
    (reverse new)))

;; *** list-interp
;;; looks at a list as a series of y values, spaced equally from 0 to max-x
;;; give any rational x to return interpolated result between two y values
(defpublic list-interp (x list &optional (max-x (1- (length list))))
  (let* ((len (1- (length list)))
	 last-y
	 next-y
	 inter)
    (cond ((<= x 0) (first list))
	  ((>= x max-x) (car (last list)))
	  (t (setf last-y (nth (floor (* (/ x max-x) len)) list)
		   next-y (nth (ceiling (* (/ x max-x) len )) list)
		   inter (mod (* (/ x max-x) len) 1))
	     (+ (* inter next-y)
		(* (- 1 inter) last-y))))))

;; *** make-list-into-function
;;; curried list-interp
(defpublic make-list-into-function (list duration) ; &optional last-at-what-x)
  (lambda (x) (list-interp x list duration)))

;; alias
(setf (symbol-function 'list-to-function) #'make-list-into-function)
(export 'list-to-function :layers-utils)

;; *** lists-to-envelopes
;;; combine a list of x-values and one of y-values into an envelope
(defpublic lists-to-envelopes (x-list y-list)
  (unless (= (length x-list) (Length y-list))
    (error "the lists given to lists-to-envelopes should be of the same length ~
           but are of length: ~a and ~a" (length x-list) (length y-list)))
  (let* ((envelopes '()))
    (loop for x in x-list and y in y-list with new-env with last-x = 0 do
	 (when (< x last-x)
	   (push (reverse new-env) envelopes)
	   (setf new-env '()))
	 (push x new-env)
	 (push y new-env)
	 (setf last-x x)
       finally (push (reverse new-env) envelopes))
    (reverse envelopes)))

;; *** flatness-of-list
;;; this might not be useful at all for anything but spectra (spectral flatnes)
(defpublic flatness-of-list (ls)
  (let ((arithmetic-mean 0)
	(geometric-mean 0)
	(len (length ls)))
    (loop for i in ls do
	 (when (= i 0) (setf i 0.000001))
	 (incf arithmetic-mean i)
	 (incf geometric-mean (log i)))
    (setf arithmetic-mean (/ arithmetic-mean len))
    (setf geometric-mean (exp (/ geometric-mean len)))
    (/ geometric-mean arithmetic-mean)))

;; *** get-spectral-centroid
;;; calulate the spectral centroid out of a list of frequency-magnitude pairs
;;; eq.: '((440 0.5) (630 0.46) (880 0.25))
;;; TODO this is different from the method used in #'analyse-soundfile
(defpublic get-spectral-centroid (list-of-pairs)
  (let ((centroid 1))
    (loop for pair in list-of-pairs
       sum (* (car pair) (cadr pair)) into sum1
       sum (cadr pair) into sum2
       do (setf centroid (/ sum1 sum2)))
    centroid))

;; *** parabolic-peak-offset
;;; Given three y-values (y-1, y0 and y+1) that describe a curve, calculate the
;;; x-value at which this curve hits a maximum relative to x0.
;;; Example: (parabolic-peak-offset 1 2 0) -> -1/6
(defpublic parabolic-peak-offset (y-1 y0 y+1)
  (/ (- y-1 y+1)
     (* 2 (- y-1 (* 2 y0) y+1))))

;; *** harmonic-score
;;; Sum all magnitudes of the harmonics of i, do this for multiple i and
;;; compare; higher scores mean higher likelyhood of i being a fundamental.
(defun harmonic-score (mags i &optional (max-h 10))
  (loop for h from 1 to max-h
	for idx = (* i h)
	while (< idx (length mags))
        sum (aref mags idx)))

;; *** harmonic-product-spectrum
;;; Very simple monophonic pitch detection from a list of magnitudes and the
;;; base-frequency of the first bin. Returns a frequency.
(defpublic harmonic-product-spectrum (list-of-mags
				      base-freq
				      &optional (max-freq 2000))
  (let* ((n (length list-of-mags))
	 (mags (make-array n :initial-contents list-of-mags))
	 (max-bin (min (1- n) (floor (/ max-freq base-freq))))
	 (result (copy-seq mags))
	 (max-h 5)
	 peak-idx)
    ;; reduce spectrum and add harmonics to result vector
    (loop for h from 2 to max-h do
      (loop for i from 0 below (min max-bin (/ n h)) do
	;; dividing by h again adds a bias towards original spectrum
	(incf (aref result i) (/ (aref mags (* i h)) h))))
    ;; find index of peak
    (loop for i from 1 below max-bin
	  for val across result
	  with peak-val = (aref result 0) do
	    (when (> val peak-val)
	      (setf peak-idx i peak-val val)))
    ;; ;; debugging
    ;; (plot-points (loop for i from 0 and m across mags
    ;; 		       collect (list (* i base-freq 1.0) m))
    ;; 		 t t)
    ;; (plot-points (loop for i from 0 and m across result
    ;; 		       collect (list (* i base-freq 1.0) m))
    ;; 		 t t)
    (when peak-idx
      ;; check whether found peak could actually be second or third partial
      (loop for idx in (list peak-idx (floor peak-idx 2) (floor peak-idx 3))
	    with mag-threshold = (* (aref mags peak-idx) 0.3)
	    with best-score = (harmonic-score mags peak-idx) do
	      (when (and (> idx 0)
			 (> (aref mags idx) mag-threshold)
			 (> (harmonic-score mags idx) best-score))
		(setf peak-idx idx)))
      ;; calculate frequency
      (let* ((offset (parabolic-peak-offset
		      (aref mags (1- peak-idx))
		      (aref mags peak-idx)
		      (aref mags (1+ peak-idx)))))
	(when (> (abs offset) 1) (setf offset 0.0))
	(* (+ offset peak-idx) base-freq)))))

;; *** taylor-polynom
;;; TODO
;;; get taylor polynomial, pls check this again, probably false
(defpublic second-order-taylor (ls x)
  (let* ((len (length ls))
	 (n (floor (* x len)))
	 ;; list as function
	 (f1 #'(lambda (n) (nth n ls)))
	 ;; crooked derivative function
	 (f2 #'(lambda (f n) (/ (- (funcall f (1+ n)) (funcall f (1- n)))
			   (/ 2 len))))
	 (a (funcall f1 n))
	 (b (funcall f2 f1 n))
	 (c (funcall f2 #'(lambda (n) (funcall f2 f1 n)) n))
	 )
    (lambda (x) (+ a
	      (* b x)
	      (* c (expt x 2))))))

;; *** plot-points
;;; Calls gnuplot in a shell, so gnuplot must be installed.
;;; (check with gnuplot --version)
;;; - points: list of (x y) pairs.
(defun plot-points (points &optional lines logscale)
  (let ((data-file "/tmp/points.dat"))
    (with-open-file (out data-file :direction :output :if-exists :supersede)
      (dolist (p points)
	(format out "~A ~A~%" (first p) (second p))))
    (uiop:run-program
     (format nil "gnuplot -p -e \"~aplot '/tmp/points.dat' with ~a\""
	     (if logscale "set logscale x 10; " "")
	     (if lines "lines" "points")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          [ LISTS OF DURATIONS ]                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; *** get-start-times
;;; TODO should also account for rests (in parens)
(defpublic get-start-times (list-of-durations)
  (loop for i in list-of-durations
	and sum = 0 then (+ sum i)
	collect sum))

;; *** get-durations
;;; TODO should also account for rests (in parens)
;;; collect the durations in a certain section of a certain layer
(defpublic get-durations (list-of-start-times)
  (let ((ls (avoid-repetition (sort list-of-start-times #'<) t)))
    (loop for time in (cdr ls)
       with last = (car ls)
       collect (- time last)
       do (setf last time))))

;; *** get-durations-within
;;; TODO should also account for rests (in parens)
;;; collect the durations in a certain section of a certain layer
(defpublic get-durations-within (durations min max)
  (loop for i in durations sum i into sum while (< sum max)
     when (> sum min) collect i))

;; *** get-start-times-within
;;; TODO should also account for rests (in parens)
;;; collect the start-times in a certain section of a certain layer
(defpublic get-start-times-within (durations min max)
  (loop for i in durations sum i into sum while (< sum max)
     when (> sum min) collect sum))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                              [ ENVELOPES ]                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; *** make-function-into-env
(defpublic make-function-into-env (function &optional (from 0) (to 1) (step .1))
  (unless (and (numberp from) (numberp to) (numberp step))
    (error "from, to and step in #'make-function-into-env must be a number"))
  (unless (<= from to)
    (error "'from' should be <= than 'to' but they are: ~a ~a" from to))
  (loop for x from from to to by step collect x collect (funcall function x)))

;; alias
(setf (symbol-function 'function-to-env) #'make-function-into-env)
(export 'function-to-env :layers-utils)

;; *** combine-envelopes
;;; take a list of envelopes and a list of durations and combine all
;;; envelopes into one.
#|
(combine-envelopes '((0 0  1 1) (0 3  5 3)) '(5 5))
=> '(0.0 0 5.0 1 5.0001 3  10.0 3)
|#
(defpublic combine-envelopes (list-of-envelopes list-of-durations)
  (unless (= (length list-of-envelopes) (length list-of-durations))
    (error "list-of-envelopes and list-of-durations should be the same length: ~
            ~&~a ~a"  list-of-envelopes list-of-durations))
  (let* ((starts (cons 0 (loop for i in list-of-durations sum i into sum
			       collect sum))))
    (setf list-of-envelopes
	  (loop for s in starts and e in (cdr starts)
		and env in list-of-envelopes and i from 0
		do (when (> i 0) (setf s (+ s .0001)))
		collect (scale-env env 1 :first-x s :last-x e)))
    (flatten list-of-envelopes)))

;; *** rescale-env
;;; rescale all y-values of an envelope using rescale
(defpublic rescale-env (env new-y-min new-y-max)
  (loop for y in (cdr env) by #'cddr
	minimize y into old-min
	maximize y into old-max
	finally
	   (return
	     (loop for x in env by #'cddr and y in (cdr env) by #'cddr
		   collect x
		   collect (rescale y old-min old-max new-y-min new-y-max)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                [ ARRAYS ]                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; *** distance-between-points
;;; distance between two points p1 and p2,
;;; both need to be vectors with the same length
(defpublic distance-between-points (p1 p2)
  (unless (and (vectorp p1) (vectorp p2) (= (length p1) (length p2)))
    (error "~&both arguments need to be same-length vectors in function ~
             distance-between-points, ~%args: ~a ~a" p1 p2))
  (sqrt (loop for q across p1 and p across p2 sum (expt (- q p) 2.0))))

;; *** max-of-array
;;; max of an array
(defpublic max-of-array (array &optional abs?)
  (loop for i across array maximize (if abs? (abs i) i)))

;; *** max-of-array-with-index
;;; max of an array and the index at which it first occurs
(defpublic max-of-array-with-index (array &optional abs?)
  (loop for i from 0 and el across array with max = 0 with max-i
     when (> (if abs? (abs el) el) max) do (setf max el max-i i)
     finally (return `(,max ,max-i))))

;; *** index-of-min-within
;;; get the index of the smallest element in an array,
;;; within a min and max index
(defpublic index-of-min-within (array &optional (from 0) (to (1- (length array))))
  (unless (arrayp array)
    (error "index-of-min-within-range didn't get an array as input: ~a" array))
  (loop for i from from to (min to (1- (length array)))
     with min-i = from with min = (aref array i)
     when (< (aref array i) min) do (setf min-i i min (aref array i))
     finally (return min-i)))

;; *** multiply-arrays
;;; copied from Common Lisp Music - for some reason they got rid of this...
(defpublic multiply-arrays (rdat window)
  (let ((len (min (length rdat) (length window))))
    (loop for i from 0 below len do
      (setf (aref rdat i) (* (aref rdat i) (aref window i))))
    rdat))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                [ STRUCTS ]                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; *** copy-and-update
(defpublic copy-and-update (struct &rest bindings)
  (loop
    with copy = (copy-structure struct)
    for (slot value) on bindings by #'cddr
    do (setf (slot-value copy slot) value)
    finally (return copy)))

;;;; EOF utilities.lsp
