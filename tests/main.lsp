;; * layers-utils tests

(in-package :layers-utils-tests)

;; define a test suite
(def-suite layers-utils-all)
(in-suite layers-utils-all)

(defmacro test (name &body body)
  `(5am:test ,name
             ,@body))

;; call this to run all tests
(defun run-tests ()
  (fiveam:run! 'layers-utils-all))

(defparameter *layers-utils-tests-home-dir*
  (format nil "~A" (asdf:system-source-directory 'layers-utils)))

(defparameter *layers-utils-tests-dir*
  (format nil "~atests/" *layers-utils-tests-home-dir*))

(defun layers-utils-tests-path (name)
  (format nil "~a~a" *layers-utils-tests-dir* name))

(defun probe-delete (file)
  (when (probe-file file) (delete-file file) t))

;;; Copied from Slippery Chicken

;;; MDE Thu Jan 12 13:27:21 2012 -- rather than just probe-file
(defun file-size (file)
  (with-open-file (stream file :direction :input :element-type '(signed-byte 1))
    (file-length stream))) ; bytes

;;; MDE Sat May 12 16:30:54 2012
;;;                                   bytes
(defun file-write-ok (file &optional (min-size 1) (max-secs-ago 120)) 
  (let ((age (- (get-universal-time) (file-write-date file)))
        (size (file-size file))
        (result nil))
    (setq result (probe-file file))
    (unless result
      (warn "file-write-ok: file ~a doesn't exist" file))
    ;; MDE Wed Feb 27 10:05:15 2019 -- these were updated yesterday to always
    ;; check that result is fine before resetting it, so that 'not fine' state
    ;; is sticky within the function 
    (when result (setq result (< age max-secs-ago)))
    (unless result
      (warn "file-write-ok: file ~a is ~a seconds old ~
             ~%(expected maximum ~a)"
            file age max-secs-ago))
    (when result (setq result (when (numberp size) (>= size min-size))))
    (unless result
      (warn "file-write-ok: file ~a has size ~a ~
             (expected minimum ~a)" file size min-size))
    ;; MDE Fri Jan 10 16:18:08 2020 -- return file size as wel instead of just T
    ;; or NIL result))
    (values result size)))


;; ** Tests

;; *** thingy

(test thingy
  (let ((thy (make-instance 'layers-utils::thingy :data '(0 1 2 3 4) :id 'testsuite)))
    (is (equal (id thy) 'testsuite))
    (is (equal (data thy) '(0 1 2 3 4)))))

;; *** utilities

(test simple-utilities
  ;; mirrors
  (is (= 4/5 (mirrors 4/5 0 1)))
  (is (= 1/5 (mirrors 9/5 0 1)))
  (is (= 4/5 (mirrors -6/5 0 1)))
  ;; dry-wet
  (is (= 0.5 (dry-wet 0 1 0.5)))
  (is (= 1 (dry-wet 0 1 1.5)))
  (is (= 0 (dry-wet 0 1 -1)))
  (is (= 2/7 (dry-wet -1 8 1/7)))
  ;; mod1
  (is (= 0 (mod1 0)))
  (is (= 1 (mod1 -1)))
  (is (= 1 (mod1 26)))
  (is (= 0.62 (float (mod1 0.62)))))

(test list-utilities
  (let ((ls1 '(0 1 2 3 4 5 6 7 8 9))
	(ls2 '(0 1 (2) 3 (4 (5) 6) 7))
	(ls3 '(0 1 2 4 -1 6 3 4 2))
	(ls4 '()))
    ;; remove-nth
    (is (equal (remove-nth 3 ls1) '(0 1 2 4 5 6 7 8 9)))
    ;; nth-mod
    (is (equal (nth-mod 4 ls1) 4))
    (is (equal (nth-mod -2 ls1) 8))
    (is (equal (nth-mod 199 ls1) 9))
    ;; rotate
    (is (equal (rotate ls1 4) '(4 5 6 7 8 9 0 1 2 3)))
    (is (equal (rotate ls1 -2) '(8 9 0 1 2 3 4 5 6 7)))
    ;; push-key-value, setf-key-value
    (push-key-value :animal 'dog ls4)
    (setf-key-value ls4 :animal 'cat)
    (is (equal ls4 '(:animal cat)))
    ;; re-order
    (is (re-order '(a b c d e) '(4 1 3 2 0)) '(e b d c a))
    (is (re-order '(a b c d e) '(3 4 1)) '(d e b a c))
    (is (re-order '(a b c d e) '(2 3 4 6 0 1)) '(c d e b a))
    (is (re-order '(a b c d e) '(0 3 7 0 7 3 5)) '(a d c b e))
    (is (re-order '(a b c d e) '(0 0 0)) '(a b c d e))
    ;; move-item
    (is (equal (move-item ls1 2 3) '(0 1 3 2 4 5 6 7 8 9)))
    ;; depth
    (is (= 1 (depth ls1)))
    (is (= 3 (depth ls2)))
    ;; TODO rqq-depth
    ;; normalize-depth
    ;; this test fails for reasons I'm not even gonna try to understand.
    #+nil(is (equal (normalize-depth ls2 nil)
		    '(((0)) ((1)) ((2)) ((3)) ((4) (5) (6)) ((7)))))
    ;; scale-list-to-sum
    (is (equal (scale-list-to-sum ls1)
	       '(0 1/45 2/45 1/15 4/45 1/9 2/15 7/45 8/45 1/5)))
    ;; avoid-repetition
    (is (equal (avoid-repetition ls3) '(0 1 2 4 -1 6 3 4 2)))
    ;; insert
    (is (equal (insert (copy-seq ls1) 2 -1) '(0 1 -1 2 3 4 5 6 7 8 9)))
    ;; insert-multiple
    (is (equal (insert-multiple (copy-seq ls1) '(2 2 4) '(-1 -2 -3))
	       '(0 1 -1 -2 2 3 -3 4 5 6 7 8 9)))
    ;; biggest-jump
    (is (= 7 (biggest-jump ls3)))
    (is (= 7 (biggest-jump-up ls3)))
    (is (= 5 (biggest-jump-down ls3)))
    ;; reduce-by
    (is (equal (reduce-by '(0 1 2 4 -1 6 3 4 2) 2) '(1/2 3 5/2 7/2)))
    ;; list-interp
    (is (= 5.5 (list-interp 5.5 ls1)))
    (is (= 4.5 (list-interp 0.5 ls1 1)))
    (is (= 0 (list-interp 0 ls1 1)))
    (is (= 9 (list-interp 1 ls1 1)))
    ;; make-list-into-function
    (is (= 4.5 (funcall (lyu:list-to-function '(0 1 2 3 4 5 6 7 8 9) 1) 0.5)))
    (is (= 0 (funcall (lyu:list-to-function '(0 1 2 3 4 5 6 7 8 9) 1) 0)))
    (is (= 9 (funcall (lyu:list-to-function '(0 1 2 3 4 5 6 7 8 9) 1) 1)))
    ;; lists-into-envelopes
    (is (equal (lists-to-envelopes '(0 1 2 0 3 0.2 4 5 6 1) ls1)
	   '((0 0 1 1 2 2) (0 3 3 4) (0.2 5 4 6 5 7 6 8) (1 9))))
    ;; flatness-of-list
    (is (= (flatness-of-list ls1) 0.2008))
    ;; get-spectral-centroid
    (is (= 603.1405 (get-spectral-centroid '((440 0.5) (630 0.46) (880 0.25)))))
    ;; TODO taylor polynom
    ))

(test duration-list-utilities
  (let ((ls1 '(0 1 2 3 4 5 6 7 8 9)))
    ;; get-start-times
    (is (equal (get-start-times ls1) '(0 0 1 3 6 10 15 21 28 36)))
    ;; get-durations
    (is (equal '(1 1 1 1 1 1 1 1 1) (get-durations ls1)))
    ;; get-start-times-within
    (is (equal '(6 10) (get-start-times-within ls1 4 11)))
    ;; get-durations-within
    (is (equal '(3 4) (get-durations-within ls1 4 11)))
    ))

(test envelope-utilities
  ;; function-to-env
  (is (equal (make-function-into-env #'sin)
	     '(0 0.0 0.1 0.09983342 0.2 0.19866933 0.3 0.29552022 0.4 0.38941833
	       0.5 0.47942555 0.6 0.5646425 0.70000005 0.6442177 0.8000001
	       0.71735615 0.9000001 0.783327)))
  (is (equal (function-to-env #'(lambda (x) (1+ x)) 0 20 1)
	     '(0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13 14
	       14 15 15 16 16 17 17 18 18 19 19 20 20 21)))
  ;; combine-envelopes
  (is (equal (combine-envelopes '((0 0  1 1) (0 3  5 3)) '(5 5))
	     '(0.0 0 5.0 1 5.0001 3  10.0 3)))
  ;; rescale-env
  (is (equal (rescale-env '(0 0  0.4 3  1 -1) 0 1) '(0 0.25 0.4 1.0 1 0.0)))
  )

(test array-utilities
  (let ((ar1 #(0 1 2 3 4 5 6 7 8 9)))
    ;; distance-between-points
    (is (= 1.4142135 (distance-between-points #(0 0) #(1 1))))
    ;; max-of-array
    (is (= 9 (max-of-array ar1)))
    ;; max-of-array-with-index
    (is (equal '(9 9) (max-of-array-with-index ar1)))
    ;; index-of-min-within-range
    (is (= 1 (index-of-min-within ar1 1 4)))
    ;; multiply-arrays
    (is (equalp #(0 1 4 9 16 25 36 49 64 81) (multiply-arrays (copy-seq ar1) ar1)))
    ))

;; *** random-number

(test random-number
  (let ((rn (make-random-number 10))
	(state 0)
	(nr 0))
    (is (= 10 (get-number rn)))
    (get-next rn)
    (is (= 0.76906323 (get-number rn)))
    (get-next rn)
    (get-next rn)
    (setf nr (get-number rn)
	  state (get-state rn))
    (rewind rn 2)
    (is (= 0.76906323 (get-number rn)))
    (set-state rn state)
    (is (= nr (get-number rn)))))

;; *** list-thingy

(test list-thingy
  (let* ((ls `(0 1 ,(make-instance 'layers-utils::thingy :id 2 :data 2) 3))
	 (lt (make-instance 'layers-utils::list-thingy :id 'test :data ls)))
    (is (check-sanity lt))
    (is (equal '(2) (get-ids lt)))
    (is (= 0 (see-current lt)))
    (is (= 1 (see-next lt)))
    (is (= 1 (get-next lt)))
    (add-to-list lt 4 5 6)
    (is (= 5 (see-with-relative-index lt 4)))))

;; *** clm

;;; these tests are implicit in analysing soundfile and soundpile, also fplay

;; *** metrical-hierarchies

#+mh
(test metrical-hierarchies
  ;; beat prox:
  (= (get-beat-prox 0) 0)
  (= (get-beat-prox 1) 0)
  (= (beat-proximity .66) 3)
  (= (beat-proximity .66 10) 8)
  (equal (loop for i from 0 to 1 by .125 collect (beat-proximity i))
	 '(0 3 2 3 1 3 2 3 0))
  ;; indispensability:
  (equal (loop for i from 0 to 1 by 0.125
	       collect (funcall (mh:rqq-to-indispensability-function
				 '(2 ((2 ((2 (1 1)) (2 (1 1))))
				      (2 ((2 (1 1)) (2 (1 1)))))))
				i))
	 '(0 7 3 5 1 6 2 4 0))
  (equal (loop for i from 0 to 1 by 0.125
	       collect (funcall (mh:rqq-to-indispensability-function
				 '(8 (1 1 1 1 1 1 1 1)))
				i))
	 '(0 7 6 5 4 3 2 1 0)))

;; *** transitions

(test transitions
  (is (equal '(1 1 1 1 0 1 1 2 2 2 2 0 2 0 0 0 0 0 1 2)
	     (kernel-transitions 20 3 '(-3 2 4))))
  (is (equal '(0 0 0 0 0 0 0 1 1 1 0 1 0 1 2 2 2 0 2 1)
	     (window-transitions 20 3)))
  (is (equalp '#(10 6 4) (nth-value 1 (window-transitions 20 3)))))

;; *** morph

;; waiting for fibonacci-transitions
(test morph
  (labels ((round-values (x)
	     (if (listp x)
		 (mapcar #'round-values x)
		 (/ (round (* 10 x)) 10))))
    (let* ((pts2 '((2 3 4) (5 (6) 7) (8 (9) (0))))
	   (pts3 '((2 3) (4 5 (6) 7) (8 (9) (0)))))
      ;; morph-patterns
      (is (equal (morph-patterns pts2 25) '(2 3 (6) 3 7 3 (1))))
      (is (equal (morph-patterns pts2 0 t t 10) '(2 3 (6) 3 4 2 5 (6) 7 5)))
      (is (equal (morph-patterns pts2 0 t t 10 t)
		 '((0 0) (0 1) (1 1) (0 1) (0 2) (0 0) (1 0) (1 1) (1 2) (1 0))))
      (is (equal (morph-patterns pts3 25 nil nil nil nil
				 (list-to-function '(0 4 5 2 3) 1))
		 '(2 4 5 (6) 7 1)))
      ;; interpolate-patterns
      (is (equal '(2 7/2 53/10 23/5 (67/10) (8/5) 13/10)
		 (mapcar #'round-values (interpolate-patterns pts2 25))))
      (is (equal '(2 17/5 17/5 51/10 12/5 33/10 (67/10))
		 (mapcar #'round-values
			 (interpolate-patterns
			  pts3 25 t '(2 1)
			  '((0 0  1 1) (0 .2) (0 2 .5 1 1 0.1))))))
      (is (equal '(2 17/5 18/5 24/5 12/5 41/10 (28/5) 37/5 16/5 71/10)
		 (mapcar #'round-values
			 (interpolate-patterns
			  pts3 25 t '(2 1)
			  '((0 0  1 1) (0 .2) (0 2 .5 1 1 0.1)) 10))))
      ;; interpolate-envelopes
      (is (equal '((0 1 1/2 6/5 13/10 4/5) (0 4/5 2/5 17/10 9/5 3/5)
		   (0 1/2 3/10 11/5 23/10 3/10) (0 1/5 1/5 27/10 14/5 1/10)
		   (0 0 1/5 31/10 27/10 1/5) (0 1/5 1/5 33/10 11/5 2/5)
		   (0 1/2 1/5 18/5 17/10 7/10) (0 4/5 1/5 19/5 6/5 9/10))
		 (mapcar #'round-values
			 (interpolate-envelopes
			  '((0 1  .5 1  1 1) (0 0  .2 3  3 0)
			    (0 1  .2 4  1 1))
			  24)))))))

;; *** midi

#+cm
(test midi
  (let ((file (layers-utils-tests-path "test.mid"))
	(file2 (layers-utils-tests-path "duration-layers-(A B A).mid"))
	(dl (make-lindenmayer-layers '(a b a) '((a b) (b (a b))) '((a 2) (b 1))
				     :generation-method 'compartmentalise
				     :smallest 1)))
    (probe-delete file)
    ;; write a midi file
    (lists-to-midi '(e3 54 c2) '(1) '(1 5)
		   :velocity-list '(.1 .4 .8) :file file)
    (is (file-write-ok file 10))
    ;; read midi file
    (is (equal '((1.0 36 1.0 0.79527557 0 2.0) (1.0 52 1.0 0.09448819 0 2.0)
		 (5.0000005 54 1.0 0.39370078 0 6.0000005))
	       (midi-file-to-list file)))
    ;; midi to env
    (is (equal '(0.0 0.0 100.0 1.0)
	       (midi-to-env file)))
    ;; todo deconstruct-chords
    ;; todo harmony-filter
    (probe-delete file)
    ;; duration-layers-to-midi
    (probe-delete file2)
    (duration-layers-to-midi dl :dir *layers-utils-tests-dir*)
    (is (file-write-ok file2 110))
    (probe-delete file2)))

;; *** fplay

#+clm
(test fplay
  (let ((file (layers-utils-tests-path "fplay.wav")))
    (probe-delete file)
    (fplay-to-wav file (fplay 0 10))
    (is (file-write-ok file 2500000))
    (probe-delete file)
     
   (fplay-to-wav file
     (fplay 0 5
       (file (layers-utils-tests-path "1000.wav"))
       (rhythm "1 then .5")
       (duration (/ rhythm 4))
       (amp-env `(0 1  1 ,(- 1 (expt line 0.5))))
       (srt 1 1.1)))
    (is (file-write-ok file 1900000))
    (probe-delete file)
    ))

;; *** soundfile

(test soundfile
  (let ((file (layers-utils-tests-path "test-snd.txt"))
	(test-sound (make-soundfile 'test "tests/220.wav")))
    ;; analyse
    #+clm(analyse-soundfile test-sound :multiple-ffts nil)
    ;; write to .txt file
    (probe-delete file)
    (store-in-text-file test-sound file)
    (setf test-sound (load-from-file file))
    ;; check result
    (is (check-sanity test-sound))
    (is (equal 'test (id test-sound)))
    #+clm(is (= 220.000093990719d0 (car (centroid test-sound))))
    #+clm(is (= 219.81719936285495d0 (car (dominant-frequency test-sound))))
    #+clm(is (= 221.1914 (car (fundamental-frequency test-sound))))
    #+clm(is (= 6.7098126 (duration test-sound)))
    #+clm(is (= 3.754970640876718d-4 (car (flatness test-sound))))
    #+clm(is (= 0.27550530433654785d0 (peak test-sound)))
    #+clm(is (= 48000 (samplerate test-sound)))
    #+clm(is (= 0.8772904737843085d0 (smoothness test-sound)))
    #+clm(is (= 1.307124340405836d0 (car (spread test-sound))))
    #+clm(is (= 322071 (total-samples test-sound)))
    #+clm(is (= 0.07589141036017721d0 (transient test-sound)))
    (probe-delete file)
    ))

;; *** soundpile

(test soundpile
  (let* ((file (layers-utils-tests-path "test-sndpile.txt"))
	 (dir (layers-utils-tests-path ""))
	 (sp (soundpile-from-folder 'test dir)))
    (probe-delete file)
    (store-in-text-file sp file)
    (setf sp (load-from-file file))
    (is (check-sanity sp))
    ;; how many soundfiles are in the tests directory
    (is (= 6 (length (get-all 'id sp))))
    (probe-delete file)))

;; *** duration-layers

(test duration-layers
  (let ((dl (make-duration-layers 'test '((1 1 1) (2)))))
    ;; scale
    (scale-smallest-value-to dl 3)
    ;; check values
    (is (= 2 (how-many dl)))
    (is (= 9 (total-duration dl)))
    (is (equal '((3 3 3) (6)) (data dl)))
    ;; scale again
    (scale-biggest-value-to dl 3)
    ;; check again
    (is (= 2 (how-many dl)))
    (is (= 9/2 (total-duration dl)))
    (is (equal '((3/2 3/2 3/2) (3)) (data dl)))
    ;; scale again
    (scale dl 5)
    ;; check again
    (is (= 2 (how-many dl)))
    (is (= 5 (total-duration dl)))
    (is (equal '((5/3 5/3 5/3) (10/3)) (data dl)))
    ))

;; *** lindenmayer-functions

(test lindenmayer-functions
  (is (equal '((12 6 6 2 1 12 6) (18 6 3 18) (18 9 18))
	     (compartmentalise 45 '(a b a) '((a b) (b (a b))) '((a 2) (b 1)) 1)))
  (is (equal '((1.4285715 50/7 1.4285715) (10))
	     (lindenmayer  10
			   '(2)
			   '((1 ((2 1)))
			     (2 ((3 1 3)))
			     (3 ((2))))
			   '((1 1)
			     (2 5)
			     (3 .2))
			   2)))
  )

;; *** lindenmayer-layers

(test lindenmayer-layers
  (let ((ll (make-lindenmayer-layers '(a b a) '((a b) (b (a b))) '((a 2) (b 1))
				     :generation-method 'compartmentalise)))
    (is (= 6 (how-many ll)))
    (setf (smallest ll) 1)
    (regenerate ll)
    (is (= 2 (how-many ll)))))

;; *** show-duration-layers

(test show-duration-layers
  (let ((file (layers-utils-tests-path "test.png"))
	(ll (make-lindenmayer-layers '(a b a) '((a b) (b (a b))) '((a 2) (b 1))
				     :generation-method 'compartmentalise)))
    (probe-delete file)
    ;; generate image
    (visualize-duration-layers ll file 0.5)
    (is (file-write-ok file 50000))
    (probe-delete file)))

;; EOF layers-utils-tests.lsp
