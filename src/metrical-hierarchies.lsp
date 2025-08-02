;; ** metrical hierarchy

;; *** some aliases
;; this is mainly for backwards-compatability with older projects
;; (altough some edge cases might behave differently now)
(setf (symbol-function 'get-beat-prox) #'mh:get-metrical-depth-simple)
(setf (symbol-function 'beat-proximity) #'mh:get-metrical-depth-simple)
(setf (symbol-function 'rqq-to-indispensability) #'mh:rqq-to-indispensability-list)
(export '(get-beat-prox beat-proximity rqq-to-indispensability) :layers-utils)

;; *** apply-indispensability
;;; returns the amplitude values for a list of rhythms according to an
;;; indispensability function:
;;; beats: a list of durations. These durations are kind of like bars,
;;;  in that the indispensability-function is applied to each of these durations
;;;  if there is more rhythms than bars to be filled they are ignored.
;;; rhythms: a list of durations, for which we want to get a list of
;;;  amplitude-values. If there are more bars to be filled than available
;;;  rhythms, they are ignored.
;;; indispensability-function: a function that returns an amplitude value
;;;  for an input value (x) between 0 and 1.
;;;  See #'rqq-to-indispensability-function.
;;; returns a list of numbers.
(defpublic apply-indispensability (beats rhythms indispensability-function)
  (let* ((duration (apply #'+ beats)))
    (loop for rthm in rhythms
	  for sum = 0 then (+ sum rthm)
	  until (>= sum duration)
	  with beat-sum = 0
	  with last-beat-i = 0
	  for progress = (rationalize (/ sum duration))
	  for current-beat-i = (decider progress beats)
	  for current-beat = (nth current-beat-i beats)
	  with position = 0
	  do (unless (= current-beat-i last-beat-i)
	       (incf beat-sum (nth last-beat-i beats))
	       (setf last-beat-i current-beat-i))
	     ;; position in current beat:
	     (setf position (- sum beat-sum))
	  collect (funcall indispensability-function
			   (rationalize (/ position current-beat))))))

;; EOF metrical-hierarchies.lsp
