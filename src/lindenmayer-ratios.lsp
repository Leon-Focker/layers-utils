;; ** lindenmayer-ratios

(in-package :layers-utils)

;; *** scale-nested-to
;;; scale all values in the recursive structure to wanted length.
;;; returns flattened list
(defun scale-nested-to (structure target-sum)
  (scale-list-to-sum (flatten structure) target-sum))

;; *** lindenmayer-ratios-check-arguments
(defun lindenmayer-ratios-check-arguments
    (total-duration seed rules ratios smallest)
  (unless (numberp smallest)
    (error "lindenmayer-ratios: smallest should be a number but is ~a"
	   smallest))
  (unless (numberp total-duration)
    (error "lindenmayer-ratios: total-duration should be a number! Got: ~a"
	   total-duration))
  ;; check that rules are well formed
  (unless (and (listp rules)
	       (every #'listp rules))
    (error "lindenmayer-ratios: rules should be a list of lists but is: ~&~a"
	   rules))
  ;; check that ratios are well formed
  (unless (and (listp ratios)
	       (every #'listp ratios))
    (error "lindenmayer-ratios: ratios should be a list of lists but is: ~&~a"
	   ratios))
  ;; check that ratios are numbers:
  (unless (every #'(lambda (x) (numberp (second x))) ratios)
    (error "lindenmayer-ratios: found a ratio that's not a number in ~a"
	   ratios))
  ;; check that rules and ratios contain the same keys
  ;; check that rules a lists of other keys
  (let ((keys (loop for r in rules collect (first r)))
	(keys-ratios (loop for r in ratios collect (first r))))
    
    (unless (loop for r in rules
		  always (every #'(lambda (x) (find x keys)) (second r)))
      (error "lindenmayer-ratios: found a rule that replaces a key with ~
              something that's not a key! ~&rules: ~a" rules))
    (unless (loop for e in seed always (find e keys))
      (error "lindenmayer-ratios: rules do not contain every element of seed ~a"
	     seed))
    (unless (and (loop for e in keys always (find e keys-ratios))
		 (loop for e in keys-ratios always (find e keys)))
      (error "lindenmayer-ratios: rules and ratios should contain the same set ~
              of keys!"))))

;; *** wellformed-rules
(defun wellformed-rules (rules)
  (loop for rule in rules
	collect (list (first rule)
		      (if (listp (second rule))
			  (if (listp (caadr rule))
			      (caadr rule)
			      (second rule))
			  (list (second rule))))))

;; *** lindenmayer
;;; Generate self-similar lists with the principals of lindenmayer systems.
;;; The rules specify how each key in an axiom is replaced with other keys for
;;; each generation. However, each key is also associated with a
;;; ratio/proportion. A list of lists of these proportions is the result of this
;;; function. When a key is replaced with two or more other keys, their
;;; associated proportions are scaled to sum to the proportion of their parent
;;; key. For example: 
;;; (compartmentalise 45 '(a b a) '((a b) (b (a b))) '((a 2) (b 1)) 1)
;;; => ((12 6 6 2 1 12 6) (18 6 3 18) (18 9 18))
;;;
;;; Let's compare a "normal" lindenmayer list (right) with our ratios:
;;;
;;;       (  18   |   9       |   18  )  == (  a   |   b       |   a  )
;;;       (  18   | 6 |   3   |   18  )  == (  b   | a |   b   |   b  )
;;;       (12 | 6 | 6 | 2 | 1 | 12 | 6)  == (a | b | b | a | b | a | b)
;;;
;;; What's special now is, that the three groups in the third generation that
;;; stem from the three groups in the first generation retained their duration:
;;; 12 + 6 = 18,   6 + 2 + 1 = 9,   12 + 6 = 18.
;;;
;;; Note that only the 'comparmentalise type exhibits this behavior. When using
;;; 'lindenmayer, the ratios are scaled at an earlier time, depending on the
;;; keys they replace. For this reason, keys can only be numbers when using this
;;; type. While 'compartmentalise is maybe more beautiful, the 'lindenmayer type
;;; generates similar lists but varies the proportions in potentially
;;; interesting ways as soon as not all keys are the same number as their ratio.
;;; When using the resulting lists as durations for rhythms, this would mean
;;; that different instruments using different generations do not always line up
;;; together, which could be desirable.
;;;
;;; You could use #'visualize-duration-layers to visualize the differences:
#|
(let ((ll1 (make-lindenmayer-layers '(1 2 1) '((1 2) (2 (1 2))) '((1 2) (2 1))
				    :generation-method 'lindenmayer))
      (ll2 (make-lindenmayer-layers '(a b a) '((a b) (b (a b))) '((a 2) (b 1))
				    :generation-method 'compartmentalise)))
  ;; generate image
  (visualize-duration-layers ll1 (layers-utils-path "lind.png") 0.5)
  (visualize-duration-layers ll2 (layers-utils-path "comp.png") 0.5))
|#
;;;
;;; Arguments:
;;; - type: Either 'lindenmayer or 'compartmentalise. 
;;; - seed: A key or a list of keys, which are the starting axiom.
;;; - rules: A list of sublists. Each sublist contains a key and a rule on how
;;;   to replace that key. This rule can be another key or a list of other keys.
;;; - ratios: A list of sublists, one for each key specified in rules. Each
;;;   sublist should consist of the key, followed by a number which represents
;;;   the proportion that key will have in relation to other keys.
;;; - smallest: As soon as a generation contains a value smaller than this, stop
;;;   generating.
(defun lindenmayer-ratios (type total-duration seed rules ratios
			   &optional (smallest 0.003))
  ;; sanity checks
  (setf seed (force-list seed))
  ;; make sure the second part of rules is a list without sublists
  (setf rules (wellformed-rules rules))
  (lindenmayer-ratios-check-arguments total-duration seed rules ratios smallest)
  ;; the fun part
  (let ((proportions 1)	; the proportion of a list in seed to other lists in it
	(result '()))   ; all generations are collected as sublists in result
    ;; This is where #'lindenmayer and #'compartmentalise differ:
    ;; The former is actually a faulty implementation of the latter.
    (labels ((lind (seed proportions)
	       (unless (every #'numberp seed)
		 (error "lindenmayer-ratios: for type 'lindenmayer, all keys ~
                         need to be numbers! Maybe use type 'compartmentalise"))
	       (let ((sum (apply #'+ seed)))
		 ;; ratios are scaled according to keys before replacing them
		 (loop for e in seed collect (* (cadr (assoc e ratios))
						(/ proportions sum)))))
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	     (comp (seed proportions)
	       (let* ((ratios (mapcar (lambda (e) (rationalize
					      (cadr (assoc e ratios))))
				      seed))
		      (sum (apply #'+ ratios)))
		 ;; ratios are scaled after replacing the keys
		 (loop for e in ratios
		       collect (* e (/ (rationalize proportions) sum)))))
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	     (lindenmayer-ratios-aux (seed proportions)
	       ;; if the first element of seed is an atom, all elements of
	       ;; seed and proportions should be atoms!
	       (if (atom (car seed))
		   ;; when all elements are atoms, replace them
		   (values
		    ;; replace all keys with associated values (from rules)
		    (loop for e in seed collect (cadr (assoc e rules)))
		    ;; replace all keys with associated ratios and scale them
		    ;; Call the right aux-function according to 'type
		    (case type
		      (lindenmayer (lind seed proportions))
		      ((compartmentalise compartmentalize)
		       (comp seed proportions))
		      (t (error "lindenmayers-ratios: unknown type: ~a" type))))
		   ;; if seed and proportions are still nested, loop through
		   ;; them and call lindenmayer-aux on their elements
		   (loop for e in seed and prop in proportions
			 collect (multiple-value-list
				  (lindenmayer-ratios-aux e prop))
			   into results
			 finally (return (values
					  (mapcar #'first results)
					  (mapcar #'second results)))))))
      ;; with each loop, one sublist (one generation) is generated
      ;; loop for a maximum of 20 iterations
      (loop for i from 0 to 20
	    do (setf (values seed proportions)
		     (lindenmayer-ratios-aux seed proportions))
	       ;; re-scaling is only necessary for the lindenmayer-type.
	       (push (scale-nested-to proportions total-duration) result)
	       ;; stop when a value smaller than 'smallest was produced
	    while (> (apply #'min (car result)) smallest)
	    finally (return result)))))

(defpublic lindenmayer (total-duration seed rules ratios
				     &optional (smallest 0.003))
  (lindenmayer-ratios 'lindenmayer total-duration seed rules ratios smallest))

(defpublic compartmentalise (total-duration seed rules ratios
					  &optional (smallest 0.003))
  (lindenmayer-ratios 'compartmentalise total-duration seed rules ratios
		      smallest))

;;; similar to lindenmayer but 

;;; example:
#|
(lindenmayer 60
	    '(2)
	    '((1 ((2 1)))
	      (2 ((3 1 3)))
	      (3 ((2))))
	    '((1 1)
	      (2 5)
(3 .2)))
|#

;;;; EOF lindenmayer-functions.lsp
