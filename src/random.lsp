;; ** random

(in-package :layers-utils)

;; *** random-number
;;; stores a fixed-seed pseudo random number and can generate a new one.
(defclass random-number (thingy)
  ((rng-type :accessor rng-type :initarg :rng-type
	     :initform #-wasm :pcg #+wasm :wihi)
   (rng :accessor rng :initarg :rng)
   (state :accessor state :initform 0 :initarg :state)
   (seed :accessor seed :initarg :seed :initform 5 :type number)))

(defmethod print-object ((rn random-number) stream)
  (format stream "<Random Number ~a, Type: ~a, State: ~a>"
	  (id rn) (rng-type rn) (state rn)))

;; *** random-number
;;; make an instance of random-number
(defpublic make-random-number
    (&optional (seed 5) (rng-type #-wasm :pcg #+wasm :wihi))
  (case rng-type
    #-wasm
    (:pcg
     (let ((rng (pcg::make-pcg :seed seed)))
       (make-instance 'random-number
		      :id (gensym "RANDOM_NUMBER")
		      :rng-type :pcg
		      :rng rng
		      :seed seed
		      :state (pcg::pcg-state rng)
		      :data (pcg::pcg-random rng 1.0))))
    (:wihi
     (let ((rng (wihi::make-wihi seed)))
       (make-instance 'random-number
		      :id (gensym "RANDOM_NUMBER")
		      :rng-type :wihi
		      :rng rng
		      :seed seed
		      :state (wihi::wihi-state rng)
		      :data (wihi::wihi-random rng 1.0))))
    (t (error "make-random-number: rng-type not recognized: ~a"
	      rng-type))))

;;; get current value of a random-number object
(defpubmethod get-number ((rn random-number))
  (data rn))

;;; get current state of a random-number object
(defpubmethod get-state ((rn random-number))
  (state rn))

;; *** get-next
;;; advance the rng
(defpubmethod get-next ((rn random-number))
  (case (rng-type rn)
    #-wasm
    (:pcg (setf (state rn)
		(pcg::pcg-state (rng rn))
		(data rn)
		(pcg::pcg-random (rng rn) 1.0)))
    (:wihi (setf (state rn)
		 (wihi::wihi-state (rng rn))
		 (data rn)
		 (wihi::wihi-random (rng rn) 1.0)))))

;; *** rewind
;;; go to the nth last number
(defpubmethod rewind ((rn random-number) n)
  (case (rng-type rn)
    #-wasm
    (:pcg (pcg::pcg-rewind (rng rn) (1+ n))
     (get-next rn))
    (t (error "RandomNumber:rewind: not supported for type ~a" (rng-type rn)))))

;; *** set-state
;;; set the random-state of the prng
(defpubmethod set-state ((rn random-number) state)
  (case (rng-type rn)
    #-wasm
    (:pcg 
     (prog1 (setf (pcg::pcg-state (rng rn)) state
		  (state rn) state)
       (rewind rn -1)))
    (:wihi
     (prog1 (setf (wihi::wihi-state (rng rn)) state
		  (state rn) state)
       (get-next rn)))))
    
;; (export '(random) :layers-utils)

;;;; EOF random.lsp
