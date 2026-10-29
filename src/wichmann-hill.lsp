;; ** Wichmann-Hill RNG

;;; A simple RNG for ECL/WASM (wasm32)
;;; Wichmann-Hill generator (B. A. Wichmann and I. D. Hill, 1982): three small
;;; LCGs whose normalized outputs are summed modulo 1. All intermediate
;;; integers stay far below 2^29 (ECL wasm32 fixnum range).
;;; Period is about 7e12. Seeds should be below 2^29.

(defpackage :wichmann-hill
  (:use :cl)
  (:nicknames :wihi)
  (:export
   #:make-rng
   #:rng-state
   #:rng-random))

(in-package :wichmann-hill)

(defstruct (wihi (:constructor actually-make-wihi))
  (x 1)
  (y 1)
  (z 1))

(defun wihi-next (wihi)
  "Advance the generator and return a double-float in [0,1)."
  (setf (wihi-x wihi) (mod (* 171 (wihi-x wihi)) 30269)
        (wihi-y wihi) (mod (* 172 (wihi-y wihi)) 30307)
        (wihi-z wihi) (mod (* 170 (wihi-z wihi)) 30323))
  (mod (+ (/ (wihi-x wihi) 30269d0)
          (/ (wihi-y wihi) 30307d0)
          (/ (wihi-z wihi) 30323d0))
       1d0))

(defun make-wihi (seed)
  "Create a generator from an integer seed."
  (let* ((s (mod seed 1000003))
         (wihi (actually-make-wihi
		:x (1+ (mod s 30268))
		:y (1+ (mod (+ (* 3 s) 17) 30306))
		:z (1+ (mod (+ (* 5 s) 101) 30322)))))
    ;; warm up
    (dotimes (i 20) (wihi-next wihi))
    wihi))

(defun wihi-state (wihi)
  "Return the state as a list (x y z)."
  (list (wihi-x wihi) (wihi-y wihi) (wihi-z wihi)))

(defun (setf wihi-state) (state wihi)
  "Set the state from a list (x y z), as returned by `wihi-state`."
  (destructuring-bind (x y z) state
    (setf (wihi-x wihi) x
          (wihi-y wihi) y
          (wihi-z wihi) z))
  state)

(defun wihi-random (wihi bound &optional max inclusive?)
  "Random number in the interval.
If `max` is omitted the interval is [0, bound), otherwise [bound, max)
(or [bound, max] for integers if `inclusive?`).
If `bound` or `max` is a float, the result is a single-float, else an integer."
  (let* ((float? (or (floatp bound) (floatp max)))
         (lo (if (null max) 0 bound))
         (hi (if (null max) bound max))
         (u (wihi-next wihi)))
    (if float?
        (progn
          (assert (< lo hi) () "Invalid interval: [~S, ~S)" lo hi)
          (float (+ lo (* (- hi lo) u)) 1.0))
        (let ((hi (if inclusive? (1+ hi) hi)))
          (assert (< lo hi) () "Invalid interval: [~S, ~S)" lo hi)
          (+ lo (floor (* u (- hi lo))))))))

;; EOF wichmann-hill
