;; ** thingy

(in-package :layers-utils/light)

;; *** thingy
;;; as simple as it gets
(defclass thingy ()
  ((id :accessor id :initarg :id :initform nil)
   (data :accessor data :initarg :data :initform nil)))

(defmethod print-object ((thy thingy) stream)
  (format stream "<Thingy ~a>" (id thy)))

(export '(thingy id data) :layers-utils/light)

;;;; EOF thingy.lsp
