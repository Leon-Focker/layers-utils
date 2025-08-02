;; ** thingy

(in-package :layers-utils)

;; *** thingy
;;; as simple as it gets
(defclass thingy ()
  ((id :accessor id :initarg :id :initform nil)
   (data :accessor data :initarg :data :initform nil)))

(defmethod print-object ((thy thingy) stream)
  (format stream "<Thingy ~a>" (id thy)))

(export '(id data) :layers-utils)

;;;; EOF thingy.lsp
