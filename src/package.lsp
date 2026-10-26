;; * Package

(in-package :cl-user)

(defpackage :layers-utils
  (:use :common-lisp)
  (:nicknames :lyu)
  ;; most exports happen with #'defpublic, #'defpubmethod or inline.
  (:export
   :get-pathname-dir))

(defun lyu () (in-package :layers-utils))

(in-package :layers-utils)

;; use this instead of #'defun to automatically export the function
(defmacro defpublic (name args &body body)
  `(progn
     (defun ,name ,args ,@body)
     (export ',name :layers-utils)))

;; use this instead of #'defmethod to automatically export the method
(defmacro defpubmethod (name args &body body)
  `(progn
     (defmethod ,name ,args ,@body)
     (export ',name :layers-utils)))


;; EOF package.lsp
