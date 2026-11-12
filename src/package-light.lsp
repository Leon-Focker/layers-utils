;; * Package

(in-package :cl-user)

(defpackage :layers-utils/light
  (:use :common-lisp)
  (:nicknames :lyul)
  ;; most exports happen with #'defpublic, #'defpubmethod or inline.
  (:export
   :get-pathname-dir))

(in-package :layers-utils/light)

;; use this instead of #'defun to automatically export the function
(defmacro defpublic (name args &body body)
  `(progn
     (defun ,name ,args ,@body)
     (export ',name (package-name *package*))))

;; use this instead of #'defmethod to automatically export the method
(defmacro defpubmethod (name args &body body)
  `(progn
     (defmethod ,name ,args ,@body)
     (export ',name (package-name *package*))))


;; EOF package-light.lsp
