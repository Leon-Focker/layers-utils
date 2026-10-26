;; * Layers-utils/light

;; ** Package

(in-package :cl-user)

(defpackage :layers-utils
  (:use :common-lisp)
  (:nicknames :lyu)
  ;; most exports happen with #'defpublic, #'defpubmethod or inline.
  (:export
   :get-pathname-dir))

(defun lyu () (in-package :layers-utils))

(in-package :cl) 

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


;; ** Paths
(defparameter *layers-utils-home-dir*
  (format nil "~A" (asdf:system-source-directory 'layers-utils)))

(defparameter *layers-utils-src-dir*
  (format nil "~asrc/" *layers-utils-home-dir*))

(defun load-layers-utils-src-file (file)
  (when (stringp file)
    (load (format nil "~a~a" *layers-utils-src-dir* file)))
  (when (and (listp file)
	     (find-package (first file)))
    (load (format nil "~a~a" *layers-utils-src-dir* (second file)))))

(defun layers-utils-path (name)
  (format nil "~a~a" *layers-utils-home-dir* name))

(defun get-pathname-dir (path &optional file)
  (format nil "~a~a"
          (namestring
           (make-pathname :directory (pathname-directory path)
                          :device (pathname-device path)))
          (if file file "")))


;; ** Dependencies

;; *** metrical-hierarchies
;;; (https://github.com/Leon-Focker/metrical-hierarchies)

;; ** Load

(defun load-all-light ()
  (dolist (file
	   '("thingy.lsp"
	     "random.lsp"
	     "slippery-utils.lsp"
	     "utilities.lsp"
	     "list-thingy.lsp"
	     "transitions.lsp"
	     "morph.lsp"
	     "duration-layers.lsp"
	     "lindenmayer-ratios.lsp"
	     "lindenmayer-layers.lsp"
	     ))
    (load-layers-utils-src-file file)))

(load-all-light)

(format t "~&finished loading layers-utils/light!")

;;;; EOF all-light.lsp
