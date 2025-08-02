;; * Layers-utils

;; ** Package

(defpackage :layers-utils
  (:use :common-lisp)
  (:nicknames :lyu)
  (:export
   :get-pathname-dir))

(defun lyu () (in-package :layers-utils))

(in-package :layers-utils)


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

;;; These are not on quicklisp, which is why we handle them outside of the
;;; .asdf file. Put them into your quicklisp/local-projects folder to load them
;;; automatically.

;; *** CMN
;;; needed for :cm
;;; (https://ccrma.stanford.edu/software/cmn/)

;; *** CM
;;; needed for some slippery-utils, midi.lsp
;;; (https://github.com/ormf/cm)

;; *** CLM
;;; needed in clm.lsp, fplay, to analyse soundfiles
;;; (https://ccrma.stanford.edu/software/clm/)

;; *** metrical-hierarchies
;;; (https://github.com/Leon-Focker/metrical-hierarchies)

;; ** Load

;;; load all .lsp files
(defun load-all ()
  (dolist (file
	   '("dependencies.lsp"
	     "thingy.lsp"
	     "slippery-utils.lsp"
	     "utilities.lsp"
	     "clm.lsp"
	     "random.lsp"
	     "list-thingy.lsp"
	     (:mh "metrical-hierarchies.lsp")
	     "transitions.lsp"
	     "morph.lsp"
	     (:cm "midi.lsp")
	     (:clm "fplay.lsp")
	     "soundfile.lsp"
	     "soundpile.lsp"
	     "duration-layers.lsp"
	     "lindenmayer-ratios.lsp"
	     "lindenmayer-layers.lsp"
	     "show-duration-layers.lsp"
	     ))
    (load-layers-utils-src-file file)))

(load-all)

(format t "~&finished loading layers-utils!")

;; ** Tests

;;; Call tests with (asdf:test-system 'layers-utils)

;;;; EOF all.lsp
