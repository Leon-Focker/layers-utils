;; * Layers-utils

;;; This file loads an extendet set of files with different dependencies, but
;;; only if the relevant packages are found.

(in-package :layers-utils)

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
	     "clm.lsp"
	     (:mh "metrical-hierarchies.lsp")
	     (:cm "midi.lsp")
	     (:clm "fplay.lsp")
	     "soundfile.lsp"
	     "soundpile.lsp"
	     "show-duration-layers.lsp"
	     ))
    (load-layers-utils-src-file file)))

(load-all)

(format t "~&finished loading layers-utils!")

;; ** Tests

;;; Call tests with (asdf:test-system 'layers-utils)

;;;; EOF all.lsp
