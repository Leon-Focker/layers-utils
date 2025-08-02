;; ** clm

(in-package :layers-utils)

#+clm
(progn
  (format t "~&~&Loading files related to CLM...")
  
  ;; *** soundfile-duration
  ;;; get the duration of a soundfile in seconds
  (defpublic soundfile-duration (path)
    (handler-case (clm::sound-duration path)
      (error (c)
	(error "soundfile-duration: ~a" c))))
  
  ;; *** soundfile-framples
  ;;; get the number of framples (number of samples / channels)
  (defpublic soundfile-framples (path)
    (handler-case (clm::sound-framples path)
      (error (c)
	(error "soundfile-framples: ~a" c))))

  ;; *** soundfile-samplerate
  ;;; samplerate of the soundfile
  (defpublic soundfile-samplerate (path)
    (handler-case (clm::sound-srate path)
      (error (c)
	(error "soundfile-samplerate: ~a" c))))

  ;;; handling warnings quietly to keep repl clean
  (defun quiet-warning-handler (c)
    (let ((r (find-restart 'muffle-warning c)))
      (when r 
	(invoke-restart r))))

  ;; clm makes a lot of annoying warnings :c
  (handler-bind ((warning
		   #'quiet-warning-handler))
    (load (compile-file (format nil "~a~a" *layers-utils-src-dir* "analysis.lsp")))
    ;; Michael Edwards samp1 instrument, but modified so you can select the
    ;; input channel. It would also be possible to use samp5 (slippery chicken).
    (load (compile-file (format nil "~a~a" *layers-utils-src-dir* "samp0.ins")))
    ;; nrev from CLM
    (load (compile-file (format nil "~a~a" *layers-utils-src-dir* "nrev.ins")))
    ;; jc-reverb from CLM
    (load (compile-file (format nil "~a~a" *layers-utils-src-dir* "jcrev.ins"))))

  ;; import these to use in layers-utils
  (import '(clm::with-sound
	    clm::with-mix
	    clm::sound-let
	    ;;clm::mix
	    clm::*CLM-MIX-CALLS*
	    clm::*CLM-MIX-OPTIONS*
	    clm::add-soundq
	    clm::samp0
	    clm::nrev
	    clm::jc-reverb)
	  :layers-utils))

;; EOF clm.lsp
