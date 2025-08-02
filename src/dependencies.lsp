;; ** dependencies

;; *** cm

(unless (find-package :cmn) (ignore-errors (ql:quickload :cmn)))
(unless (find-package :cmn)
  (warn "layers-utils: Package Common Music Notation (:cmn) not found. Loading ~
         Common Music (:cm) will likely fail!"))

(unless (find-package :cm) (ignore-errors (ql:quickload :cm)))
(unless (find-package :cm)
  (warn "layers-utils: Package Common Music (:cm) not found. Midi ~
         functionality and some utilities will not be available!"))

;; *** clm

(unless (find-package :clm) (ql:quickload :clm));(ignore-errors (ql:quickload :clm)))
(unless (find-package :clm)
  (warn "layers-utils: Package Common Lisp Music (:clm) not found. Analysing ~
         and generating soundfiles (with fplay) will not be available!"))

;; *** metrical-hierarchies

(unless (find-package :mh) (ignore-errors (ql:quickload :metrical-hierarchies)))
(unless (find-package :mh)
  (warn "layers-utils: Package Metrical-hierarchies (:mh) not found. ~
         Functions for generating metrical hierarchies will not be available!"))

;; EOF dependencies.lsp
