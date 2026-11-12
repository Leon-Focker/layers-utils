;; * Package

(in-package :cl-user)

(defpackage :layers-utils
  (:use :common-lisp :layers-utils/light)
  (:nicknames :lyu)
  (:import-from
   :layers-utils/light
   :layers-utils-path
   :load-layers-utils-src-file
   :defpublic
   :defpubmethod
   :*layers-utils-home-dir*
   :*layers-utils-src-dir*))

(defun lyu () (in-package :layers-utils))


;; EOF package.lsp
