(defsystem #:layers-utils
  :version "0.1"
  :default-component-class "cl-source-file.lsp"
  :description "My personal collection of useful stuff."
  :author "Leon Focker contact@leonfocker.de"
  :license "GNU General Public License v3.0"
  ;; there is more dependencies like  ; cm, cmn, clm, metrical-hierarchies
  ;; but these are not on quicklisp, so let's just specify:
  :depends-on ("imago")
  :in-order-to ((test-op (test-op "layers-utils/tests")))
  :serial t
  :components ((:file "src/pcg-copy")
	       (:file "src/all-light")
	       (:file "src/all")))

(defsystem #:layers-utils/light
  :version "0.1"
  :default-component-class "cl-source-file.lsp"
  :description "Light Version of lyu without any dependencies."
  :author "Leon Focker contact@leonfocker.de"
  :license "GNU General Public License v3.0"
  :serial t
  :components ((:file "src/pcg-copy")
	       (:file "src/all-light")))

;;; regression tests
(defsystem #:layers-utils/tests
  :version "0.1"
  :default-component-class "cl-source-file.lsp"
  :description "Test suite for layers-utils"
  :author "Leon Focker contact@leonfocker.de"
  :license "GNU General Public License v3.0"
  :depends-on ("layers-utils"
               "fiveam")
  :perform (test-op (o c)
		     (uiop:symbol-call :layers-utils-tests :run-tests))
  :components ((:module "tests"
		:serial t
		:components ((:file "package")
			     (:file "main")))))

;; EOF layers-utils.asd
