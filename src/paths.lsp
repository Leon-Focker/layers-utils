;; ** Paths

(in-package :layers-utils)

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


;; EOF paths.lsp
