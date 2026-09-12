(require :asdf)
(require :cl-dotenv)

(defparameter *current-path* (uiop:pathname-directory-pathname *load-truename*))

(defun get-relative-path (path)
  (merge-pathnames path *current-path*))

(asdf:load-asd (get-relative-path #P"./portfolio-cl.asd"))
(dotenv:load-env (asdf:system-relative-pathname :portfolio-cl ".env"))

(asdf:load-system :portfolio-cl/cmd)