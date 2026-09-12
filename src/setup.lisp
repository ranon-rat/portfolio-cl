(in-package #:portfolio-cl)


(defun setup ()
  (setf *random-state* (make-random-state t))
  (setup-variables)
  (setup-db)
  (setup-templates)
  (setup-public-routes)
  (setup-private-routes))