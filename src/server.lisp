(in-package #:portfolio-cl)


(defun start-server ()
  (setup)
  (unless *server*
    (format t "starting server on http://localhost:~a~%" *port*)
    (describe *public-routes*)

    (setf *server* (clack:clackup (build-handler) :server :woo :port *port* :address "127.0.0.1"))))
