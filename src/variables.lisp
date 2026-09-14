(in-package #:portfolio-cl)
;db
(defvar *path-to-db* nil)
(defvar *db-directory* nil)
; routers
(defvar *public-routes* nil)
(defvar *private-routes* nil)
; server
(defvar *server* nil)
;env
(defvar *password* "1234")
(defvar *port* 8080)
(defvar *undesirable* (list "env" "php" ".json" "key" "py" "admin" "mjs" ".properties" ".xml"))

(defun setup-variables ()
  (let ((password (uiop:getenv "PASSWORD"))
        (port-str (uiop:getenv "PORT")))
    (when password (setf *password* password))
    (when port-str (setf *port* (parse-integer port-str)))))
