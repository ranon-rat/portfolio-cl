(defpackage #:portfolio-cl/cmd
  (:use #:cl))
(in-package #:portfolio-cl/cmd)

(portfolio-cl:start-server)

;; Bloquear para que el proceso no muera en --script
(handler-case
    (loop (sleep 3600))
  (sb-sys:interactive-interrupt ()
                                (sb-ext:exit :code 0)))