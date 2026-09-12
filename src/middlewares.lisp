(in-package #:portfolio-cl)
; this is what the env contains
;(:request-method :get
; :path-info " /test-test"
;:query-string ""
;:server-name "localhost"
;:server-port 8080
;:url-scheme :http
;:script-name ""
;:remote-addr "127.0.0.1"
;:headers (("host" . "localhost:8080")
;          ("user-agent" . "curl/8.18.0"))
;:body #<stream>
;...)

(defun logger-middleware (app)
  (lambda (env)
    ;(print env)
    (let ((method (getf env :request-method))
          (path (getf env :path-info)))

      (format t "[~A] ~A~%" method path)
      (let ((response (funcall app env)))
        response))))
(defun auth-middleware (app)
  (lambda (env)
    ; okay this finally works :)
    (let* ((req (lack.request:make-request env))
           (response (funcall app env))
           (cookies (lack.request:request-cookies req))
           (password (cdr (assoc "password" cookies :test #'string=))))
      (if (string= password *password*)
          response
          '(302 (:location "/") (""))))))