(in-package #:portfolio-cl)


(defun setup-private-routes ()
  (unless *private-routes*
    (setf *private-routes* (make-instance 'ningle:<app>))
    (setf (ningle:route *private-routes* "/test-private" :method :get) #'get-test-private)
    (setf (ningle:route *private-routes* "/new-post" :method :post) #'post-new-post)
    (setf (ningle:route *private-routes* "/post-json" :method :get) #'get-post-json)
    (setf (ningle:route *private-routes* "/update-post" :method :post) #'update-post)
    (setf (ningle:route *private-routes* "/delete-post" :method :post) #'delete-post)))
(defun setup-public-routes ()
  (unless *public-routes*
    (setf *public-routes* (make-instance 'ningle:<app>))
    ; this seem quite simple
    (setf (ningle:route *public-routes* "/login" :method :post) #'post-login)))