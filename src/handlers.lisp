(in-package #:portfolio-cl)


(defun search-in-path (words path)
  (loop for w in words
        do
          (when (search w path)
                (return-from search-in-path t))))
(defun check-file-exist (path)
  (let* ((safe-path (if path path "/"))
         (last-char (char safe-path (1- (length path))))
         (sliced-path (subseq safe-path 1))
         (directing-to (merge-pathnames sliced-path #p"./public/")))
    (cond
     ((uiop:directory-exists-p directing-to)
       (format nil "~a~a~a" safe-path (if (char= last-char #\/) "" "/") "index.html"))

     ((probe-file directing-to)
       path)

     ((or (search-in-path *undesirable* path)) "/secrets.zip"))))

(defun build-handler ()
  (lack:builder
   #'logger-middleware
   (:static :path #'check-file-exist
            :root #p"./public/")
   (:mount "/private" (lack:builder #'auth-middleware *private-routes*))
   *public-routes*))
