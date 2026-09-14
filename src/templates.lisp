(in-package #:portfolio-cl)
(defun setup-templates ()
  (djula:add-template-directory (asdf:system-relative-pathname :portfolio-cl "templates/"))
  (create-blog-index)
  (rebuild-posts)
  (generate-sitemaps))


(defun create-if-newer-version (&key path result-path execute)
  (let* ((age-public (check-write-date result-path))
         (age-private (check-write-date path)))
    (ensure-directories-exist result-path)
    (when (> age-private age-public)
          (format t "[LOG] creating template {~A}.~%" execute)
          (with-open-file (stream result-path
                                  :direction :output ; Open for writing
                                  :if-exists :supersede ; Overwrite if file exists
                                  :if-does-not-exist :create) ; Create file if missing
            (write-string (funcall execute) stream)))))
; for this i must 1 check if the file index.html exist in ./public/blog
(defun create-blog-index ()
  (let* ((path-to-template (asdf:system-relative-pathname :portfolio-cl "templates/blog.html"))
         (path-to-public (asdf:system-relative-pathname :portfolio-cl "public/blog/index.html"))
         (path-to-base (asdf:system-relative-pathname :portfolio-cl "templates/base.html"))
         (path-to-db (asdf:system-relative-pathname :portfolio-cl "db/db.lispsv"))
         (posts (get-posts-from-db))
         (func (lambda () (djula:render-template* "blog.html" nil :posts (mapcar #'struct->alist posts)))))
    (create-if-newer-version :path path-to-base :result-path path-to-public :execute func)
    (create-if-newer-version :path path-to-template :result-path path-to-public :execute func)
    (create-if-newer-version :path path-to-db :result-path path-to-public :execute func)))

(defun rebuild-blog-index ()
  (let ((path-to-public (asdf:system-relative-pathname :portfolio-cl "public/blog/index.html"))
        (posts (get-posts-from-db)))
    (with-open-file (stream path-to-public
                            :direction :output ; Open for writing
                            :if-exists :supersede ; Overwrite if file exists
                            :if-does-not-exist :create) ; Create file if missing
      (write-string (djula:render-template* "blog.html" nil :posts (mapcar #'struct->alist posts)) stream))))

(defun get-amount-posts ()
  (let ((path (asdf:system-relative-pathname :portfolio-cl "public/blog/posts")))
    (ensure-directories-exist path)
    (length (uiop:directory-files path))))

(defun build-post (post)
  (let* ((path-to-template (asdf:system-relative-pathname :portfolio-cl "templates/post.html"))
         (path-to-public (asdf:system-relative-pathname :portfolio-cl (format nil "public/blog/posts/~a.html" (post-id post))))
         (path-to-base (asdf:system-relative-pathname :portfolio-cl "templates/base.html"))

         (func (lambda () (djula:render-template* "post.html" nil :post (struct->alist post)))))
    (create-if-newer-version :path path-to-base :result-path path-to-public :execute func)
    (create-if-newer-version :path path-to-template :result-path path-to-public :execute func)))

(defun rebuild-post (post)
  (let ((path-to-public (asdf:system-relative-pathname :portfolio-cl (format nil "public/blog/posts/~a.html" (post-id post)))))
    (with-open-file (stream path-to-public
                            :direction :output ; Open for writing
                            :if-exists :supersede ; Overwrite if file exists
                            :if-does-not-exist :create) ; Create file if missing
      (write-string (djula:render-template* "post.html" nil :post (struct->alist post)) stream))))
(defun delete-post-template (id)
  (let ((path-to-public (asdf:system-relative-pathname :portfolio-cl (format nil "public/blog/posts/~a.html" id))))
    (delete-file path-to-public)))
(defun rebuild-posts ()
  (let ((posts (get-posts-from-db)))
    (loop for post in posts do
            (build-post post))))


(defun generate-sitemaps ()

  (let* ((public-dir (asdf:system-relative-pathname :portfolio-cl "public/"))
         (posts-dir (asdf:system-relative-pathname :portfolio-cl "public/blog/posts"))
         (blog-index-dir (asdf:system-relative-pathname :portfolio-cl "public/blog/index.html"))
         (index-dir (asdf:system-relative-pathname :portfolio-cl "public/index.html"))
         (files-of-post-dir (remove-if-not (lambda (f) (search "html" (namestring f))) (uiop:directory-files posts-dir)))
         (sitemaps-post (mapcar (lambda (p)
                                  (get-file-sitemap-url (namestring p)
                                                        (format nil "http://ranon-rat.work/~a" (subseq (namestring p) (length (namestring public-dir))))
                                                        :changefreq "monthly"
                                                        :priority 0.5))
                            files-of-post-dir))
         (sitemaps (append (list
                            (get-file-sitemap-url index-dir "http://ranon-rat.work/index.html" :changefreq "monthly" :priority 1)
                            (get-file-sitemap-url blog-index-dir "http://ranon-rat.work/blog/index.html" :changefreq "weekly" :priority 0.3))
                     sitemaps-post))
         (output-dir (asdf:system-relative-pathname :portfolio-cl "public/sitemaps.xml")))

    (with-open-file (stream output-dir
                            :direction :output ; Open for writing
                            :if-exists :supersede ; Overwrite if file exists
                            :if-does-not-exist :create) ; Create file if missing
      (write-string (djula:render-template* "sitemaps.xml" nil :sitemaps (mapcar #'struct->alist sitemaps)) stream))))