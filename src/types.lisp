(in-package #:portfolio-cl)

(defstruct post
  (tags nil :type string)
  (content nil :type string)
  (parsed-content nil :type string)
  (image-url nil :type string)
  (title nil :type string)
  (written-at 0 :type integer) ;it would be saved in unix time
  (written-at-str "" :type string)
  (id nil :type string)
  (resume "" :type string))


(defun post->text (p)
  (let ((*print-readably* t)
        (*print-circle* t)
        (*print-pretty* nil))
    (write-to-string p)))

(defun text->post (txt)
  (let ((*read-eval* nil))
    (read-from-string txt)))


(defstruct sitemap-url
  (url "" :type string)
  (last-mod "" :type string)
  (changefreq "weekly" :type string)
  (priority 1 :type number))

(defun get-file-sitemap-url (path url &key (changefreq "weekly") (priority 1))
  (let* ((date (check-write-date path))
         (last-mod (unix-time-to-str date)))
    (make-sitemap-url
     :url url
     :last-mod last-mod
     :changefreq changefreq
     :priority priority)))