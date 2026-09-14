(in-package #:portfolio-cl)

(defun setup-db ()
  (setf *path-to-db* (asdf:system-relative-pathname :portfolio-cl "db/db.lispsv"))
  (setf *db-directory* (asdf:system-relative-pathname :portfolio-cl "db/"))
  (ensure-directories-exist *path-to-db*))
(defun add-new-to-db (post)
  (with-open-file (stream (format nil "~a~a.lispcv" *db-directory* (post-id post))
                          :direction :output)
    (format stream "~a~%" (escape-line (post->text post))))
  (with-open-file (stream *path-to-db*
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream "~a~%" (escape-line (post->text post)))))

(defun delete-from-db (to-delete-id)
  ; here i must create a temp file
  ; then add all of the values except for one
  ; and thats all
  (delete-file (format nil "~a~a.lispcv" *db-directory* to-delete-id))
  (let ((temp-db (asdf:system-relative-pathname :portfolio-cl "db/tmp-db.lispsv")))
    (with-open-file (out-stream temp-db
                                :direction :output)

      (with-open-file (stream *path-to-db* :direction :input)

        (loop for line = (read-line stream nil 'eof)
              until (eq line 'eof)

              for post = (text->post (unescape-line line))
              for current-id = (post-id post)
              do
                (when (string/= current-id to-delete-id)

                      (format out-stream "~a~%" line)))))
    (delete-file *path-to-db*)
    (rename-file temp-db *path-to-db*)))

(defun update-from-db (post)
  (let ((id (post-id post)))
    (delete-from-db id)
    (add-new-to-db post)))
(defun get-posts-from-db ()
  (if (probe-file *path-to-db*)
      (with-open-file (stream *path-to-db* :direction :input)
        (reverse (loop for line = (read-line stream nil 'eof)
                       until (eq line 'eof)
                       for post = (text->post (unescape-line line))
                       collect post)))
      nil))

(defun get-individual-post (id)
  (let ((path-to-file (format nil "~a~a.lispcv" *db-directory* id))
        (line ""))
    (when (probe-file path-to-file)
          (with-open-file (stream path-to-file :direction :input)
            (setf line (read-line stream nil 'eof))
            (when line
                  (text->post (unescape-line line)))))))