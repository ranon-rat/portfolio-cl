(in-package #:portfolio-cl)
; POST /login
(defun post-login (params)
  (let* ((password (ingle:get-param "password" params))
         (cookie (create-cookie "password" password)))
    (if (string= password *password*)
        `(302 (:location "/private/post.html" :set-cookie ,cookie) (""))
        `(302 (:location "/") ("")))))


; GET /private/test-private
(defun get-test-private (params)
  (declare (ignore params))
  (format nil "hey i am here :)"))

; GET /private/post-json
(defun get-post-json (params)
  (format t "~A~%" params)
  (let* ((id (ingle:get-param "id" params))
         (post (get-individual-post id)))
    (format t "~a~%" post)
    (cl-json:encode-json-to-string (post->alist post))))
; POST /private/new-post
(defun post-new-post (params)
  ; here what i should do its simple i should get the information
  (let* ((current-time (get-universal-time))
         (date (unix-time-to-str current-time))
         (content (ingle:get-param "content" params))
         (parsed-content (render-markdown content))
         (id (format nil "~a.~a" current-time (random 10000)))
         (new-post (make-post :id id
                              :written-at current-time
                              :written-at-str date
                              :content content
                              :parsed-content parsed-content
                              :title (ingle:get-param "title" params)
                              :image-url (ingle:get-param "image-url" params)
                              :tags (ingle:get-param "tags" params)
                              :resume (ingle:get-param "resume" params))))
    (add-new-to-db new-post)
    (build-post new-post)
    (rebuild-blog-index)

    `(302 (:location ,(format nil "/blog/posts/~a.html" id)) ())))
; POST /private/update-post
(defun update-post (params)
  (let* ((current-time (get-universal-time))
         (date (unix-time-to-str current-time))
         (content (ingle:get-param "content" params))
         (parsed-content (render-markdown content))
         (id (ingle:get-param "id" params))

         (updated-post (make-post :id id
                                  :written-at current-time
                                  :written-at-str date
                                  :content content
                                  :parsed-content parsed-content
                                  :title (ingle:get-param "title" params)
                                  :image-url (ingle:get-param "image-url" params)
                                  :tags (ingle:get-param "tags" params)
                                  :resume (ingle:get-param "resume" params))))
    (unless id
      (return-from update-post '(302 (:location "/blog/") (""))))
    (update-from-db updated-post)
    (rebuild-blog-index)
    (build-post updated-post)
    `(302 (:location ,(format nil "/blog/posts/~a.html" id)) ())))
; POST /private/delete-post
(defun delete-post (params)
  (let* ((id (ingle:get-param "id" params)))

    (unless id
      (return-from delete-post '(302 (:location "/blog/") (""))))
    (delete-from-db id)
    (delete-post-template id)
    (rebuild-blog-index)
    '(302 (:location "/blog/") (""))))