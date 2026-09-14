(in-package #:portfolio-cl)


(defun create-cookie (cookie-name cookie-value &key (path "/") (max-age nil) (http-only t) (same-site "Lax"))
  (format nil "~A=~A; Path=~A~@[; Max-Age=~A~]~@[; HttpOnly~]~@[; SameSite=~A~]"
    cookie-name cookie-value path max-age http-only same-site))


(defun check-write-date (file)
  (handler-case
      (let ((time (file-write-date file)))
        (if time time 0))
    (file-error (c)
                (declare (ignore c))
                0)))

(defun unix-time-to-str (unix-time)
  (multiple-value-bind (second minute hour date month year day-of-week daylight-saving-time-p time-zone)
      (decode-universal-time unix-time)
    (declare (ignore time-zone daylight-saving-time-p day-of-week minute hour second))
    (format nil "~d-~2,'0d-~2,'0d" year month date)))
(defun fenced-code-marker-length (line)
  "Return the length of a backtick fence at the start of LINE, if any."
  (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
    (when (and (>= (length trimmed) 3)
               (char= (char trimmed 0) #\`)
               (char= (char trimmed 1) #\`)
               (char= (char trimmed 2) #\`))
          (loop for index from 0 below (length trimmed)
                while (char= (char trimmed index) #\`)
                finally (return index)))))

(defun closing-fence-p (line opening-length)
  "Whether LINE closes a backtick fence with OPENING-LENGTH backticks."
  (let* ((trimmed (string-left-trim '(#\Space #\Tab) line))
         (length (fenced-code-marker-length trimmed)))
    (and length
         (>= length opening-length)
         (every (lambda (character)
                  (member character '(#\Space #\Tab)))
             (subseq trimmed length)))))

(defun escape-markdown-eval (line)
  "Prevent cl-markdown's {command} extension syntax in user-authored prose."
  (with-output-to-string (out)
    (loop for character across line do
            (when (member character '(#\{ #\}))
                  (write-char #\\ out))
            (write-char character out))))

(defun normalize-fenced-code-blocks (content)
  "Translate GitHub-style fenced blocks to cl-markdown's indented blocks.

CL-MARKDOWN predates fenced blocks.  Without this conversion it parses the
contents as ordinary Markdown, including its {command} evaluation syntax."
  (with-input-from-string (input content)
    (with-output-to-string (output)
      (loop with opening-length = nil
            for raw-line = (read-line input nil nil)
            while raw-line
            ;; READ-LINE removes #\Newline but leaves #\Return from CRLF
            ;; input, which would otherwise prevent a closing ``` from matching.
            for line = (string-right-trim '(#\Return) raw-line)
            do
              (cond
                (opening-length
                 (if (closing-fence-p line opening-length)
                     ;; The closing fence is removed, so add a blank line to
                     ;; terminate cl-markdown's indentation-based code block.
                     (progn
                       (setf opening-length nil)
                       (terpri output))
                     (format output "    ~A~%" line)))
               ((fenced-code-marker-length line)
                 ;; CL-MARKDOWN requires a blank line before an indented block.
                 (setf opening-length (fenced-code-marker-length line))
                 (terpri output))
               (t
                 (format output "~A~%" (escape-markdown-eval line))))))))

(defun render-markdown (content)
  (with-output-to-string (stream)
    (format t "~a~%" (normalize-fenced-code-blocks content))
    (cl-markdown:markdown (normalize-fenced-code-blocks content) :stream stream)))

(defun escape-line (string)
  (with-output-to-string (out)
    (loop for c across string do
            (case c
              (#\\ (write-string "\\\\" out))
              (#\Newline (write-string "\\n" out))
              (#\Return (write-string "\\r" out))
              (#\Tab (write-string "\\t" out))
              (t (write-char c out))))))

(defun unescape-line (string)
  (with-output-to-string (out)
    (loop with i = 0
          with n = (length string)
          while (< i n)
          for c = (char string i)
          do (cond
              ((char= c #\\)
                (if (< (1+ i) n)
                    (let ((next (char string (1+ i))))
                      (case next
                        (#\\ (write-char #\\ out))
                        (#\n (write-char #\Newline out))
                        (#\r (write-char #\Return out))
                        (#\t (write-char #\Tab out))
                        (t (write-char #\\ out)
                           (write-char next out)))
                      (incf i))
                    (write-char #\\ out)))
              (t (write-char c out)))
            (incf i))))
