(in-package #:portfolio-cl)


(defun fenced-code-marker-length (line)
  "Return the length of a backtick fence at the start of LINE, if any."
  (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
    (when (and (>= (length trimmed) 3)
               (string= (subseq trimmed 0 3) "```"))
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