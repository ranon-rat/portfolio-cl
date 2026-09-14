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


(defun struct->alist (p)
  (let* ((class (class-of p))
         (slots (closer-mop:class-slots class)))
    (mapcar (lambda (slot)
              (let ((slot-name (closer-mop:slot-definition-name slot)))
                (cons slot-name (slot-value p slot-name))))
        slots)))

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
