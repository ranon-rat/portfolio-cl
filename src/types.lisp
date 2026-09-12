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
(defun post->alist (p)
  (let* ((class (class-of p))
         (slots (closer-mop:class-slots class)))
    (mapcar (lambda (slot)
              (let ((slot-name (closer-mop:slot-definition-name slot)))
                (cons slot-name (slot-value p slot-name))))
        slots)))
(defun text->post (txt)
  (let ((*read-eval* nil))
    (read-from-string txt)))