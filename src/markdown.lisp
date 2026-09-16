(in-package #:portfolio-cl)

;;;; A small Markdown renderer.
;;;;
;;;; CL-MARKDOWN predates fenced code blocks, renumbers ordered lists, drops
;;;; the line structure of a paragraph and evaluates {...} as Lisp, so posts
;;;; are rendered here instead.  The subset covered is the one a blog post
;;;; actually uses: ATX and setext headings, fenced code, blockquotes, nested
;;;; and mixed lists, pipe tables, thematic breaks, raw HTML blocks, and the
;;;; usual inline spans.

;;; ------------------------------------------------------------------
;;; Lines
;;; ------------------------------------------------------------------

(defparameter +tab-width+ 4
  "Columns a leading #\\Tab expands to when measuring indentation.")

(defun expand-leading-tabs (line)
  "Expand the tabs in LINE's leading whitespace so indentation is countable."
  (let ((index (position-if-not (lambda (character)
                                  (member character '(#\Space #\Tab)))
                                line)))
    (if (and index (zerop (count #\Tab line :end index)))
        line
        (let ((end (or index (length line))))
          (with-output-to-string (out)
            (loop for position from 0 below end
                  do (if (char= (char line position) #\Tab)
                         (dotimes (ignored +tab-width+)
                           (declare (ignore ignored))
                           (write-char #\Space out))
                         (write-char #\Space out)))
            (write-string (subseq line end) out))))))

(defun split-into-lines (content)
  "Split CONTENT into a vector of lines, without #\\Return or leading tabs."
  (let ((lines '()))
    (with-input-from-string (input content)
      (loop for raw-line = (read-line input nil nil)
            while raw-line
            ;; READ-LINE removes #\Newline but leaves #\Return from CRLF input,
            ;; which would otherwise keep a closing ``` from ever matching.
            do (push (expand-leading-tabs (string-right-trim '(#\Return) raw-line))
                     lines)))
    (coerce (nreverse lines) 'vector)))

(defun blank-line-p (line)
  (every (lambda (character) (member character '(#\Space #\Tab))) line))

(defun line-indent (line)
  "Number of leading spaces in LINE, or its length when LINE is all spaces."
  (or (position #\Space line :test-not #'char=) (length line)))

(defun dedent-line (line width)
  "Drop up to WIDTH leading spaces from LINE."
  (subseq line (min (line-indent line) width (length line))))

;;; ------------------------------------------------------------------
;;; Escaping
;;; ------------------------------------------------------------------

(defun escape-html (text)
  "Escape TEXT for use as element content or as an attribute value."
  (with-output-to-string (out)
    (loop for character across text
          do (case character
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (t (write-char character out))))))

(defun html-entity-length (text start)
  "Length of the HTML entity starting at START in TEXT, or NIL."
  (let ((end (length text))
        (index (1+ start)))
    (when (>= index end)
      (return-from html-entity-length nil))
    (let ((numericp (char= (char text index) #\#)))
      (when numericp
        (incf index)
        (when (and (< index end) (member (char text index) '(#\x #\X)))
          (incf index)))
      (let ((body-start index))
        (loop while (and (< index end) (alphanumericp (char text index)))
              do (incf index))
        (when (and (> index body-start)
                   (< index end)
                   (char= (char text index) #\;))
          (- (1+ index) start))))))

(defun html-tag-length (text start)
  "Length of the HTML tag starting at START in TEXT, or NIL.

Posts embed raw HTML such as <iframe>, so tags are passed through untouched
instead of being escaped."
  (let ((end (length text))
        (index (1+ start)))
    (when (and (< index end) (member (char text index) '(#\/ #\!)))
      (incf index))
    (unless (and (< index end) (alpha-char-p (char text index)))
      (return-from html-tag-length nil))
    (loop while (and (< index end)
                     (or (alphanumericp (char text index))
                         (char= (char text index) #\-)))
          do (incf index))
    ;; Attributes, up to the closing angle bracket.  A nested #\< means this
    ;; was never a tag to begin with.
    (loop while (and (< index end) (char/= (char text index) #\>))
          do (when (char= (char text index) #\<)
               (return-from html-tag-length nil))
             (incf index))
    (when (< index end)
      (- (1+ index) start))))

;;; ------------------------------------------------------------------
;;; Inline spans
;;; ------------------------------------------------------------------

(defparameter +escapable-characters+ "\\`*_{}[]()#+-.!~<>&\"|"
  "Punctuation a backslash may escape in Markdown.")

(defun backtick-run-length (text start)
  (loop for index from start below (length text)
        while (char= (char text index) #\`)
        finally (return (- index start))))

(defun delimiter-run-length (text start character)
  (loop for index from start below (length text)
        while (char= (char text index) character)
        finally (return (- index start))))

(defun matching-bracket (text start open close)
  "Index of the bracket closing the OPEN at START in TEXT, or NIL."
  (let ((depth 0)
        (index start)
        (end (length text)))
    (loop while (< index end)
          do (let ((character (char text index)))
               (cond ((char= character #\\) (incf index))
                     ((char= character open) (incf depth))
                     ((char= character close)
                      (decf depth)
                      (when (zerop depth)
                        (return-from matching-bracket index)))))
             (incf index))
    nil))

(defun split-link-target (target)
  "Split a link target into its URL and optional title."
  (let* ((trimmed (string-trim '(#\Space #\Tab) target))
         (quote-position (position #\" trimmed)))
    (if (and quote-position
             (plusp quote-position)
             (char= (char trimmed (1- (length trimmed))) #\")
             (< quote-position (1- (length trimmed))))
        (values (string-right-trim '(#\Space #\Tab) (subseq trimmed 0 quote-position))
                (subseq trimmed (1+ quote-position) (1- (length trimmed))))
        (values trimmed nil))))

(defun render-code-span (text index out)
  (let* ((fence (backtick-run-length text index))
         (start (+ index fence))
         (end (length text))
         (scan start))
    (when (zerop fence)
      (return-from render-code-span nil))
    (loop while (< scan end)
          do (if (char= (char text scan) #\`)
                 (let ((run (backtick-run-length text scan)))
                   (when (= run fence)
                     (format out "<code>~A</code>"
                             (escape-html (string-trim '(#\Space) (subseq text start scan))))
                     (return-from render-code-span (- (+ scan run) index)))
                   (incf scan run))
                 (incf scan)))
    nil))

(defun render-link (text index out &key imagep)
  "Render the link or image whose [label] starts at INDEX."
  (let* ((bracket (if imagep (1+ index) index))
         (label-end (when (and (< bracket (length text))
                               (char= (char text bracket) #\[))
                      (matching-bracket text bracket #\[ #\]))))
    (when (and label-end
               (< (1+ label-end) (length text))
               (char= (char text (1+ label-end)) #\())
      (let ((target-end (matching-bracket text (1+ label-end) #\( #\))))
        (when target-end
          (let ((label (subseq text (1+ bracket) label-end))
                (target (subseq text (+ label-end 2) target-end)))
            (multiple-value-bind (url title) (split-link-target target)
              (if imagep
                  (format out "<img src=\"~A\" alt=\"~A\"~@[ title=\"~A\"~] />"
                          (escape-html url)
                          (escape-html label)
                          (when title (escape-html title)))
                  (format out "<a href=\"~A\"~@[ title=\"~A\"~]>~A</a>"
                          (escape-html url)
                          (when title (escape-html title))
                          (render-inline label)))
              (- (1+ target-end) index))))))))

(defun emphasis-tag (character marker-length)
  (cond ((char= character #\~) "del")
        ((= marker-length 2) "strong")
        (t "em")))

(defun closing-delimiter-position (text start character marker-length)
  "Position of the run of CHARACTER that closes an emphasis span, or NIL."
  (let ((end (length text))
        (index start))
    (loop while (< index end)
          do (let ((current (char text index)))
               (cond ((char= current #\\) (incf index))
                     ((char= current #\`)
                      ;; Code spans bind tighter than emphasis.
                      (incf index (max 1 (backtick-run-length text index))))
                     ((char= current character)
                      (let ((run (delimiter-run-length text index character)))
                        (if (>= run marker-length)
                            (return-from closing-delimiter-position index)
                            (incf index (1- run)))))))
             (incf index))
    nil))

(defun render-emphasis (text index out)
  (let ((character (char text index)))
    (unless (member character '(#\* #\_ #\~))
      (return-from render-emphasis nil))
    ;; Underscores inside a word are part of the word, not emphasis.
    (when (and (char= character #\_)
               (plusp index)
               (alphanumericp (char text (1- index))))
      (return-from render-emphasis nil))
    (let* ((run (delimiter-run-length text index character))
           (marker-length (if (>= run 2) 2 1)))
      (when (and (char= character #\~) (< run 2))
        (return-from render-emphasis nil))
      (let* ((start (+ index marker-length))
             (closing (closing-delimiter-position text start character marker-length)))
        (when (and closing (> closing start))
          (let ((tag (emphasis-tag character marker-length)))
            (format out "<~A>~A</~A>" tag (render-inline (subseq text start closing)) tag)
            (- (+ closing marker-length) index)))))))

(defun render-raw-html (text index out)
  (let ((length (html-tag-length text index)))
    (when length
      (write-string (subseq text index (+ index length)) out)
      length)))

(defun render-character (text index out)
  (let ((character (char text index)))
    (cond
      ((and (char= character #\\)
            (< (1+ index) (length text))
            (find (char text (1+ index)) +escapable-characters+))
       (write-string (escape-html (string (char text (1+ index)))) out)
       2)
      ((char= character #\&)
       (let ((length (html-entity-length text index)))
         (cond (length (write-string (subseq text index (+ index length)) out)
                       length)
               (t (write-string "&amp;" out) 1))))
      ((char= character #\<) (write-string "&lt;" out) 1)
      ((char= character #\>) (write-string "&gt;" out) 1)
      (t (write-char character out) 1))))

(defun render-inline (text)
  "Render the inline spans of TEXT to an HTML string."
  (with-output-to-string (out)
    (loop with index = 0
          with end = (length text)
          while (< index end)
          do (incf index
                   (or (case (char text index)
                         (#\` (render-code-span text index out))
                         (#\! (render-link text index out :imagep t))
                         (#\[ (render-link text index out))
                         (#\< (render-raw-html text index out))
                         ((#\* #\_ #\~) (render-emphasis text index out))
                         (t nil))
                       (render-character text index out))))))

(defun render-inline-lines (lines)
  "Render LINES as one inline run, honouring Markdown's hard line breaks."
  (let ((count (length lines)))
    (with-output-to-string (out)
      (loop for line in lines
            for position from 1
            for hard-break-p = (and (< position count)
                                    (or (and (>= (length line) 2)
                                             (string= "  " (subseq line (- (length line) 2))))
                                        (and (plusp (length line))
                                             (char= #\\ (char line (1- (length line)))))))
            do (write-string (render-inline (string-right-trim '(#\Space #\Tab #\\) line)) out)
               (when (< position count)
                 (write-string (if hard-break-p "<br />" "") out)
                 (terpri out))))))

;;; ------------------------------------------------------------------
;;; Block recognisers
;;; ------------------------------------------------------------------

(defun code-fence (line)
  "Return (values RUN CHARACTER INFO) when LINE opens a fenced code block."
  (let* ((trimmed (string-left-trim '(#\Space) line))
         (character (when (plusp (length trimmed)) (char trimmed 0))))
    (when (member character '(#\` #\~))
      (let ((run (delimiter-run-length trimmed 0 character)))
        (when (>= run 3)
          (values run character (string-trim '(#\Space #\Tab) (subseq trimmed run))))))))

(defun code-fence-opening-p (line)
  (and (<= (line-indent line) 3)
       (multiple-value-bind (run character info) (code-fence line)
         (declare (ignore character))
         ;; A backtick info string would make the rest of the line a code span.
         (and run (not (find #\` info))))))

(defun closing-fence-p (line run character)
  (multiple-value-bind (line-run line-character info) (code-fence line)
    (and line-run
         (char= line-character character)
         (>= line-run run)
         (zerop (length info)))))

(defun atx-heading (line)
  "Return (values LEVEL TEXT) when LINE is an ATX heading."
  (when (<= (line-indent line) 3)
    (let* ((trimmed (string-left-trim '(#\Space) line))
           (level (delimiter-run-length trimmed 0 #\#)))
      (when (and (<= 1 level 6)
                 (or (= level (length trimmed))
                     (member (char trimmed level) '(#\Space #\Tab))))
        (values level
                (string-right-trim '(#\Space #\Tab)
                                   (string-right-trim '(#\#)
                                                      (string-trim '(#\Space #\Tab)
                                                                   (subseq trimmed level)))))))))

(defun setext-underline-level (line)
  "1 or 2 when LINE underlines a setext heading, NIL otherwise."
  (when (<= (line-indent line) 3)
    (let ((trimmed (string-trim '(#\Space #\Tab) line)))
      (when (plusp (length trimmed))
        (cond ((every (lambda (character) (char= character #\=)) trimmed) 1)
              ((every (lambda (character) (char= character #\-)) trimmed) 2))))))

(defun thematic-break-p (line)
  (when (<= (line-indent line) 3)
    (let ((trimmed (remove #\Space (remove #\Tab line))))
      (and (>= (length trimmed) 3)
           (member (char trimmed 0) '(#\- #\* #\_))
           (every (lambda (character) (char= character (char trimmed 0))) trimmed)))))

(defun blockquote-p (line)
  (and (<= (line-indent line) 3)
       (let ((trimmed (string-left-trim '(#\Space) line)))
         (and (plusp (length trimmed)) (char= (char trimmed 0) #\>)))))

(defun html-block-p (line)
  (and (<= (line-indent line) 3)
       (let ((trimmed (string-left-trim '(#\Space) line)))
         (and (plusp (length trimmed))
              (char= (char trimmed 0) #\<)
              (html-tag-length trimmed 0)
              t))))

(defun list-marker (line)
  "Return (values KIND CONTENT-INDENT NUMBER) when LINE starts a list item."
  (let* ((indent (line-indent line))
         (end (length line))
         (index indent))
    (when (>= index end)
      (return-from list-marker nil))
    (let ((character (char line index))
          (number nil)
          (kind nil))
      (cond
        ((member character '(#\- #\* #\+))
         (setf kind :bullet)
         (incf index))
        ((digit-char-p character)
         (let ((digits-start index))
           (loop while (and (< index end) (digit-char-p (char line index)))
                 do (incf index))
           (when (or (> (- index digits-start) 9)
                     (>= index end)
                     (not (member (char line index) '(#\. #\)))))
             (return-from list-marker nil))
           (setf kind :ordered
                 number (parse-integer line :start digits-start :end index))
           (incf index)))
        (t (return-from list-marker nil)))
      ;; The marker must be followed by whitespace, or end the line.
      (cond ((>= index end)
             (values kind (+ (- index indent) 1) number))
            ((member (char line index) '(#\Space #\Tab))
             (let ((content (position #\Space line :start index :test-not #'char=)))
               (values kind (if content (- content indent) (- (1+ index) indent)) number)))
            (t nil)))))

(defun table-delimiter-row-p (line)
  "Whether LINE is the ---|:---: row separating a table's head from its body."
  (let ((cells (split-table-row line)))
    (and cells
         (every (lambda (cell)
                  (let ((trimmed (string-trim '(#\Space #\Tab) cell)))
                    (and (plusp (length trimmed))
                         (every (lambda (character) (member character '(#\- #\:))) trimmed)
                         (find #\- trimmed))))
                cells))))

(defun split-table-row (line)
  "Split a pipe-table row into its cells, or NIL when LINE is not one."
  (let ((trimmed (string-trim '(#\Space #\Tab) line)))
    (unless (find #\| trimmed)
      (return-from split-table-row nil))
    (let ((cells '())
          (cell (make-string-output-stream))
          (index 0)
          (end (length trimmed)))
      (when (and (plusp end) (char= (char trimmed 0) #\|))
        (incf index))
      (loop while (< index end)
            do (let ((character (char trimmed index)))
                 (cond ((and (char= character #\\) (< (1+ index) end))
                        (write-char character cell)
                        (write-char (char trimmed (1+ index)) cell)
                        (incf index))
                       ((char= character #\|)
                        (push (get-output-stream-string cell) cells))
                       (t (write-char character cell))))
               (incf index))
      (let ((last (get-output-stream-string cell)))
        (unless (blank-line-p last)
          (push last cells)))
      (nreverse cells))))

(defun table-alignment (cell)
  (let* ((trimmed (string-trim '(#\Space #\Tab) cell))
         (leftp (char= (char trimmed 0) #\:))
         (rightp (char= (char trimmed (1- (length trimmed))) #\:)))
    (cond ((and leftp rightp) "center")
          (rightp "right")
          (leftp "left")
          (t nil))))

(defun block-start-p (line)
  "Whether LINE interrupts a running paragraph."
  (or (blank-line-p line)
      (atx-heading line)
      (code-fence-opening-p line)
      (thematic-break-p line)
      (blockquote-p line)
      (html-block-p line)
      (and (<= (line-indent line) 3) (list-marker line) t)))

;;; ------------------------------------------------------------------
;;; Block rendering
;;; ------------------------------------------------------------------

(defun render-code-block (lines index out)
  "Render the fenced code block opening at INDEX; return the index after it."
  (multiple-value-bind (run character info) (code-fence (aref lines index))
    (let ((indent (line-indent (aref lines index)))
          (body '())
          (scan (1+ index))
          (end (length lines)))
      (loop while (< scan end)
            do (when (closing-fence-p (aref lines scan) run character)
                 (incf scan)
                 (return))
               (push (dedent-line (aref lines scan) indent) body)
               (incf scan))
      (let ((language (subseq info 0 (position #\Space info))))
        (format out "<pre><code~@[ class=\"language-~A\"~]>~{~A~%~}</code></pre>~%"
                (when (plusp (length language)) (escape-html language))
                (mapcar #'escape-html (nreverse body))))
      scan)))

(defun render-html-block (lines index out)
  (let ((scan index)
        (end (length lines)))
    (loop while (and (< scan end) (not (blank-line-p (aref lines scan))))
          do (write-line (aref lines scan) out)
             (incf scan))
    scan))

(defun render-blockquote (lines index out)
  (let ((body '())
        (scan index)
        (end (length lines)))
    (loop while (< scan end)
          do (let ((line (aref lines scan)))
               (cond ((blockquote-p line)
                      (let ((stripped (string-left-trim '(#\Space) line)))
                        (push (dedent-line (subseq stripped 1) 1) body)))
                     ;; A non-blank line without > lazily continues the quote.
                     ((or (blank-line-p line) (block-start-p line))
                      (return))
                     (t (push line body))))
             (incf scan))
    (format out "<blockquote>~%")
    (render-blocks (coerce (nreverse body) 'vector) out)
    (format out "</blockquote>~%")
    scan))

(defun render-table (lines index out)
  "Render the pipe table starting at INDEX; return the index after it."
  (let* ((end (length lines))
         (header (split-table-row (aref lines index)))
         (alignments (mapcar #'table-alignment (split-table-row (aref lines (1+ index)))))
         (scan (+ index 2)))
    (format out "<table>~%<thead>~%<tr>~%")
    (loop for cell in header
          for alignment = (pop alignments)
          do (format out "<th~@[ style=\"text-align:~A\"~]>~A</th>~%"
                     alignment
                     (render-inline (string-trim '(#\Space #\Tab) cell))))
    (format out "</tr>~%</thead>~%<tbody>~%")
    (loop while (and (< scan end)
                     (not (blank-line-p (aref lines scan)))
                     (find #\| (aref lines scan)))
          do (let ((row (split-table-row (aref lines scan)))
                   (row-alignments (mapcar #'table-alignment
                                           (split-table-row (aref lines (1+ index))))))
               (format out "<tr>~%")
               (loop for cell in row
                     for alignment = (pop row-alignments)
                     do (format out "<td~@[ style=\"text-align:~A\"~]>~A</td>~%"
                                alignment
                                (render-inline (string-trim '(#\Space #\Tab) cell))))
               (format out "</tr>~%"))
             (incf scan))
    (format out "</tbody>~%</table>~%")
    scan))

(defun table-start-p (lines index)
  (and (< (1+ index) (length lines))
       (split-table-row (aref lines index))
       (table-delimiter-row-p (aref lines (1+ index)))
       (= (length (split-table-row (aref lines index)))
          (length (split-table-row (aref lines (1+ index)))))))

(defun collect-list (lines index)
  "Collect the list starting at INDEX.

Returns (values ITEMS KIND START LOOSEP NEXT-INDEX), where ITEMS is a list of
line vectors already dedented to their item's content indent."
  (multiple-value-bind (kind content-indent number) (list-marker (aref lines index))
    (let ((end (length lines))
          (items '())
          (current '())
          ;; Column at which the current item's content starts, absolute.
          (current-indent (+ (line-indent (aref lines index)) content-indent))
          (start number)
          (loosep nil)
          (pending-blanks 0)
          (scan index))
      (flet ((finish-item ()
               (when current
                 (push (coerce (nreverse current) 'vector) items)
                 (setf current nil))))
        (loop while (< scan end)
              do (let ((line (aref lines scan)))
                   (cond
                     ((blank-line-p line)
                      (incf pending-blanks)
                      (incf scan))

                     ;; A new item of the same list.
                     ((multiple-value-bind (line-kind width) (list-marker line)
                        (when (and (eq line-kind kind)
                                   (< (line-indent line) current-indent))
                          (when (and current (plusp pending-blanks))
                            (setf loosep t))
                          (finish-item)
                          (setf pending-blanks 0
                                current-indent (+ (line-indent line) width))
                          ;; Keep only what follows the marker; the marker
                          ;; itself must not reach the item's block renderer.
                          (push (subseq line (min current-indent (length line))) current)
                          (incf scan)
                          t)))

                     ;; Anything indented to the item's content belongs to it.
                     ((and current (>= (line-indent line) current-indent))
                      (when (plusp pending-blanks) (setf loosep t))
                      (dotimes (ignored pending-blanks)
                        (declare (ignore ignored))
                        (push "" current))
                      (setf pending-blanks 0)
                      (push (dedent-line line current-indent) current)
                      (incf scan))

                     ;; An unindented fenced block still reads as part of the
                     ;; item it follows; posts write code under a step without
                     ;; indenting it.
                     ((and current (code-fence-opening-p line))
                      (when (plusp pending-blanks) (setf loosep t))
                      (dotimes (ignored pending-blanks)
                        (declare (ignore ignored))
                        (push "" current))
                      (setf pending-blanks 0)
                      (multiple-value-bind (run character) (code-fence line)
                        (push line current)
                        (incf scan)
                        (loop while (< scan end)
                              do (push (aref lines scan) current)
                                 (incf scan)
                                 (when (closing-fence-p (aref lines (1- scan)) run character)
                                   (return)))))

                     ;; Lazy continuation of the item's paragraph.
                     ((and current
                           (zerop pending-blanks)
                           (not (block-start-p line)))
                      (push line current)
                      (incf scan))

                     (t (return)))))
        (finish-item))
      (values (nreverse items) kind (or start 1) loosep scan))))

(defun render-list (lines index out)
  (multiple-value-bind (items kind start loosep scan) (collect-list lines index)
    (if (eq kind :ordered)
        (format out "<ol~@[ start=\"~A\"~]>~%" (when (/= start 1) start))
        (format out "<ul>~%"))
    (dolist (item items)
      (write-string "<li>" out)
      (render-blocks item out :tightp (not loosep))
      (format out "</li>~%"))
    (format out "~A~%" (if (eq kind :ordered) "</ol>" "</ul>"))
    scan))

(defun render-paragraph (lines index out tightp)
  "Render the paragraph starting at INDEX; return the index after it."
  (let ((body '())
        (scan index)
        (end (length lines)))
    (loop while (< scan end)
          do (let ((line (aref lines scan)))
               (when (and body (setext-underline-level line))
                 (let ((level (setext-underline-level line)))
                   (format out "<h~D>~A</h~D>~%"
                           level (render-inline-lines (nreverse body)) level)
                   (return-from render-paragraph (1+ scan))))
               (when (and body (block-start-p line))
                 (return))
               (when (and (null body) (blank-line-p line))
                 (return))
               (push line body)
               (incf scan)))
    (let ((text (render-inline-lines (nreverse body))))
      (if tightp
          (write-string text out)
          (format out "<p>~A</p>~%" text)))
    scan))

(defun render-blocks (lines out &key tightp)
  "Render the block structure of the line vector LINES.

TIGHTP suppresses the <p> around a paragraph, which is what a tight list item
wants."
  (let ((index 0)
        (end (length lines)))
    (loop while (< index end)
          do (let ((line (aref lines index)))
               (setf index
                     (cond
                       ((blank-line-p line) (1+ index))
                       ((code-fence-opening-p line) (render-code-block lines index out))
                       ((atx-heading line)
                        (multiple-value-bind (level text) (atx-heading line)
                          (format out "<h~D>~A</h~D>~%" level (render-inline text) level))
                        (1+ index))
                       ((thematic-break-p line)
                        (format out "<hr />~%")
                        (1+ index))
                       ((blockquote-p line) (render-blockquote lines index out))
                       ((table-start-p lines index) (render-table lines index out))
                       ((and (<= (line-indent line) 3) (list-marker line))
                        (render-list lines index out))
                       ((html-block-p line) (render-html-block lines index out))
                       (t (render-paragraph lines index out tightp))))))))

(defun render-markdown (content)
  "Render CONTENT, a Markdown string, to HTML."
  (with-output-to-string (stream)
    (render-blocks (split-into-lines content) stream)))
