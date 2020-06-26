(defun do-it ()
  (dolist (msg-file (directory "*.MSG"))
    (let ((out-file (format nil "~A" (string-downcase (pathname-name msg-file)))))
      (format t "~A => ~A (~D message~:P)~%"
              (uiop:native-namestring msg-file)
              out-file
              (convert-msg-file msg-file out-file)))))

(defun convert-msg-file (input-filename output-filename)
  (write-messages (read-msg-file input-filename) output-filename))

(defun write-messages (messages output-filename)
  (with-open-file (stream output-filename :direction :output
                                          :if-exists :supersede)
    (format stream "--~%")
    (dolist (message messages)
      (format stream "~A~2%--~%" (trim-whitespace (escape-dashes message)))))
  (length messages))

(defun escape-dashes (string)
  (ppcre:regex-replace-all "\\n--" string "
- --"))

(defun trim-whitespace (string)
  (string-trim '(#\Space #\Tab #\Newline) string))

(defun read-msg-file (filename)
  (cl-arrows:->>
   (alexandria:read-file-into-string filename)
   (split-sequence:split-sequence #\Page)
   (mapcar #'convert-message)
   (remove nil)))

(defun convert-message (string)
  (with-input-from-string (in string)
    (let ((first-line (read-line in nil nil))
          (header-fields nil)
          (more-header-fields nil)
          (field nil)
          (in-headers nil))
      (cond ((or (setf header-fields (parse-weird-header-line first-line))
                 (setf field (header-field-p first-line)))
             (when field
               (setf header-fields (list field))
               (setf in-headers t))
             (with-output-to-string (out)
               (flet ((maybe-write-headers ()
                        (when header-fields
                          (setf header-fields (order-header-fields header-fields))
                          (dolist (header-field header-fields)
                            (format out "~A: ~A~%" (car header-field) (cdr header-field)))
                          (terpri out)
                          (setf header-fields nil))))
                 (loop for line = (read-line in nil nil)
                       while line
                       do (cond ((and header-fields (setq more-header-fields (parse-weird-header-line line)))
                                 (setf header-fields more-header-fields))
                                ((and header-fields (setf field (header-field-p line)))
                                 (setf in-headers t)
                                 (setf header-fields (add-header-field field header-fields)))
                                (in-headers
                                 (cond ((equal (trim-whitespace line) "")
                                        (setf in-headers nil)
                                        (maybe-write-headers))
                                       ((not (whitespace-char-p (char line 0)))
                                        ;; Some messages don't have a
                                        ;; header/text separator...
                                        (setf in-headers nil)
                                        (maybe-write-headers)
                                        (write-line line out))))
                                (t
                                 (maybe-write-headers)
                                 (write-line line out)))))))
            ((alexandria:starts-with-subseq "COMMENT " first-line)
             nil)
            ((null first-line)
             nil)
            (t
             (format nil "From: unknown~%Subject: (no subject)~2%~A" string))))))

(defun parse-weird-header-line (string)
  (setf string (trim-whitespace (remove-if-not #'ascii-char-p string)))
  (let ((fields (split-sequence:split-sequence #\Tab string)))
    (when (and (>= (length fields) 2)
               (pre-y2k-datestring-p (first fields)))
      (destructuring-bind (datestring from &optional (subject "(no subject)") &rest whatever) fields
        (declare (ignore whatever))
        (list (cons "From" from)
              (cons "Subject" subject)
              (cons "Date" (rfc882-datestring datestring)))))))

(defun header-field-p (object &aux colon-pos)
  (and (stringp object)
       (setf colon-pos (position #\: object))
       (plusp colon-pos)
       (null (position #\Space object :end colon-pos))
       (cons (subseq object 0 colon-pos)
             (subseq object (1+ colon-pos)))))

(defun ascii-char-p (char)
  (let ((code (char-code char)))
    (and code (< code 128))))

(defun whitespace-char-p (char)
  (or (eql char #\Space)
      (eql char #\Tab)))

(defun pre-y2k-datestring-p (string)
  (ppcre:scan "^\\d{2}-\\w{3}-\\d{2}  \\d{4}$" string))

(defun rfc882-datestring (old-datestring)
  (let ((day (parse-integer old-datestring :start 0 :end 2))
        (month (subseq old-datestring 3 6))
        (year (+ 1900 (parse-integer old-datestring :start 7 :end 9)))
        (hour (parse-integer old-datestring :start 11 :end 13))
        (minute (parse-integer old-datestring :start 13 :end 15)))
    ;; Bogus timezone
    (format nil "~2,'0D ~A ~4,'0D ~2,'0D:~2,'0D EST" day month year hour minute)))

(defun add-header-field (field header-fields)
  (let ((entry (assoc (car field) header-fields :test #'equal)))
    (cons field (remove entry header-fields))))

(defun order-header-fields (header-fields)
  (add-header-field (or (assoc "From" header-fields :test #'equal)
                        (cons "From" "unknown"))
                    header-fields))
