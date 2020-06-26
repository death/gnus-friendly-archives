(defpackage #:snippets/ll1-munge
  (:use #:cl)
  (:import-from #:sb-int)
  (:import-from #:sb-impl)
  (:import-from #:cl-ppcre
                #:regex-replace-all)
  (:import-from #:chtml)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:import-from #:babel
                #:octets-to-string)
  (:import-from #:constantia
                #:slurp-file))

(in-package #:snippets/ll1-munge)

(defun call-with-progress-indicator (fn format-string rate-in-seconds)
  (let ((last-indication-time (get-universal-time))
        (k 0))
    (flet ((tick ()
             (incf k)
             (when (>= (- (get-universal-time) last-indication-time) rate-in-seconds)
               (format t format-string k)
               (force-output)
               (setf last-indication-time (get-universal-time)))
             (values)))
      (funcall fn #'tick))))

(defmacro with-progress-indicator ((tick-fn-name format-string rate-in-seconds) &body forms)
  (let ((tick-arg (gensym)))
    `(call-with-progress-indicator
      (lambda (,tick-arg)
        (flet ((,tick-fn-name () (funcall ,tick-arg)))
          (declare (ignorable #',tick-fn-name))
          ,@forms))
      ,format-string
      ,rate-in-seconds)))

(defun build-mbox (mbox-filename msg-filenames)
  (with-open-file (out mbox-filename
                       :direction :output
                       :if-exists :supersede
                       :external-format :latin-1)
    (handler-bind ((sb-int:stream-encoding-error
                     (lambda (error)
                       (when (find-restart 'sb-impl::output-nothing error)
                         (invoke-restart 'sb-impl::output-nothing)))))
      (with-progress-indicator (tick "~D files processed.~%" 1)
        (format out "--~%")
        (dolist (msg-filename msg-filenames)
          (multiple-value-bind (fields text)
              (parse-msg msg-filename)
            (when (and fields text)
              (loop for (key . val) in (augment-fields fields)
                    do (format out "~A: ~A~%" key val))
              (format out "~%~A~2%--~%" (trim-whitespace (escape-dashes text)))))
          (tick))))))

(defun escape-dashes (string)
  (regex-replace-all "\\n--" string "
- --"))

(defun msg-filenames (maillist-filename)
  (let ((doc (chtml:parse (pathname maillist-filename)
                          (chtml:make-lhtml-builder))))
    (reverse
     (mapcar (lambda (filename)
               (merge-pathnames filename maillist-filename))
             (remove-if-not #'msg-filename-p (collect-hrefs '() doc))))))

(defun msg-filename-p (x)
  (starts-with-subseq "msg" x))

(defun collect-hrefs (result doc)
  (cond ((consp doc)
         (if (eq (car doc) :a)
             (cons (cadr (assoc :href (cadr doc))) result)
             (reduce #'collect-hrefs (cddr doc) :initial-value result)))
        (t result)))

(defun parse-msg (filename)
  (let ((doc (chtml:parse (octets-to-string (slurp-file filename)
                                            :encoding :latin-1)
                          (chtml:make-lhtml-builder))))
    (values (doc-fields doc)
            (doc-text doc))))

(defun doc-fields (doc)
  (mapcan #'make-field (cdr (follow doc '(:html :body :ul)))))

(defun make-field (item)
  (let ((sub (caddr item)))
    (when (consp sub)
      (list (cons (caddr sub)
                  (subseq (format-value (cdddr item)) 2))))))

(defun format-value (x)
  (typecase x
    (string x)
    (cons
     (with-output-to-string (out)
       (labels ((str (a)
                  (cond ((stringp a) a)
                        ((and (consp a) (eq (car a) :a))
                         (str (caddr a))))))
         (dolist (a x)
           (write-string (str a) out)))))
    (t "(unknown)")))

(defun filter-doc (fn doc)
  (let ((result '()))
    (labels ((rec (doc)
               (cond ((null doc))
                     ((atom doc)
                      (if (eq (funcall fn doc) :collect)
                          (push doc result)))
                     (t
                      (if (eq (funcall fn doc) :collect)
                          (push doc result)
                          (dolist (child (cddr doc))
                            (rec child)))))))
      (rec doc))
    (nreverse result)))

(defun message-part (doc)
  (let ((hr-seen 0))
    (filter-doc (lambda (x)
                  (cond ((and (consp x) (eq (car x) :hr))
                         (incf hr-seen)
                         nil)
                        ((= hr-seen 3)
                         :collect)))
                doc)))

(defun trim-whitespace (string)
  (string-trim '(#\Space #\Tab #\Newline) string))

(defun whitespace-char-p (char)
  (or (eql char #\Space)
      (eql char #\Tab)
      (eql char #\Newline)))

(defun doc-text (doc)
  (with-output-to-string (out)
    (let ((first t)
          (text-seen nil))
      (labels ((rec (x)
                 (cond ((null x))
                       ((stringp x)
                        (when (or text-seen
                                  (notevery #'whitespace-char-p x))
                          (setf text-seen t)
                          (if first
                              (setf first nil)
                              (write-char #\Space out))
                          (write-string (substitute #\Space #\  x) out)))
                       ((atom x) (warn "Atom: ~S" x))
                       (t (dolist (xx (cddr x))
                            (rec xx))))))
        (dolist (subpart (message-part doc))
          (rec subpart))))))

(defun follow (doc path)
  (cond ((null path) doc)
        ((eq (car doc) (car path)) (follow-1 (cdr doc) (cdr path)))
        (t nil)))

(defun follow-1 (elms path)
  (let (x)
    (cond ((null path) elms)
          ((setq x (find (car path) elms :key #'safe-car))
           (follow-1 (cdr x) (cdr path)))
          (t nil))))

(defun safe-car (x)
  (if (listp x)
      (car x)
      nil))

(defvar *default-fields*
  '(("From" . "unknown")
    ("Subject" . "(no subject)")))

(defun merge-alists (alist-1 alist-2 &key (test #'equal))
  (loop for (k . v) in alist-2
        do (when (not (assoc k alist-1 :test test))
             (setf alist-1 (acons k v alist-1))))
  alist-1)

(defun order-fields (fields)
  (let ((entry (assoc "From" fields :test #'equal)))
    (cons entry (remove entry fields))))

(defun augment-fields (fields)
  (order-fields (merge-alists fields *default-fields*)))
