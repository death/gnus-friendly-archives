(defun build-mbox (mbox-filename msg-filenames)
  (with-open-file (out mbox-filename
                       :direction :output
                       :if-exists :supersede
                       :external-format :latin-1)
    (dolist (msg-filename msg-filenames)
      (multiple-value-bind (fields text)
          (parse-msg msg-filename)
        (when (and fields text)
          (loop for (key . val) in (augment-fields fields)
                do (format out "~A: ~A~%" key val))
          (format out "~%~A~2%" text))))))

(defun msg-filenames (maillist-filename)
  (let ((doc (chtml:parse (pathname maillist-filename)
                          (chtml:make-lhtml-builder))))
    (reverse
     (mapcar (lambda (filename)
               (merge-pathnames filename maillist-filename))
             (remove-if-not #'msg-filename-p (collect-hrefs '() doc))))))

(defun msg-filename-p (x)
  (alexandria:starts-with-subseq "msg" x))

(defun collect-hrefs (result doc)
  (cond ((consp doc)
         (if (eq (car doc) :a)
             (cons (cadr (assoc :href (cadr doc))) result)
             (reduce #'collect-hrefs (cddr doc) :initial-value result)))
        (t result)))

(defun parse-msg (filename)
  (let ((doc (chtml:parse (pathname filename)
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

(defun doc-text (doc)
  (cadr (follow doc '(:html :body :pre))))

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
  '(("From" . "rpg")
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
