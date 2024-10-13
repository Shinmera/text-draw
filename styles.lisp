(in-package #:org.shirakumo.text-draw)

(defvar *here* #.(make-pathname :name NIL :type NIL :defaults (or *compile-file-pathname* *load-pathname*)))
(defvar *styles*)

(defun split (string split)
  (let ((items ()) (out (make-string-output-stream)))
    (flet ((push-item ()
             (let ((string (get-output-stream-string out)))
               (when (string/= "" string)
                 (push string items)))))
      (loop for char across string
            do (if (char= char split)
                   (push-item)
                   (write-char char out))
            finally (push-item))
      (nreverse items))))

(defun load-styles (&optional (file (make-pathname :name "styles" :type "txt" :defaults *here*)))
  (with-open-file (stream file)
    (let ((table (make-hash-table :test 'eq))
          (cols (loop for el in (split (read-line stream) #\Space)
                      collect (intern (string-upcase el) "KEYWORD"))))
      (loop for el in cols
            do (setf (gethash el table) (make-hash-table :test 'eql)))
      (loop for line = (read-line stream NIL NIL)
            while line
            do (destructuring-bind (key . styles) (split line #\Space)
                 (loop for style in styles
                       for col in cols
                       do (setf (gethash (char key 0) (gethash col table)) (char style 0)))))
      table)))

(setf *styles* (load-styles))

(defun style (text style)
  (let ((tab (or (gethash style *styles*)
                 (error "No such style ~s.~%Known styles:~{ ~s~}"
                        style (loop for style being the hash-keys of *styles* collect style)))))
    (flet ((style (char) (gethash char tab char)))
      (map 'string #'style text))))
