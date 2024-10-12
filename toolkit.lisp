(in-package #:org.shirakumo.text-draw)

(defmacro with-normalized-stream ((stream streamish) &body body)
  `(let ((,stream ,streamish))
     (flet ((,thunk (,stream) ,@body))
       (etypecase ,stream
         (stream (,thunk ,stream))
         ((eql T) (,thunk *standard-output*))
         (null (with-output-to-string (,stream)
                 (,thunk ,stream)))))))

(defun table (table &key (stream T) (padding 1) (borders T))
  (with-normalized-stream (stream stream)
    (let* ((columns (length (first table)))
           (widths (append (loop for i from 0 below columns
                                 collect (loop for row in table
                                               maximize (+ (* 2 padding) (length (princ-to-string (nth i row))))))
                           '(0)))
           (values (loop for row in table
                         collect (loop for value in row
                                       for width in widths
                                       collect width collect value))))
      (if borders
          (loop for row = (pop values)
                do (loop for (width val) on row by #'cddr
                         do (format stream "~v{ ~}" padding 0)
                            (format stream "~va" (- width padding padding) val)
                            (format stream "~v{ ~}" padding 0))
                   (format stream "~%")
                while values)
          (loop initially (format stream "┌~{~v{─~}~^┬~:*~}┐~%" widths)
                for row = (pop values)
                do (format stream "│")
                   (loop for (width val) on row by #'cddr
                         do (format stream "~v{ ~}" padding 0)
                            (format stream "~va" (- width padding padding) val)
                            (format stream "~v{ ~}│" padding 0))
                   (format stream "~%")
                   (when values
                     (format stream "├~{~v{─~}~^┼~:*~}┤~%" widths))
                while values
                finally (format stream "└~{~v{─~}~^┴~:*~}┘" widths))))))

(defun tree (root children-fun &key (stream T) (max-depth (or *print-level* 3)) (key #'identity))
  (with-normalized-stream (stream stream)
    (labels ((recurse (node last depth)
               (when last
                 (destructuring-bind (cur . rest) last
                   (dolist (p (reverse rest))
                     (format stream "~:[│  ~;   ~]" p))
                   (format stream "~:[├~;└~]─" cur)))
               (cond ((< depth max-depth)
                      (format stream " ~a~%" (funcall key node))
                      (let ((children (funcall children-fun node)))
                        (when (typep children 'sequence)
                          (loop with max = (1- (length children))
                                for j from 0 to max
                                do (recurse (elt children j) (list* (= max j) last) (1+ depth))))))
                     (t
                      (format stream "...~%")))))
      (recurse root () 0))))

(defun node (inputs outputs &key (stream T) label)
  (with-normalized-stream (stream stream)
    ))

(defun graph (connections &key (stream T))
  (with-normalized-stream (stream stream)
    ))

(defun progress (percentage &key (stream T))
  (with-normalized-stream (stream stream)
    ))

(defun box (text &key (stream T) (width *print-right-margin*) (align :middle))
  (with-normalized-stream (stream stream)
    ))
