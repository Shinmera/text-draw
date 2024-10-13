(in-package #:org.shirakumo.text-draw)

(defun table (table &key stream (padding 1) (borders T))
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

(defun tree (root children-fun &key stream (max-depth (or *print-level* 3)) (key #'identity))
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

(defun node (inputs outputs &key stream label (background :transparent))
  (with-normalized-stream (stream stream)
    (let* ((height (max (length inputs) (length outputs)))
           (igap (truncate (- height (length inputs)) 2))
           (ogap (truncate (- height (length outputs)) 2))
           (ivlen (loop for port in inputs maximize (if (consp port) (length (princ-to-string (cdr port))) 0)))
           (iplen (loop for port in inputs maximize (if (consp port) (length (princ-to-string (car port))) (length (princ-to-string port)))))
           (ovlen (loop for port in outputs maximize (if (consp port) (length (princ-to-string (cdr port))) 0)))
           (oplen (loop for port in outputs maximize (if (consp port) (length (princ-to-string (car port))) (length (princ-to-string port)))))
           (width (+ iplen oplen (if label (+ 1 (length (princ-to-string label))) 1)))
           (bg (char (background background) 0)))
      (format stream "~v{ ~} ┌~v{─~}┐ ~v{ ~}~%" ivlen 0 width 0 ovlen 0)
      (dotimes (i height)
        ;; Print the left hand port
        (cond ((or (< 0 igap) (null inputs))
               (format stream "~v{ ~} │~v@{~a~:*~}" ivlen 0 iplen bg)
               (decf igap))
              ((consp (car inputs))
               (destructuring-bind (port . value) (pop inputs)
                 (format stream "~v@a╶┤~v,,,va" ivlen value iplen bg port)))
              (T
               (format stream "~v{ ~}~* ┤~v,,,va" ivlen 0 iplen bg (pop inputs))))
        ;; Print the label
        (format stream "~v@{~a~:*~}" (- width iplen oplen) bg)
        ;; Print the right hand port
        (cond ((or (< 0 ogap) (null outputs))
               (format stream "~v@{~a~:*~}~*│ ~v{ ~}" oplen bg ovlen 0)
               (decf ogap))
              ((consp (car outputs))
               (destructuring-bind (port . value) (pop outputs)
                 (format stream "~v,,,v@a├╴~va" oplen bg port oplen value)))
              (T
               (format stream "~v,,,v@a├ ~v{ ~}" oplen bg (pop outputs) ovlen 0)))
        (terpri stream))
      (format stream "~v{ ~} └~v{─~}┘ ~v{ ~}" ivlen 0 width 0 ovlen 0))))

(defun align (text alignment &key stream)
  (with-normalized-stream (stream stream)
    (let* ((lines (lines text))
           (width (loop for line in lines maximize (length line))))
      (dolist (line lines)
        (destructuring-bind (l . r) (alignment alignment line width)
          (format stream "~v{ ~}~a~v{ ~}~%" l 0 line r 0))))))

(defun box (text &key stream (width *print-right-margin*) (align :middle) (background :transparent))
  (with-normalized-stream (stream stream)
    (let ((text (if (listp text) text (list text)))
          (bg (background background)))
      (setf text (loop for line in text
                       append (wrap line (if width (- width 2) most-positive-fixnum))))
      (when (or (eql T width) (null width))
        (setf width (loop for line in text maximize (+ 2 (length line)))))
      (cond ((member background '(:transparent :white))
             (format stream "┌~v{─~}┐~%" (- width 2) 0)
             (dolist (line text)
               (destructuring-bind (l . r) (alignment align line (- width 2))
                 (format stream "│~v@{~a~:*~}~*~a~v@{~a~:*~}│~%" l bg line r bg)))
             (format stream "└~v{─~}┘" (- width 2) 0))
            (T
             (format stream "▗~v{▄~}▖~%" (- width 2) 0)
             (dolist (line text)
               (destructuring-bind (l . r) (alignment align line (- width 2))
                 (format stream "▐~v@{~a~:*~}~*~a~v@{~a~:*~}▌~%" l bg line r bg)))
             (format stream "▝~v{▀~}▘" (- width 2) 0))))))

(defun progress (percentage &key stream (width *print-right-margin*) (label T))
  (with-normalized-stream (stream stream)
    (let* ((width (or width 80))
           (bar-width (max 0 (if label (- width 5) width)))
           (full (max 0 (min bar-width (floor (* percentage 1/100 bar-width)))))
           (empty (- bar-width full)))
      (format stream "~v{█~}~v{░~}" full 0 empty 0)
      (when label (format stream " ~3d%" (floor percentage))))))

(defun check (filled &key stream label)
  (with-normalized-stream (stream stream)
    (format stream "~:[☐~;☑~]~@[ ~a~]" filled label)))

(defun radio (filled &key stream label)
  (with-normalized-stream (stream stream)
    (format stream "~:[⦾~;⦿~]~@[ ~a~]" filled label)))

(defun rows (&rest parts)
  (format NIL "~{~a~^~%~}" parts))

(defun horizontal-line (width height &key stream (bend :middle))
  (with-normalized-stream (stream stream)
    (when (< width 0) (setf width (- width) height (- height)))
    (let* ((l (ecase bend
                ((:left :start) 1)
                ((:right :end) (- width 2))
                ((:middle :center) (truncate width 2))))
           (r (- width l)))
      (cond ((< +1 height)
             (format stream "~v{─~}╮~v{ ~}~%" l 0 r 0)
             (dotimes (i (- height 2))
               (format stream "~v{ ~}│~v{ ~}~%" l 0 r 0))
             (format stream "~v{ ~}╰~v{─~}" l 0 r 0))
            ((< height -1)
             (format stream "~v{ ~}╭~v{─~}~%" l 0 r 0)
             (dotimes (i (- (- height) 2))
               (format stream "~v{ ~}│~v{ ~}~%" l 0 r 0))
             (format stream "~v{─~}╯~v{ ~}" l 0 r 0))
            (T
             (format stream "~v{─~}" width 0))))))

(defun vertical-line (width height &key stream (bend :middle))
  (with-normalized-stream (stream stream)
    (when (< height 0) (setf width (- width) height (- height)))
    (let* ((u (ecase bend
                ((:top :start) 1)
                ((:bottom :end) (- height 2))
                ((:middle :center) (truncate height 2))))
           (b (- height u 1)))
      (cond ((< +1 width)
             (dotimes (i u)
               (format stream "│~v{ ~}~%" (- width 1) 0))
             (format stream "╰~v{─~}╮~%" (- width 2) 0)
             (dotimes (i b)
               (format stream "~v{ ~}│~%" (- width 1) 0)))
            ((< width -1)
             (dotimes (i u)
               (format stream "~v{ ~}│~%" (- (- width) 1) 0))
             (format stream "╭~v{─~}╯~%" (- (- width) 2) 0)
             (dotimes (i b)
               (format stream "│~v{ ~}~%" (- (- width) 1) 0)))
            (T
             (format stream "~v{│~^~%~}" height 0))))))

(defun line (width height &rest args)
  (if (<= (abs height) (abs width))
      (apply #'horizontal-line width height args)
      (apply #'vertical-line width height args)))

(defun translate (string x y &key stream)
  (with-normalized-stream (stream stream)
    (let ((width (with-input-from-string (in string)
                   (loop for line = (read-line in NIL NIL)
                         while line maximize (length line)))))
      (dotimes (i y)
        (format stream "~v{ ~}~%" (+ (abs x) width) 0))
      (with-input-from-string (in string)
        (loop for line = (read-line in NIL NIL)
              while line
              do (format stream "~v{ ~}~va~v{ ~}~%" x 0 width line (- x) 0)))
      (dotimes (i (- y))
        (format stream "~v{ ~}~%" (+ (abs x) width) 0)))))

(defun composite (a b &key a-offset b-offset stream)
  (let ((a (destructuring-bind (x . y) (or a-offset '(0 . 0))
             (translate a x y :stream NIL)))
        (b (destructuring-bind (x . y) (or b-offset '(0 . 0))
             (translate b x y :stream NIL))))
    (with-normalized-stream (stream stream)
      (with-input-from-string (a a)
        (with-input-from-string (b b)
          (loop for al = (read-line a NIL)
                for bl = (read-line b NIL)
                do (cond ((and al bl)
                          (loop for i from 0 below (min (length al) (length bl))
                                for ac = (char al i) for bc = (char bl i)
                                do (write-char (if (char= #\Space bc) ac bc) stream))
                          (loop for i from (min (length al) (length bl)) below (length al)
                                do (write-char (char al i) stream))
                          (loop for i from (min (length al) (length bl)) below (length bl)
                                do (write-char (char bl i) stream))
                          (terpri stream))
                         (al (write-line al stream))
                         (bl (write-line bl stream))
                         (T (return)))))))))