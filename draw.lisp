(in-package #:org.shirakumo.text-draw)

(defun fill (width height &key stream (background :black))
  (with-normalized-stream (stream stream)
    (let ((bg (background background)))
      (format stream "~v@{~v@{~a~:*~}~:*~%~}" height width bg))))

(defun table (table &key stream (padding 1) (borders T))
  ;; TODO: background
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
                finally (format stream "└~{~v{─~}~^┴~:*~}┘" widths))
          (loop for row = (pop values)
                do (loop for (width val) on row by #'cddr
                         do (format stream "~v{ ~}" padding 0)
                            (format stream "~va" (- width padding padding) val)
                            (format stream "~v{ ~}" padding 0))
                   (format stream "~%")
                while values)))))

(defun tree (root children-fun &key stream (max-depth (or *print-level* 3)) (key #'identity))
  (with-normalized-stream (stream stream)
    (let ((max-depth (etypecase max-depth
                       (integer max-depth)
                       ((member NIL T) most-positive-fixnum))))
      (labels ((recurse (node last depth)
                 (when last
                   (destructuring-bind (cur . rest) last
                     (dolist (p (reverse rest))
                       (format stream "~:[│  ~;   ~]" p))
                     (format stream "~:[├~;└~]─" cur)))
                 (cond ((< depth max-depth)
                        (format stream "~@< ~@;~a~;~:>~%" (funcall key node))
                        (let ((children (funcall children-fun node)))
                          (when (typep children 'sequence)
                            (loop with max = (1- (length children))
                                  for j from 0 to max
                                  do (recurse (elt children j) (list* (= max j) last) (1+ depth))))))
                       (t
                        (format stream "...~%")))))
        (recurse root () 0)))))

(defun node (inputs outputs &key stream label (background :white))
  (with-normalized-stream (stream stream)
    (let* ((height (max (length inputs) (length outputs)))
           (igap (truncate (- height (length inputs)) 2))
           (ogap (truncate (- height (length outputs)) 2))
           (ivlen (loop for port in inputs maximize (if (consp port) (length (princ-to-string (cdr port))) 0)))
           (iplen (loop for port in inputs maximize (if (consp port) (length (princ-to-string (car port))) (length (princ-to-string port)))))
           (ovlen (loop for port in outputs maximize (if (consp port) (length (princ-to-string (cdr port))) 0)))
           (oplen (loop for port in outputs maximize (if (consp port) (length (princ-to-string (car port))) (length (princ-to-string port)))))
           (label (when label (princ-to-string label)))
           (width (+ iplen oplen (if label (+ 2 (length label)) 1)))
           (bg (background background)))
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
               (format stream "~v{ ~} ┤~v,,,va" ivlen 0 iplen bg (pop inputs))))
        ;; Print the label
        (if (and label (= i (floor height 2)))
            (format stream "~a~a~a" bg label bg)
            (format stream "~v@{~a~:*~}" (- width iplen oplen) bg))
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

(defun box (text &key stream (width *print-right-margin*) (align :middle) (background :white))
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

(defun pad (text size &key stream (background :white))
  (with-normalized-stream (stream stream)
    (destructuring-bind (l &optional (u l) (r l) (b u)) (if (listp size) size (list size))
      (let* ((bg (background background))
             (lines (lines text))
             (width (loop for line in lines maximize (length line))))
        (format stream "~v@{~v@{~a~:*~}~:*~%~}" u (+ l width r) bg)
        (dolist (line lines)
          (format stream "~v@{~a~:*~}~*~v,,,va~v@{~a~:*~}~%" l bg width bg line r bg))
        (format stream "~v@{~v@{~a~:*~}~:*~%~}" b (+ l width r) bg)))))

(defun check (filled &key stream label)
  (with-normalized-stream (stream stream)
    (format stream "~:[☐~;☑~]~@[ ~a~]" filled label)))

(defun radio (filled &key stream label)
  (with-normalized-stream (stream stream)
    (format stream "~:[⦾~;⦿~]~@[ ~a~]" filled label)))

(defun rows (&rest parts)
  (format NIL "~{~a~^~%~}" parts))

(defun horizontal-line (width height &key stream (bend :middle) start end)
  (with-normalized-stream (stream stream)
    (when (< width 0) (setf width (- width) height (- height)))
    (cond ((< 1 width)
           (when start (decf width))
           (when end (decf width))
           (let* ((l (ecase bend
                       ((:left :start) 1)
                       ((:right :end) (- width 2))
                       ((:middle :center) (truncate width 2))))
                  (r (- width l 1))
                  (lpad l) (rpad r))
             (cond ((< +1 height)
                    (when start (incf lpad))
                    (when end (incf rpad))
                    (format stream "~@[~a~]~v{─~}╮~v{ ~}~%" (when start (arrow :left start)) l 0 rpad 0)
                    (dotimes (i (- height 2))
                      (format stream "~v{ ~}│~v{ ~}~%" lpad 0 rpad 0))
                    (format stream "~v{ ~}╰~v{─~}~@[~a~]" lpad 0 r 0 (when end (arrow :right end))))
                   ((< height -1)
                    (when end (incf lpad))
                    (when start (incf rpad))
                    (format stream "~v{ ~}╭~v{─~}~@[~a~]~%" lpad 0 r 0 (when start (arrow :right start)))
                    (dotimes (i (- (- height) 2))
                      (format stream "~v{ ~}│~v{ ~}~%" lpad 0 rpad 0))
                    (format stream "~@[~a~]~v{─~}╯~v{ ~}" (when end (arrow :left end)) l 0 rpad 0))
                   (T
                    (when start (format stream "~a" (arrow :left start)))
                    (format stream "~v{─~}" width 0)
                    (when end (format stream "~a" (arrow :right end)))))))
          ((= 1 height)
           (format stream "~a~%" (cond ((and start end) (arrow :left-right start)) (start (arrow :left start)) (end (arrow :right end)) (T "─"))))
          ((= -1 height)
           (format stream "~a~%" (cond ((and start end) (arrow :left-right start)) (start (arrow :right start)) (end (arrow :left end)) (T "─"))))
          ((= 1 width)
           (if (< 0 height)
               (format stream "╮~v{~%│~}~%╰~%" (- height 2) 0)
               (format stream "╭~v{~%│~}~%╯~%" (- (- height) 2) 0))))))

(defun vertical-line (width height &key stream (bend :middle) start end)
  (with-normalized-stream (stream stream)
    (when (< height 0) (setf width (- width) height (- height)))
    (cond ((< 1 height)
           (when start (decf height))
           (when end (decf height))
           (let* ((u (ecase bend
                       ((:top :start) 1)
                       ((:bottom :end) (- height 2))
                       ((:middle :center) (truncate height 2))))
                  (b (- height u 1)))
             (cond ((< +1 width)
                    (when start (format stream "~a~v{ ~}~%" (arrow :up start) (- width 1) 0))
                    (dotimes (i u)
                      (format stream "│~v{ ~}~%" (- width 1) 0))
                    (format stream "╰~v{─~}╮~%" (- width 2) 0)
                    (dotimes (i b)
                      (format stream "~v{ ~}│~%" (- width 1) 0))
                    (when end (format stream "~v{ ~}~a~%" (- width 1) 0 (arrow :down end))))
                   ((< width -1)
                    (when start (format stream "~v{ ~}~a~%" (- (- width) 1) 0 (arrow :up start)))
                    (dotimes (i u)
                      (format stream "~v{ ~}│~%" (- (- width) 1) 0))
                    (format stream "╭~v{─~}╯~%" (- (- width) 2) 0)
                    (dotimes (i b)
                      (format stream "│~v{ ~}~%" (- (- width) 1) 0))
                    (when end (format stream "~a~v{ ~}~%" (arrow :down end) (- (- width) 1) 0)))
                   (T
                    (when start (format stream "~a~%" (arrow :up start)))
                    (format stream "~v{│~^~%~}" height 0)
                    (when end (format stream "~a~%" (arrow :down end)))))))
          ((= 1 width)
           (format stream "~a~%" (cond ((and start end) (arrow :up-down start)) (start (arrow :up start)) (end (arrow :down end)) (T "│"))))
          ((= -1 width)
           (format stream "~a~%" (cond ((and start end) (arrow :up-down start)) (start (arrow :down start)) (end (arrow :up end)) (T "│"))))
          ((= 1 height)
           (if (< 0 width)
               (format stream "╰~v{─~}╮~%" (- width 2) 0)
               (format stream "╭~v{─~}╯~%" (- (- width) 2) 0))))))

(defun line (width height &rest args)
  (if (<= (abs height) (abs width))
      (apply #'horizontal-line width height args)
      (apply #'vertical-line width height args)))

(defun plot (function &key (width (or *print-right-margin* 80)) (height 15)
                           (left 0) (right 1) bottom top stream (background :white))
  (with-normalized-stream (stream stream)
    (let* ((plot-width (- width 2))
           (plot-height (- height 2))
           (background (background background))
           (samples (loop for x from 0 below plot-width
                          collect (funcall function (float (+ left (* (/ x (1- plot-width)) (- right left))) 0f0)))))
      (format stream "~&~5,2@f~v@{▁~} ~%" top (- plot-width 4) 0)
      (unless bottom (setf bottom (loop for sample in samples minimize sample)))
      (unless top (setf top (loop for sample in samples maximize sample)))
      (loop for i downfrom height to 0
            for threshold = (/ i plot-height)
            do (format stream "▕")
               (loop for sample in samples
                     for normalized = (- (/ sample (- top bottom)) bottom)
                     for subcell = (* plot-height (- sample threshold))
                     for clamped = (min 1.0 (max 0.0 subcell))
                     do (format stream "~[~a~;▁~;▂~;▃~;▄~;▅~;▆~;▇~;█~]" (round (* 8 clamped)) background))
               (format stream "▏~%"))
      (format stream "~5,2@f/~5,2@f~v@{▔~}~5,2@f" bottom left (- plot-width 14) right))))

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

(defmacro compose (stream &body parts)
  (if (cddr parts)
      (destructuring-bind ((x y part) . parts) parts
        `(composite ,part (compose NIL ,@parts)
                    :a-offset (cons ,x ,y) :stream ,stream))
      (destructuring-bind ((ax ay a) (bx by b)) parts
        `(composite ,a ,b :a-offset (cons ,ax ,ay) :b-offset (cons ,bx ,by) :stream ,stream))))
