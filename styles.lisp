(in-package #:org.shirakumo.text-draw)

(defvar *here* #.(make-pathname :name NIL :type NIL :defaults (or *compile-file-pathname* *load-pathname*)))
(defvar *styles*)

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

(defun background (type)
  (ecase type
    (:transparent #\Space)
    ((T :white)  (code-char #x00A0))
    (:black      (char "█" 0))
    (:dark-gray  (char "░" 0))
    (:gray       (char "▒" 0))
    (:light-gray (char "▓" 0))))

(defun arrow (dir &optional (type :default))
  (char
   (ecase type
     (:head         "⮜⮞⮝⮟↕↔")
     (:light-head   "⮘⮚⮙⮛↕↔")
     (:empty-head   "🢔🢖🢕🢗⬘⬖")
     (:triangle     "⯇⯈⯅⯆⬘⬖")
     (:light        "🠐🠒🠑🠓⭥⭤")
     ((T :normal)   "⭠⭢⭡⭣⭥⭤")
     (:heavy        "🠈🠊🠉🠋⭥⭤")
     (:large        "🠜🠝🠞🠟⭥⭤")
     (:very-heavy   "🠰🠲🠱🠳⭥⭤")
     (:double       "⯬⯮⯭⯯⭥⭤")
     (:circle       "○○○○○○")
     (:full-circle  "●●●●●●")
     (:diamond      "⬦⬦⬦⬦⬦⬦")
     (:full-diamond "⬥⬥⬥⬥⬥⬥"))
   (ecase dir
     ((:left :west) 0)
     ((:right :east) 1)
     ((:up :north) 2)
     ((:down :south) 3)
     ((:up-down :north-south) 4)
     ((:left-right :east-west) 5))))
