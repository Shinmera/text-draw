(asdf:defsystem text-draw
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A utility library to draw nice presentations in pure text."
  :homepage "https://shinmera.com/docs/text-draw"
  :bug-tracker "https://shinmera.com/project/text-draw/issues"
  :source-control (:git "https://shinmera.com/project/text-draw.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "styles")
               (:file "draw")
               (:file "documentation"))
  :depends-on (:documentation-utils))
