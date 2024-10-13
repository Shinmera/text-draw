(asdf:defsystem text-draw
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A utility library to draw nice presentations in pure text."
  :homepage "https://shinmera.github.io/text-draw"
  :bug-tracker "https://github.com/Shinmera/text-draw/issues"
  :source-control (:git "https://github.com/Shinmera/text-draw.git")
  :serial T
  :components ((:file "package")
               (:file "styles")
               (:file "toolkit")
               (:file "documentation"))
  :depends-on (:documentation-utils))
