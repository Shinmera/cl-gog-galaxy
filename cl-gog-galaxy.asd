(asdf:defsystem cl-gog-galaxy
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A wrapper for the GOG Galaxy SDK"
  :homepage "https://github.com/Shinmera/cl-gog-galaxy"
  :serial T
  :components ((:file "package")
               (:file "low-level")
               (:file "conditions")
               (:file "c-object")
               (:file "id")
               (:file "listener")
               (:file "wrapper")
               (:file "apps")
               (:file "stats")
               (:file "user")
               (:file "utils")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :trivial-features
               :trivial-indent
               :cffi))
