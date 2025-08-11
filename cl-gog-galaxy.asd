(asdf:defsystem cl-gog-galaxy
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A wrapper for the GOG Galaxy SDK"
  :homepage "https://shinmera.com/docs/cl-gog-galaxy"
  :bug-tracker "https://shinmera.com/project/cl-gog-galaxy/issues"
  :source-control (:git "https://shinmera.com/project/cl-gog-galaxy.git")
  :serial T
  :components ((:file "package")
               (:file "low-level")
               (:file "conditions")
               (:file "c-object")
               (:file "id")
               (:file "listener")
               (:file "wrapper")
               (:file "apps")
               (:file "chat")
               (:file "cloud-storage")
               (:file "friends")
               (:file "matchmaking")
               (:file "networking")
               (:file "stats")
               (:file "storage")
               (:file "telemetry")
               (:file "user")
               (:file "utils")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :pathname-utils
               :trivial-features
               :trivial-indent
               :cffi))
