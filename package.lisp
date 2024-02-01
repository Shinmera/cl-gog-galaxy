(defpackage #:org.shirakumo.fraf.gog-galaxy.cffi
  (:use #:cl)
  (:export))

(defpackage #:org.shirakumo.fraf.gog-galaxy
  (:use #:cl)
  (:local-nicknames
   (#:gog #:org.shirakumo.fraf.gog-galaxy.cffi))
  (:export))
