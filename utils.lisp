(in-package #:org.shirakumo.fraf.gog-galaxy)

(define-interface utils gog:utils
  (show-webpage (url)
    (gog iutils-show-overlay-with-web-page handle url))

  (overlay-visible-p ()
    (gog iutils-is-overlay-visible handle))

  (overlay-state ()
    (gog iutils-get-overlay-state handle))

  (connection-state ()
    (gog iutils-get-gog-services-connection-state handle)))
