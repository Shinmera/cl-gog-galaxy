(in-package #:org.shirakumo.fraf.gog-galaxy)

(define-interface utils gog:utils
  (show-webpage (url)
    (gog iutils-show-overlay-with-web-page interface url))

  (overlay-visible-p ()
    (gog iutils-is-overlay-visible interface))

  (overlay-state ()
    (gog iutils-get-overlay-state interface))

  (connection-state ()
    (gog iutils-get-gog-services-connection-state interface)))
