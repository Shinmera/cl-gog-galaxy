(in-package #:org.shirakumo.fraf.gog-galaxy)

(define-interface utils gog:utils
  (show-webpage (url)
    (check-error (gog:iutils-show-overlay-with-web-page interface url)))

  (overlay-visible-p ()
    (check-error (gog:iutils-is-overlay-visible interface)))

  (overlay-state ()
    (check-error (gog:iutils-get-overlay-state interface)))

  (connection-state ()
    (check-error (gog:iutils-get-gog-services-connection-state interface))))
