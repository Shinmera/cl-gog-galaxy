(in-package #:org.shirakumo.fraf.gog-galaxy)

(define-interface apps gog:apps
  (dlc-installed-p (product-id)
    (check-error (gog:iapps-is-dlc-installed interface product-id)))

  (dlc-owned-p (product-id)
    (check-error (gog:iapps-is-dlc-owned interface product-id (cffi:null-pointer))))

  (current-game-language (&optional (product-id 0))
    (check-error (gog:iapps-get-current-game-language interface product-id)))

  (current-game-language-code (&optional (product-id 0))
    (check-error (gog:iapps-get-current-game-language-code interface product-id))))
