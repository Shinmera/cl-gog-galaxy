(in-package #:org.shirakumo.fraf.gog-galaxy)

(define-interface apps gog:apps
  (dlc-installed-p (product-id)
    (gog iapps-is-dlc-installed handle product-id))

  (dlc-owned-p (product-id)
    (gog iapps-is-dlc-owned handle product-id (cffi:null-pointer)))

  (current-game-language (&optional (product-id 0))
    (gog iapps-get-current-game-language handle product-id))

  (current-game-language-code (&optional (product-id 0))
    (gog iapps-get-current-game-language-code handle product-id)))
