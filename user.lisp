(in-package #:org.shirakumo.fraf.gog-galaxy)

(define-interface user gog:user
  (signed-in-p ()
    (check-error (gog:iuser-signed-in interface)))

  (sign-in (&key username password refresh-token launcher steam-app-ticket galaxy ps4-client-id xbox-one-user-id xbox-id anonymous server-key authorization-code)
    (with-listener* (listener)
        (cond ((and username password)
               (gog iuser-sign-in-credentials interface username password listener))
              (refresh-token
               (gog iuser-sign-in-token interface refresh-token listener))
              (launcher
               (gog iuser-sign-in-launcher interface listener))
              (steam-app-ticket
               (cffi:with-foreign-array (app-ticket steam-app-ticket '(:array :uint8))
                 (gog iuser-sign-in-steam interface app-ticket (length steam-app-ticket) username listener)))
              (galaxy
               (gog iuser-sign-in-galaxy interface NIL 30 listener))
              (ps4-client-id
               (gog iuser-sign-in-ps4 interface ps4-client-id listener))
              (xbox-one-user-id
               (gog iuser-sign-in-xb1 interface xbox-one-user-id listener))
              (xbox-id
               (gog iuser-sign-in-xbox interface xbox-id listener))
              (anonymous
               (gog iuser-sign-in-anonymous interface listener))
              (server-key
               (gog iuser-sign-in-server-key interface server-key listener))
              (authorization-code
               (gog iuser-sign-in-authorization-code interface authorization-code (cffi:null-pointer) listener))
              (T
               (error "Must specify a login method.")))
      (auth-success () (return-from listener T))
      (auth-failure (failure) (error "Authentication failed: ~a" failure))))

  (sign-out ()
    (gog iuser-sign-out interface))

  (logged-on-p ()
    (gog iuser-is-logged-on interface))

  (session-id ()
    (gog iuser-get-session-id interface))

  (access-token ()
    (gog iuser-get-access-token interface))

  (refresh-token ()
    (gog iuser-get-refresh-token interface))

  (id-token ()
    (gog iuser-get-idtoken interface))

  (user-data (key &optional (user T))
    (let ((id (id (ensure-user user))))
      (unless (gog iuser-is-user-data-available interface id)
        (with-listener* (listener)
            (gog iuser-request-user-data interface id (cffi:null-pointer))
          (specific-user-data-updated (user) (when (= id (id user)) (return-from listener T)))))
      (gog iuser-get-user-data interface key id)))

  ((setf user-data) (value key)
    (etypecase value
      (string (gog iuser-set-user-data interface key value (cffi:null-pointer)))
      (null (gog iuser-delete-user-data interface key (cffi:null-pointer))))))
