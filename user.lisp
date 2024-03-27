(in-package #:org.shirakumo.fraf.gog-galaxy)

(define-interface users gog:user
  (signed-in-p ()
    (gog iuser-signed-in handle))

  (sign-in (&key username password refresh-token (launcher T) steam-app-ticket galaxy ps4-client-id xbox-one-user-id xbox-id anonymous server-key authorization-code)
    (with-listener* (listener)
        (cond ((and username password)
               (gog iuser-sign-in-credentials handle username password listener))
              (refresh-token
               (gog iuser-sign-in-token handle refresh-token listener))
              (steam-app-ticket
               (cffi:with-foreign-array (app-ticket steam-app-ticket '(:array :uint8))
                 (gog iuser-sign-in-steam handle app-ticket (length steam-app-ticket) username listener)))
              (galaxy
               (gog iuser-sign-in-galaxy handle NIL 30 listener))
              (ps4-client-id
               (gog iuser-sign-in-ps4 handle ps4-client-id listener))
              (xbox-one-user-id
               (gog iuser-sign-in-xb1 handle xbox-one-user-id listener))
              (xbox-id
               (gog iuser-sign-in-xbox handle xbox-id listener))
              (anonymous
               (gog iuser-sign-in-anonymous handle listener))
              (server-key
               (gog iuser-sign-in-server-key handle server-key listener))
              (authorization-code
               (gog iuser-sign-in-authorization-code handle authorization-code (cffi:null-pointer) listener))
              (launcher
               (gog iuser-sign-in-launcher handle listener))
              (T
               (gog-error NIL "Must specify a login method.")))
      (auth-success () (return-from listener T))
      (auth-failure (failure) (gog-error failure))))

  (sign-out ()
    (gog iuser-sign-out handle))

  (logged-on-p ()
    (gog iuser-is-logged-on handle))

  (session-id ()
    (gog iuser-get-session-id handle))

  (access-token ()
    (gog iuser-get-access-token handle))

  (refresh-token ()
    (gog iuser-get-refresh-token handle))

  (id-token ()
    (gog iuser-get-idtoken handle))

  (user-data (key &optional (user T))
    (let ((id (id (ensure-user user))))
      (unless (gog iuser-is-user-data-available handle id)
        (with-listener* (listener)
            (gog iuser-request-user-data handle id listener)
          (specific-user-data-updated (user) (when (= id (id user)) (return-from listener T)))))
      (gog iuser-get-user-data handle key id)))

  ((setf user-data) (value key)
    (etypecase value
      (string (gog iuser-set-user-data handle key value (cffi:null-pointer)))
      (null (gog iuser-delete-user-data handle key (cffi:null-pointer))))))
