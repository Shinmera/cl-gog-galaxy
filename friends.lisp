(in-package #:org.shirakumo.fraf.gog-galaxy)

(defmethod friend-p ((user user))
  (gog ifriends-is-friend (interface 'friends) (id user)))

(defmethod persona-name ((user user))
  (gog ifriends-get-friend-persona-name (interface 'friends) (id user)))

(defmethod persona-state ((user user))
  (gog ifriends-get-friend-persona-state (interface 'friends) (id user)))

(defmethod avatar-url ((user user))
  (gog ifriends-get-friend-avatar-url (interface 'friends) (id user)))

(defmethod befriend ((user user))
  (with-listener* (listener)
        (gog:ifriends-send-friend-invitation (interface 'friends) (id user) listener)
    (friend-invitation-send-success (r-user)
      (when (eq user r-user) (return-from listener user)))
    (friend-invitation-send-failure (r-user failure)
      (when (eq user r-user) (error "Failed to send friend request: ~a" failure)))))

(defmethod unfriend ((user user))
  (with-listener* (listener)
        (gog:ifriends-delete-friend (interface 'friends) (id user) listener)
    (friend-delete-success (r-user)
      (when (eq user r-user) (return-from listener user)))
    (friend-delete-failure (r-user failure)
      (when (eq user r-user) (error "Failed to delete friend: ~a" failure)))))

(define-interface friends gog:friends
  (default-avatar-sizes ()
    (gog ifriends-get-default-avatar-criteria interface))

  ((setf default-avatar-sizes) (sizes)
    (gog ifriends-set-default-avatar-criteria interface sizes)
    sizes)
  
  (list-friends ()
    (with-listener* (listener)
          (gog ifriends-request-friend-list interface listener)
      (friend-list-retrieve-success ()
        (loop for i from 0 below (gog ifriends-get-friend-count interface)
              collect (ensure-user (gog ifriends-get-friend-by-index interface i))))
      (friend-list-retrieve-failure (failure)
        (error "Failed to retrieve friend list: ~a" failure)))))
