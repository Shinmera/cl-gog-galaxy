(in-package #:org.shirakumo.fraf.gog-galaxy)

(defmethod joinable-p ((lobby lobby))
  (gog imatchmaking-is-lobby-joinable (handle 'matchmaking) (id lobby)))

(defmethod (setf joinable-p) (value (lobby lobby))
  (with-listener* (listener)
        (gog imatchmaking-set-lobby-joinable (handle 'matchmaking) (id lobby) value listener)
    (lobby-data-update-success (r-lobby)
      (when (eq lobby r-lobby) (return-from listener value)))
    (lobby-data-update-failure (r-lobby failure)
      (when (eq lobby r-lobby) (error "Failed to set joinable status: ~a" failure)))))

(defmethod kind ((lobby lobby))
  (gog imatchmaking-get-lobby-type (handle 'matchmaking) (id lobby)))

(defmethod (setf kind) (kind (lobby lobby))
  (with-listener* (listener)
        (gog imatchmaking-set-lobby-type (handle 'matchmaking) (id lobby) kind listener)
    (lobby-data-update-success (r-lobby)
      (when (eq lobby r-lobby) (return-from listener kind)))
    (lobby-data-update-failure (r-lobby failure)
      (when (eq lobby r-lobby) (error "Failed to set lobby type: ~a" failure)))))

(defmethod join ((lobby lobby))
  (with-listener* (listener)
        (gog imatchmaking-join-lobby (handle 'matchmaking) (id lobby) listener)
    (lobby-entered (r-lobby result)
      (when (eq lobby r-lobby)
        (case result
          (:success (return-from listener lobby))
          (T (error "Failed to join lobby: ~a" result)))))))

(defmethod leave ((lobby lobby))
  (with-listener* (listener)
        (gog imatchmaking-leave-lobby (handle 'matchmaking) (id lobby) listener)
    (lobby-left (r-lobby result)
      (when (eq lobby r-lobby)
        (case result
          (:success (return-from listener lobby))
          (T (error "Failed to leave lobby: ~a" result)))))))

(defmethod max-members ((lobby lobby))
  (gog imatchmaking-get-max-num-lobby-members (handle 'matchmaking) (id lobby)))

(defmethod (setf max-members) (max (lobby lobby))
  (with-listener* (listener)
        (gog imatchmaking-set-max-num-lobby-members (handle 'matchmaking) (id lobby) max listener)
    (lobby-data-update-success (r-lobby)
      (when (eq lobby r-lobby) (return-from listener max)))
    (lobby-data-update-failure (r-lobby failure)
      (when (eq lobby r-lobby) (error "Failed to set max members: ~a" failure)))))

(defmethod member-count ((lobby lobby))
  (gog imatchmaking-get-num-lobby-members (handle 'matchmaking) (id lobby)))

(defmethod list-members ((lobby lobby))
  (let ((interface (handle 'matchmaking))
        (id (id lobby)))
    (loop for i from 0 below (gog imatchmaking-get-num-lobby-members interface id)
          collect (ensure-lobby (gog imatchmaking-get-lobby-member-by-index interface id i)))))

(defmethod owner ((lobby lobby))
  (ensure-user (gog imatchmaking-get-lobby-owner (handle 'matchmaking) (id lobby))))

(defmethod send-message ((text string) (lobby lobby))
  (cffi:with-foreign-string ((ptr len) text)
    (if (gog imatchmaking-send-lobby-message (handle 'matchmaking) (id lobby) ptr len)
        lobby
        (error "Failed to send message."))))

(defmethod get-message (i (lobby lobby))
  (cffi:with-foreign-objects ((sender 'gog:id)
                              (buffer :char 512))
    (let ((len (gog:imatchmaking-get-lobby-message (handle 'matchmaking) (id lobby) i sender buffer 512)))
      (if (< 0 len)
          (cffi:foreign-string-to-lisp buffer :count len)
          (error "Failed to get message.")))))

(defmethod data ((lobby lobby))
  (let ((interface (handle 'matchmaking))
        (id (id lobby)))
    (cffi:with-foreign-objects ((key :char 512) (val :char 512))
      (with-listener* (listener)
            (gog:imatchmaking-request-lobby-data interface id listener)
        (lobby-data-retrieve-success (r-lobby)
          (when (eq lobby r-lobby)
            (return-from listener
              (loop for i from 0 below (gog:imatchmaking-get-lobby-data-count interface id)
                    do (unless (gog:imatchmaking-get-lobby-data-by-index interface id i key 512 val 512)
                         (error "Failed to retrieve lobby data."))
                    collect (cons (cffi:mem-ref key :string) (cffi:mem-ref val :string))))))
        (lobby-data-retrieve-failure (r-lobby failure)
          (when (eq lobby r-lobby) (error "Failed to retrieve lobby data: ~a" failure)))))))

(defmethod (setf data) (data (lobby lobby))
  (loop for (key . val) in data
        do (setf (data-field key lobby) val))
  data)

(defmethod data-field (key (lobby lobby))
  (gog:imatchmaking-get-lobby-data (handle 'matchmaking) (id lobby) key))

(defmethod (setf data-field) (value key (lobby lobby))
  (with-listener* (listener)
        (if value
            (gog:imatchmaking-set-lobby-data (handle 'matchmaking) (id lobby) key value listener)
            (gog:imatchmaking-delete-lobby-data (handle 'matchmaking) (id lobby) key listener))
    (lobby-data-update-success (r-lobby)
      (when (eq lobby r-lobby) (return-from listener value)))
    (lobby-data-update-failure (r-lobby failure)
      (when (eq lobby r-lobby) (error "Failed to update lobby data: ~a" failure)))))

(defmethod member-data ((lobby lobby) (member user))
  (let ((interface (handle 'matchmaking))
        (id (id lobby))
        (mid (id member)))
    (cffi:with-foreign-objects ((key :char 512) (val :char 512))
      (with-listener* (listener)
            (gog:imatchmaking-request-lobby-data interface id listener)
        (lobby-data-retrieve-success (r-lobby)
          (when (eq lobby r-lobby)
            (return-from listener
              (loop for i from 0 below (gog:imatchmaking-get-lobby-member-data-count interface id mid)
                    do (unless (gog:imatchmaking-get-lobby-member-data-by-index interface id mid i key 512 val 512)
                         (error "Failed to retrieve lobby data."))
                    collect (cons (cffi:mem-ref key :string) (cffi:mem-ref val :string))))))
        (lobby-data-retrieve-failure (r-lobby failure)
          (when (eq lobby r-lobby) (error "Failed to retrieve lobby data: ~a" failure)))))))

(defmethod (setf member-data) (data (lobby lobby) (member user))
  (loop for (key . val) in data
        do (setf (member-data-field key lobby member) val))
  data)

(defmethod member-data-field (key (lobby lobby) (member user))
  (gog:imatchmaking-get-lobby-member-data (handle 'matchmaking) (id lobby) (id member) key))

(defmethod (setf member-data-field) (value key (lobby lobby) (member user))
  (unless (self-p member) (error "Can only set member data for yourself."))
  (with-listener* (listener)
        (if value
            (gog:imatchmaking-set-lobby-member-data (handle 'matchmaking) (id lobby) key value listener)
            (gog:imatchmaking-delete-lobby-member-data (handle 'matchmaking) (id lobby) key listener))
    (lobby-member-data-update-success (r-lobby r-member)
      (when (and (eq lobby r-lobby) (eq member r-member)) (return-from listener value)))
    (lobby-member-data-update-failure (r-lobby r-member failure)
      (when (and (eq lobby r-lobby) (eq member r-member)) (error "Failed to update lobby data: ~a" failure)))))

(define-interface matchmaking gog:matchmaking
  (create-lobby (&key (type :private) (max-members 16) (joinable T) (topology-type :fcm) enter-listener)
    (with-listener* (listener)
          (gog imatchmaking-create-lobby handle type max-members joinable topology-type listener (or enter-listener (cffi:null-pointer)))
      (lobby-created (lobby result)
        (case result
          (:success
           (return-from listener (ensure-lobby lobby)))
          (T (error "Failed to create lobby: ~a" result))))))

  (list-lobbies (&key (allow-full T) limit filters)
    (when limit (gog imatchmaking-add-request-lobby-list-result-count-filter handle limit))
    (loop for (comp key value) in filters
          do (etypecase value
               (string (gog imatchmaking-add-request-lobby-list-string-filter handle key value comp))
               (integer (gog imatchmaking-add-request-lobby-list-numerical-filter handle key value comp))))
    (with-listener* (listener)
          (gog imatchmaking-request-lobby-list handle allow-full listener)
      (lobby-list (count result)
        (case result
          (:success
           (return-from listener
             (loop for i from 0 below count
                   collect (ensure-lobby (gog imatchmaking-get-lobby-by-index handle i)))))
          (T (error "Failed to list lobbies: ~a" result)))))))
