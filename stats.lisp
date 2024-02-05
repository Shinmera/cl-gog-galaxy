(in-package #:org.shirakumo.fraf.gog-galaxy)

(defclass achievement ()
  ((name :initarg :name :reader name)
   (achieved-p :initarg :achieved-p :accessor achieved-p)))

(defmethod visible-p ((achievement achievement))
  (gog istats-is-achievement-visible (handle 'stats) (name achievement)))

(defmethod display-name ((achievement achievement))
  (gog istats-get-achievement-display-name (handle 'stats) (name achievement)))

(defmethod description ((achievement achievement))
  (gog istats-get-achievement-description (handle 'stats) (name achievement)))

(defmethod (setf achieved-p) :before (value (achievement achievement))
  (if value
      (gog istats-set-achievement (handle 'stats) (name achievement))
      (gog istats-clear-achievement (handle 'stats) (name achievement))))

(defmethod achieved-p ((name string))
  (cffi:with-foreign-objects ((unlocked-p :bool) (unlock-time :uint32))
    (gog istats-get-achievement (handle 'stats) name unlocked-p unlock-time 0)
    (cffi:mem-ref unlocked-p :bool)))

(defmethod (setf achieved-p) (value (name string))
  (setf (achieved-p (achievement T name)) value))

(defclass leaderboard-entry ()
  ((leaderboard :initarg :leaderboard :reader leaderboard)
   (rank :initarg :rank :reader rank)
   (score :initarg :score :reader score)
   (details :initarg :details :reader details)
   (user :initarg :user :reader user)))

(defclass leaderboard ()
  ((name :initarg :name :reader name)))

(defmethod display-name ((leaderboard leaderboard))
  (gog istats-get-leaderboard-display-name (handle 'stats) (name leaderboard)))

(defmethod sort-method ((leaderboard leaderboard))
  (gog istats-get-leaderboard-sort-method (handle 'stats) (name leaderboard)))

(defmethod display-type ((leaderboard leaderboard))
  (gog istats-get-leaderboard-display-type (handle 'stats) (name leaderboard)))

(defmethod update ((leaderboard leaderboard) score &key details force)
  (with-listener* (listener)
        (if details
            (cffi:with-pointer-to-vector-data (ptr details)
              (gog:istats-set-leaderboard-score-with-details (handle 'stats) (name leaderboard) score ptr (length details) force listener))
            (gog:istats-set-leaderboard-score (handle 'stats) (name leaderboard) score force listener))
    (leaderboard-score-update-success (name r-score old-rank new-rank)
      (when (and (string= name (name leaderboard)) (= score r-score))
        (return-from listener (values new-rank old-rank))))
    (leaderboard-score-update-failure (name r-score failure)
      (when (and (string= name (name leaderboard)) (= score r-score)) (gog-error failure)))))

(defmethod list-entries ((leaderboard leaderboard) &key around users start end)
  (cffi:with-foreign-objects ((rank :uint32)
                              (score :uint32)
                              (details :char 1024)
                              (written :uint32)
                              (user 'gog:id))
    (unless end
      (setf end (cond (around +10)
                      (users 0)
                      (T (gog:istats-get-leaderboard-entry-count (handle 'stats) (name leaderboard))))))
    (unless start
      (setf start (cond (around +10)
                        (users 0)
                        (T 0))))
    (with-listener* (listener)
          (cond (around
                 (gog:istats-request-leaderboard-entries-around-user (handle 'stats) (name leaderboard) start end (id (ensure-user around)) listener))
                (users
                 (cffi:with-foreign-objects ((ids 'gog:id (length users)))
                   (loop for i from 0
                         for user in users
                         do (setf (cffi:mem-aref ids 'gog:id) (id (ensure-user user))))
                   (gog:istats-request-leaderboard-entries-for-users (handle 'stats) (name leaderboard) ids (length users) listener)))
                (T
                 (gog:istats-request-leaderboard-entries-global (handle 'stats) (name leaderboard) start end listener)))
      (leaderboard-entries-retrieve-success (name count)
        (when (string= (name leaderboard) name)
          (return-from listener
            (loop for i from 0 below count
                  do (gog istats-get-requested-leaderboard-entry-with-details
                          (handle 'stats) i rank score details 1024 written user)
                  collect (make-instance 'leaderboard-entry :leaderboard leaderboard
                                                            :rank (cffi:mem-ref rank :uint32)
                                                            :score (cffi:mem-ref score :uint32)
                                                            :details (cffi:foreign-array-to-lisp details '(:array :char 1024))
                                                            :user (ensure-user (cffi:mem-ref user 'gog:id)))))))
      (leaderboard-entries-retrieve-failure (name failure)
        (when (string= (name leaderboard) name) (gog-error failure))))))

(define-interface stats gog:stats
  (achievement (name &optional (user T))
    (let ((user (id (ensure-user user))))
      (with-listener* (listener)
            (gog:istats-request-user-stats-and-achievements handle user listener)
        (user-stats-and-achievements-retrieve-success (r-user)
          (when (= user (id r-user))
            (cffi:with-foreign-objects ((unlocked-p :bool) (unlock-time :uint32))
              (gog istats-get-achievement handle name unlocked-p unlock-time (id (ensure-user user)))
              (return-from listener
                (make-instance 'achievement :name name :achieved-p (cffi:mem-ref unlocked-p :bool))))))
        (user-stats-and-achievements-retrieve-failure (r-user failure)
          (when (= user (id r-user)) (gog-error failure))))))

  (stat (name &optional (user T))
    (let ((user (id (ensure-user user))))
      (with-listener* (listener)
            (gog:istats-request-user-stats-and-achievements handle user listener)
        (user-stats-and-achievements-retrieve-success (r-user)
          (when (= user (id r-user))
            (return-from listener
              (gog istats-get-stat-float handle name (id (ensure-user user))))))
        (user-stats-and-achievements-retrieve-failure (r-user failure)
          (when (= user (id r-user)) (gog-error failure))))))

  ((setf stat) (value name)
    (gog istats-set-stat-float handle name (float value 0f0))
    value)

  (store ()
    (with-listener* (listener)
          (gog istats-store-stats-and-achievements handle listener)
      (user-stats-and-achievements-store-success () (return-from listener T))
      (user-stats-and-achievements-store-failure (failure) (gog-error failure))))

  (reset ()
    (with-listener* (listener)
          (gog istats-reset-stats-and-achievements handle listener)
      (user-stats-and-achievements-store-success () (return-from listener T))
      (user-stats-and-achievements-store-failure (failure) (gog-error failure))))

  (find-leaderboard (name &key (display-name name) (sort-method :descending) (display-type :time-seconds) (if-does-not-exist :error))
    (with-listener* (listener)
          (case if-does-not-exist
            ((NIL :error) (gog:istats-find-leaderboard handle name listener))
            (:create (gog:istats-find-or-create-leaderboard handle name display-name sort-method display-type listener)))
      (leaderboard-retrieve-success (r-name)
        (when (string= name r-name) (return-from listener (make-instance 'leaderboard :name name))))
      (leaderboard-retrieve-failure (r-name failure)
        (when (string= name r-name) (gog-error failure))))))
