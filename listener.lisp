(in-package #:org.shirakumo.fraf.gog-galaxy)

(defvar *callback-funs* (make-hash-table :test 'eql))
(defvar *global-listeners* (make-hash-table :test 'eql))
(defvar *listener-timeout* 10)

(defclass listener (c-registered-object)
  ((listener-type :initarg :listener-type :reader listener-type)
   (registered :initform NIL :accessor registered)))

(defmethod initialize-instance :after ((listener listener) &key register)
  (when register
    (gog ilistener-registrar-register (gog:listener-registrar) (listener-type listener) (handle listener))
    (setf (registered listener) T)))

(defmethod allocate-handle ((listener listener) &key)
  (cffi:with-foreign-object (struct '(:struct gog:listener))
    (loop for fun being the hash-keys of *callback-funs* using (hash-value field)
          do (setf (cffi:foreign-slot-value struct '(:struct gog:listener) field) (cffi:get-callback fun)))
    (setf (gog:listener-userptr struct) (cffi:null-pointer))
    (gog:make-listener struct (listener-type listener))))

(defmethod free-handle-function ((listener listener) handle)
  (let ((listener-type (listener-type listener))
        (registrar (gog:listener-registrar)))
    (lambda ()
      (gog:ilistener-registrar-unregister registrar listener-type handle)
      (gog:free-listener handle listener-type))))

(defclass global-listener (listener)
  ())

(defmethod initialize-instance :before ((listener global-listener) &key)
  (when (gethash (type-of listener) *global-listeners*)
    (error "An instance of ~s already exists!" (type-of listener))))

(defmethod initialize-instance :after ((listener global-listener) &key)
  (setf (gethash (type-of listener) *global-listeners*) listener))

(defmethod free-handle-function ((listener global-listener) handle)
  (let ((fun (call-next-method)))
    (lambda ()
      (setf (gethash (type-of listener) *global-listeners*) NIL)
      (funcall fun))))

(defun extract-listener-type (handlers)
  (loop with previous
        for (name) in handlers
        for type = (callback-listener-type name)
        do (cond ((null type) (error "Unknown handler type: ~a" name))
                 ((null previous) (setf previous type))
                 ((not (eq type previous)) (error "Cannot define a listener that listens to more than one type of event.
The GOG Galaxy listener API fucking sucks. SORRY!")))
        finally (return previous)))

(defmacro define-global-listener (name types &body handlers)
  `(progn
     (unless (gethash name *global-listeners*)
       (setf (gethash name *global-listeners*) NIL))

     (defclass ,name (listener)
       ((listener-type :initform ,(extract-listener-type handlers))))

     (defmethod listen-on ((listener ,name))
       ',(etypecase types
           ((eql T) (cffi:foreign-enum-keyword-list 'gog:listener-type))
           (keyword (list types))
           (list types)))

     ,@(loop for (name args . body) in handlers
             collect `(defmethod ,name ((listener ,name) ,@args)
                        ,@body))))

(defclass dynamic-listener (listener)
  ((thunktable :initform (make-hash-table :test 'eq) :accessor thunktable)))

(defmethod initialize-instance ((listener dynamic-listener) &rest args &key &allow-other-keys)
  (call-next-method)
  (loop for (k v) on args by #'cddr
        do (unless (keywordp k)
             (setf (gethash k (thunktable listener)) v))))

(defmacro with-listener ((listener) thunk &body handlers)
  `(block ,listener
     (flet ,handlers
       (let ((,listener (make-instance 'dynamic-listener
                                       :listener-type ,(extract-listener-type handlers)
                                       ,@(loop for (type) in handlers
                                               do (unless (fboundp type)
                                                    (error "Not a callback: ~a" type))
                                               collect `',type collect `#',type))))
         (unwind-protect
              (let ((,listener (handle ,listener)))
                ,thunk)
           (free ,listener))))))

(trivial-indent:define-indentation with-listener (6 6 &rest (&whole 2 6 &body)))

(defmacro with-listener* ((listener &optional (timeout '*listener-timeout*)) thunk &body handlers)
  (let ((interval 0.01))
    `(with-listener (,listener)
           (progn ,thunk
                  (loop for i from 0 below ,timeout by ,interval
                        do (process-data)
                           (sleep ,interval)
                        finally (error "Timeout")))
       ,@handlers)))

(trivial-indent:define-indentation with-listener* (6 6 &rest (&whole 2 6 &body)))

(defmacro define-callback (name field &body args)
  (let ((callback (intern (format NIL "%~a-~a" (symbol-name name) (symbol-name '#:callback))))
        (listener (gensym "LISTENER")))
    `(progn (setf (gethash ',callback *callback-funs*) ',field)
            
            (cffi:defcallback ,callback :void ((userptr :pointer) ,@(loop for (name type) in args collect (list name type)))
              (let ((,listener (pointer->object userptr)))
                (if ,listener
                    (handler-case (,name ,listener ,@(loop for (name type conv) in args
                                                           collect (if conv `(,conv ,name) name)))
                      #+trial-release
                      (error (e)
                        (warn* "Uncaught error in callback ~s to ~a: ~a" ',name listener e)))
                    (warn* "Callback for unregistered listener ~a" userptr))))
            
            (defmethod ,name ((,listener listener) ,@(mapcar #'first args))
              (warn* "Callback ~s to listener ~a is unhandled." ',name ,listener))

            (defmethod ,name ((,listener dynamic-listener) ,@(mapcar #'first args))
              (let ((thunk (gethash ',name (thunktable ,listener))))
                (if thunk
                    (funcall thunk ,@(mapcar #'first args))
                    (warn* "Callback ~s to listener ~a is unhandled." ',name ,listener)))))))

(define-callback auth-success gog::on-auth-success)

(define-callback auth-failure gog::on-auth-failure
  (reason gog:auth-failure-reason))

(define-callback auth-lost gog::on-auth-lost)

(define-callback other-session-started gog::on-other-session-started)

(define-callback operational-state-changed gog::on-operational-state-changed
  (state gog:operational-state))

(define-callback user-data-updated gog::on-user-data-updated)

(define-callback specific-user-data-updated gog::on-specific-user-data-updated
  (user gog:id ensure-user))

(define-callback encrypted-app-ticket-retrieve-success gog::on-encrypted-app-ticket-retrieve-success)

(define-callback encrypted-app-ticket-retrieve-failure gog::on-encrypted-app-ticket-retrieve-failure
  (failure-reason gog:failure-reason))

(define-callback access-token-changed gog::on-access-token-changed)

(define-callback play-fab-create-open-idconnection-success gog::on-play-fab-create-open-idconnection-success
  (connection-already-exists :bool))

(define-callback play-fab-create-open-idconnection-failure gog::on-play-fab-create-open-idconnection-failure
  (failure-reason gog:failure-reason))

(define-callback play-fab-login-with-open-idconnect-success gog::on-play-fab-login-with-open-idconnect-success)

(define-callback play-fab-login-with-open-idconnect-failure gog::on-play-fab-login-with-open-idconnect-failure
  (failure-reason gog:failure-reason))

(define-callback persona-data-changed gog::on-persona-data-changed
  (user gog:id ensure-user)
  (persona-state-change :uint32))

(define-callback user-information-retrieve-success gog::on-user-information-retrieve-success
  (user gog:id ensure-user))

(define-callback user-information-retrieve-failure gog::on-user-information-retrieve-failure
  (user gog:id ensure-user)
  (failure-reason gog:failure-reason))

(define-callback friend-list-retrieve-success gog::on-friend-list-retrieve-success)

(define-callback friend-list-retrieve-failure gog::on-friend-list-retrieve-failure
  (failure-reason gog:failure-reason))

(define-callback friend-invitation-send-success gog::on-friend-invitation-send-success
  (user gog:id ensure-user))

(define-callback friend-invitation-send-failure gog::on-friend-invitation-send-failure
  (user gog:id ensure-user)
  (failure-reason gog:friend-invitation-failure-reason))

(define-callback friend-invitation-list-retrieve-success gog::on-friend-invitation-list-retrieve-success)

(define-callback friend-invitation-list-retrieve-failure gog::on-friend-invitation-list-retrieve-failure
  (failure-reason gog:failure-reason))

(define-callback sent-friend-invitation-list-retrieve-success gog::on-sent-friend-invitation-list-retrieve-success)

(define-callback sent-friend-invitation-list-retrieve-failure gog::on-sent-friend-invitation-list-retrieve-failure
  (failure-reason gog:failure-reason))

(define-callback friend-invitation-received gog::on-friend-invitation-received
  (user gog:id ensure-user)
  (send-time :uint32))

(define-callback friend-invitation-respond-to-success gog::on-friend-invitation-respond-to-success
  (user gog:id ensure-user)
  (accept :bool))

(define-callback friend-invitation-respond-to-failure gog::on-friend-invitation-respond-to-failure
  (user gog:id ensure-user)
  (failure-reason gog:friend-invitation-respond-failure-reason))

(define-callback friend-added gog::on-friend-added
  (user gog:id ensure-user)
  (invitation-direction gog:invitation-direction))

(define-callback friend-delete-success gog::on-friend-delete-success
  (user gog:id ensure-user))

(define-callback friend-delete-failure gog::on-friend-delete-failure
  (user gog:id ensure-user)
  (failure-reason gog:failure-reason))

(define-callback rich-presence-change-success gog::on-rich-presence-change-success)

(define-callback rich-presence-change-failure gog::on-rich-presence-change-failure
  (failure-reason gog:failure-reason))

(define-callback rich-presence-updated gog::on-rich-presence-updated
  (user gog:id ensure-user))

(define-callback rich-presence-retrieve-success gog::on-rich-presence-retrieve-success
  (user gog:id ensure-user))

(define-callback rich-presence-retrieve-failure gog::on-rich-presence-retrieve-failure
  (user gog:id ensure-user)
  (failure-reason gog:failure-reason))

(define-callback game-join-requested gog::on-game-join-requested
  (user gog:id ensure-user)
  (connection-string :string))

(define-callback game-invitation-received gog::on-game-invitation-received
  (user gog:id ensure-user)
  (connection-string :string))

(define-callback invitation-send-success gog::on-invitation-send-success
  (user gog:id ensure-user)
  (connection-string :string))

(define-callback invitation-send-failure gog::on-invitation-send-failure
  (user gog:id ensure-user)
  (connection-string :string)
  (failure-reason gog:send-invitation-failure-reason))

(define-callback user-find-success gog::on-user-find-success
  (user-specifier :string)
  (user gog:id ensure-user))

(define-callback user-find-failure gog::on-user-find-failure
  (user-specifier :string)
  (failure-reason gog:user-find-failure-reason))

(define-callback chat-room-with-user-retrieve-success gog::on-chat-room-with-user-retrieve-success
  (user gog:id ensure-user)
  (chat-room-id gog:chat-room-id))

(define-callback chat-room-with-user-retrieve-failure gog::on-chat-room-with-user-retrieve-failure
  (user gog:id ensure-user)
  (failure-reason gog:chat-user-retrieve-failure-reason))

(define-callback chat-room-message-send-success gog::on-chat-room-message-send-success
  (chat-room-id gog:chat-room-id)
  (sent-message-index :uint32)
  (message-id gog:chat-message-id)
  (send-time :uint32))

(define-callback chat-room-message-send-failure gog::on-chat-room-message-send-failure
  (chat-room-id gog:chat-room-id)
  (sent-message-index :uint32)
  (failure-reason gog:chat-message-send-failure-reason))

(define-callback chat-room-messages-received gog::on-chat-room-messages-received
  (chat-room-id gog:chat-room-id)
  (message-count :uint32)
  (longest-message-lenght :uint32))

(define-callback chat-room-messages-retrieve-success gog::on-chat-room-messages-retrieve-success
  (chat-room-id gog:chat-room-id)
  (message-count :uint32)
  (longest-message-lenght :uint32))

(define-callback chat-room-messages-retrieve-failure gog::on-chat-room-messages-retrieve-failure
  (chat-room-id gog:chat-room-id)
  (failure-reason gog:chat-message-retrieve-failure-reason))

(define-callback lobby-list gog::on-lobby-list
  (lobby-count :uint32)
  (result gog:lobby-list-result))

(define-callback lobby-created gog::on-lobby-created
  (lobby gog:id ensure-lobby)
  (result gog:lobby-create-result))

(define-callback lobby-entered gog::on-lobby-entered
  (lobby gog:id ensure-lobby)
  (result gog:lobby-enter-result))

(define-callback lobby-left gog::on-lobby-left
  (lobby gog:id ensure-lobby)
  (leave-reason gog:lobby-leave-reason))

(define-callback lobby-data-updated gog::on-lobby-data-updated
  (lobby gog:id ensure-lobby)
  (member gog:id ensure-user))

(define-callback lobby-data-update-success gog::on-lobby-data-update-success
  (lobby gog:id ensure-lobby))

(define-callback lobby-data-update-failure gog::on-lobby-data-update-failure
  (lobby gog:id ensure-lobby)
  (failure-reason gog:lobby-data-update-failure-reason))

(define-callback lobby-member-data-update-success gog::on-lobby-member-data-update-success
  (lobby gog:id ensure-lobby)
  (member gog:id ensure-user))

(define-callback lobby-member-data-update-failure gog::on-lobby-member-data-update-failure
  (lobby gog:id ensure-lobby)
  (member gog:id ensure-user)
  (failure-reason gog:lobby-data-update-failure-reason))

(define-callback lobby-data-retrieve-success gog::on-lobby-data-retrieve-success
  (lobby gog:id ensure-lobby))

(define-callback lobby-data-retrieve-failure gog::on-lobby-data-retrieve-failure
  (lobby gog:id ensure-lobby)
  (failure-reason gog:lobby-data-retrieve-failure-reason))

(define-callback lobby-member-state-changed gog::on-lobby-member-state-changed
  (lobby gog:id ensure-lobby)
  (member gog:id ensure-user)
  (member-state-change gog:lobby-member-state-change))

(define-callback lobby-owner-changed gog::on-lobby-owner-changed
  (lobby gog:id ensure-lobby)
  (new-owner-id gog:id))

(define-callback lobby-message-received gog::on-lobby-message-received
  (lobby gog:id ensure-lobby)
  (sender-id gog:id)
  (message-id :uint32)
  (message-length :uint32))

(define-callback p2-ppacket-available gog::on-p2-ppacket-available
  (msg-size :uint32)
  (channel :uint8))

(define-callback nat-type-detection-success gog::on-nat-type-detection-success
  (nat-type gog:nat-type))

(define-callback nat-type-detection-failure gog::on-nat-type-detection-failure)

(define-callback user-stats-and-achievements-retrieve-success gog::on-user-stats-and-achievements-retrieve-success
  (user gog:id ensure-user))

(define-callback user-stats-and-achievements-retrieve-failure gog::on-user-stats-and-achievements-retrieve-failure
  (user gog:id ensure-user)
  (failure-reason gog:failure-reason))

(define-callback user-stats-and-achievements-store-success gog::on-user-stats-and-achievements-store-success)

(define-callback user-stats-and-achievements-store-failure gog::on-user-stats-and-achievements-store-failure
  (failure-reason gog:failure-reason))

(define-callback achievement-unlocked gog::on-achievement-unlocked
  (name :string))

(define-callback leaderboards-retrieve-success gog::on-leaderboards-retrieve-success)

(define-callback leaderboards-retrieve-failure gog::on-leaderboards-retrieve-failure
  (failure-reason gog:failure-reason))

(define-callback leaderboard-entries-retrieve-success gog::on-leaderboard-entries-retrieve-success
  (name :string)
  (entry-count :uint32))

(define-callback leaderboard-entries-retrieve-failure gog::on-leaderboard-entries-retrieve-failure
  (name :string)
  (failure-reason gog:failure-reason))

(define-callback leaderboard-score-update-success gog::on-leaderboard-score-update-success
  (name :string)
  (score :int32)
  (old-rank :uint32)
  (new-rank :uint32))

(define-callback leaderboard-score-update-failure gog::on-leaderboard-score-update-failure
  (name :string)
  (score :int32)
  (failure-reason gog:leaderboard-score-update-failure-reason))

(define-callback leaderboard-retrieve-success gog::on-leaderboard-retrieve-success
  (name :string))

(define-callback leaderboard-retrieve-failure gog::on-leaderboard-retrieve-failure
  (name :string)
  (failure-reason gog:failure-reason))

(define-callback user-time-played-retrieve-success gog::on-user-time-played-retrieve-success
  (user gog:id ensure-user))

(define-callback user-time-played-retrieve-failure gog::on-user-time-played-retrieve-failure
  (user gog:id ensure-user)
  (failure-reason gog:failure-reason))

(define-callback overlay-visibility-changed gog::on-overlay-visibility-changed
  (overlay-visible :bool))

(define-callback overlay-state-changed gog::on-overlay-state-changed
  (overlay-state gog:overlay-state))

(define-callback notification-received gog::on-notification-received
  (notification-id gog:notification-id)
  (type-length :uint32)
  (content-size :uint32))

(define-callback connection-state-change gog::on-connection-state-change
  (connection-state gog:services-connection-state))

(define-callback dlc-check-success gog::on-dlc-check-success
  (product-id gog:product-id)
  (is-owned :bool))

(define-callback dlc-check-failure gog::on-dlc-check-failure
  (product-id gog:product-id)
  (failure-reason gog:dlc-check-failure-reason))

(define-callback file-share-success gog::on-file-share-success
  (file-name :string)
  (shared-file-id gog:shared-file-id))

(define-callback file-share-failure gog::on-file-share-failure
  (file-name :string)
  (failure-reason gog:failure-reason))

(define-callback shared-file-download-success gog::on-shared-file-download-success
  (shared-file-id gog:shared-file-id)
  (file-name :string))

(define-callback shared-file-download-failure gog::on-shared-file-download-failure
  (shared-file-id gog:shared-file-id)
  (failure-reason gog:failure-reason))

(define-callback connection-open-success gog::on-connection-open-success
  (connection-string :string)
  (connection-id gog:connection-id))

(define-callback connection-open-failure gog::on-connection-open-failure
  (connection-string :string)
  (failure-reason gog:connection-open-failure-reason))

(define-callback connection-closed gog::on-connection-closed
  (connection-id gog:connection-id)
  (close-reason gog:close-reason))

(define-callback connection-data-received gog::on-connection-data-received
  (connection-id gog:connection-id)
  (data-size :uint32))

(define-callback telemetry-event-send-success gog::on-telemetry-event-send-success
  (event-type :string)
  (sent-event-index :uint32))

(define-callback telemetry-event-send-failure gog::on-telemetry-event-send-failure
  (event-type :string)
  (sent-event-index :uint32)
  (failure-reason gog:telemetry-event-failure-reason))

(define-callback get-file-list-success gog::on-get-file-list-success
  (file-count :uint32)
  (quota :uint32)
  (quota-used :uint32))

(define-callback get-file-list-failure gog::on-get-file-list-failure
  (failure-reason gog:get-file-list-failure-reason))

(define-callback get-file-success gog::on-get-file-success
  (container :string)
  (name :string)
  (file-size :uint32)
  (savegame-type gog:savegame-type)
  (savegame-id :string))

(define-callback get-file-failure gog::on-get-file-failure
  (container :string)
  (name :string)
  (failure-reason gog:get-file-failure-reason))

(define-callback put-file-success gog::on-put-file-success
  (container :string)
  (name :string))

(define-callback put-file-failure gog::on-put-file-failure
  (container :string)
  (name :string)
  (failure-reason gog:put-file-failure-reason))

(define-callback delete-file-success gog::on-delete-file-success
  (container :string)
  (name :string))

(define-callback delete-file-failure gog::on-delete-file-failure
  (container :string)
  (name :string)
  (failure-reason gog:delete-file-failure-reason))

(defvar *callback-listener-map*
  (let ((map (make-hash-table :test 'eq)))
    (loop for (k k2 v) in 
          '((auth-success gog::on-auth-success :auth)
            (auth-failure gog::on-auth-failure :auth)
            (auth-lost gog::on-auth-lost :auth)
            (other-session-started gog::on-other-session-started :other-session-start)
            (operational-state-changed gog::on-operational-state-changed :operational-state-change)
            (user-data-updated gog::on-user-data-updated :user-data)
            (specific-user-data-updated gog::on-specific-user-data-updated :specific-user-data)
            (encrypted-app-ticket-retrieve-success gog::on-encrypted-app-ticket-retrieve-success :encrypted-app-ticket-retrieve)
            (encrypted-app-ticket-retrieve-failure gog::on-encrypted-app-ticket-retrieve-failure :encrypted-app-ticket-retrieve)
            (access-token-changed gog::on-access-token-changed :access-token-change)
            (play-fab-create-open-idconnection-success gog::on-play-fab-create-open-idconnection-success :playfab-create-openid-connection)
            (play-fab-create-open-idconnection-failure gog::on-play-fab-create-open-idconnection-failure :playfab-create-openid-connection)
            (play-fab-login-with-open-idconnect-success gog::on-play-fab-login-with-open-idconnect-success :playfab-login-with-openid-connect)
            (play-fab-login-with-open-idconnect-failure gog::on-play-fab-login-with-open-idconnect-failure :playfab-login-with-openid-connect)
            (persona-data-changed gog::on-persona-data-changed :persona-data-changed)
            (user-information-retrieve-success gog::on-user-information-retrieve-success :user-information-retrieve-listener)
            (user-information-retrieve-failure gog::on-user-information-retrieve-failure :user-information-retrieve-listener)
            (friend-list-retrieve-success gog::on-friend-list-retrieve-success :friend-list-retrieve)
            (friend-list-retrieve-failure gog::on-friend-list-retrieve-failure :friend-list-retrieve)
            (friend-invitation-send-success gog::on-friend-invitation-send-success :friend-invitation-send-listener)
            (friend-invitation-send-failure gog::on-friend-invitation-send-failure :friend-invitation-send-listener)
            (friend-invitation-list-retrieve-success gog::on-friend-invitation-list-retrieve-success :friend-invitation-list-retrieve-listener)
            (friend-invitation-list-retrieve-failure gog::on-friend-invitation-list-retrieve-failure :friend-invitation-list-retrieve-listener)
            (sent-friend-invitation-list-retrieve-success gog::on-sent-friend-invitation-list-retrieve-success :sent-friend-invitation-list-retrieve-listener)
            (sent-friend-invitation-list-retrieve-failure gog::on-sent-friend-invitation-list-retrieve-failure :sent-friend-invitation-list-retrieve-listener)
            (friend-invitation-received gog::on-friend-invitation-received :friend-invitation-listener)
            (friend-invitation-respond-to-success gog::on-friend-invitation-respond-to-success :friend-invitation-respond-to-listener)
            (friend-invitation-respond-to-failure gog::on-friend-invitation-respond-to-failure :friend-invitation-respond-to-listener)
            (friend-added gog::on-friend-added :friend-add-listener)
            (friend-delete-success gog::on-friend-delete-success :friend-delete-listener)
            (friend-delete-failure gog::on-friend-delete-failure :friend-delete-listener)
            (rich-presence-change-success gog::on-rich-presence-change-success :rich-presence-change-listener)
            (rich-presence-change-failure gog::on-rich-presence-change-failure :rich-presence-change-listener)
            (rich-presence-updated gog::on-rich-presence-updated :rich-presence-listener)
            (rich-presence-retrieve-success gog::on-rich-presence-retrieve-success :rich-presence-retrieve-listener)
            (rich-presence-retrieve-failure gog::on-rich-presence-retrieve-failure :rich-presence-retrieve-listener)
            (game-join-requested gog::on-game-join-requested :game-join-requested-listener)
            (game-invitation-received gog::on-game-invitation-received :game-invitation-received-listener)
            (invitation-send-success gog::on-invitation-send-success :invitation-send)
            (invitation-send-failure gog::on-invitation-send-failure :invitation-send)
            (user-find-success gog::on-user-find-success :user-find-listener)
            (user-find-failure gog::on-user-find-failure :user-find-listener)
            (chat-room-with-user-retrieve-success gog::on-chat-room-with-user-retrieve-success :chat-room-with-user-retrieve-listener)
            (chat-room-with-user-retrieve-failure gog::on-chat-room-with-user-retrieve-failure :chat-room-with-user-retrieve-listener)
            (chat-room-message-send-success gog::on-chat-room-message-send-success :chat-room-message-send-listener)
            (chat-room-message-send-failure gog::on-chat-room-message-send-failure :chat-room-message-send-listener)
            (chat-room-messages-received gog::on-chat-room-messages-received :chat-room-messages-listener)
            (chat-room-messages-retrieve-success gog::on-chat-room-messages-retrieve-success :chat-room-messages-retrieve-listener)
            (chat-room-messages-retrieve-failure gog::on-chat-room-messages-retrieve-failure :chat-room-messages-retrieve-listener)
            (lobby-list gog::on-lobby-list :lobby-list)
            (lobby-created gog::on-lobby-created :lobby-created)
            (lobby-entered gog::on-lobby-entered :lobby-entered)
            (lobby-left gog::on-lobby-left :lobby-left)
            (lobby-data-updated gog::on-lobby-data-updated :lobby-data)
            (lobby-data-update-success gog::on-lobby-data-update-success :lobby-data-update-listener)
            (lobby-data-update-failure gog::on-lobby-data-update-failure :lobby-data-update-listener)
            (lobby-member-data-update-success gog::on-lobby-member-data-update-success :lobby-member-data-update-listener)
            (lobby-member-data-update-failure gog::on-lobby-member-data-update-failure :lobby-member-data-update-listener)
            (lobby-data-retrieve-success gog::on-lobby-data-retrieve-success :lobby-data-retrieve)
            (lobby-data-retrieve-failure gog::on-lobby-data-retrieve-failure :lobby-data-retrieve)
            (lobby-member-state-changed gog::on-lobby-member-state-changed :lobby-member-state)
            (lobby-owner-changed gog::on-lobby-owner-changed :lobby-owner-change)
            (lobby-message-received gog::on-lobby-message-received :lobby-message)
            (p2-ppacket-available gog::on-p2-ppacket-available :networking)
            (nat-type-detection-success gog::on-nat-type-detection-success :nat-type-detection)
            (nat-type-detection-failure gog::on-nat-type-detection-failure :nat-type-detection)
            (user-stats-and-achievements-retrieve-success gog::on-user-stats-and-achievements-retrieve-success :user-stats-and-achievements-retrieve)
            (user-stats-and-achievements-retrieve-failure gog::on-user-stats-and-achievements-retrieve-failure :user-stats-and-achievements-retrieve)
            (user-stats-and-achievements-store-success gog::on-user-stats-and-achievements-store-success :stats-and-achievements-store)
            (user-stats-and-achievements-store-failure gog::on-user-stats-and-achievements-store-failure :stats-and-achievements-store)
            (achievement-unlocked gog::on-achievement-unlocked :achievement-change)
            (leaderboards-retrieve-success gog::on-leaderboards-retrieve-success :leaderboards-retrieve)
            (leaderboards-retrieve-failure gog::on-leaderboards-retrieve-failure :leaderboards-retrieve)
            (leaderboard-entries-retrieve-success gog::on-leaderboard-entries-retrieve-success :leaderboard-entries-retrieve)
            (leaderboard-entries-retrieve-failure gog::on-leaderboard-entries-retrieve-failure :leaderboard-entries-retrieve)
            (leaderboard-score-update-success gog::on-leaderboard-score-update-success :leaderboard-score-update-listener)
            (leaderboard-score-update-failure gog::on-leaderboard-score-update-failure :leaderboard-score-update-listener)
            (leaderboard-retrieve-success gog::on-leaderboard-retrieve-success :leaderboard-retrieve)
            (leaderboard-retrieve-failure gog::on-leaderboard-retrieve-failure :leaderboard-retrieve)
            (user-time-played-retrieve-success gog::on-user-time-played-retrieve-success :user-time-played-retrieve)
            (user-time-played-retrieve-failure gog::on-user-time-played-retrieve-failure :user-time-played-retrieve)
            (overlay-visibility-changed gog::on-overlay-visibility-changed :overlay-visibility-change)
            (overlay-state-changed gog::on-overlay-state-changed :overlay-initialization-state-change)
            (notification-received gog::on-notification-received :notification-listener)
            (connection-state-change gog::on-connection-state-change :gog-services-connection-state-listener)
            (dlc-check-success gog::on-dlc-check-success :is-dlc-owned)
            (dlc-check-failure gog::on-dlc-check-failure :is-dlc-owned)
            (file-share-success gog::on-file-share-success :file-share)
            (file-share-failure gog::on-file-share-failure :file-share)
            (shared-file-download-success gog::on-shared-file-download-success :shared-file-download)
            (shared-file-download-failure gog::on-shared-file-download-failure :shared-file-download)
            (connection-open-success gog::on-connection-open-success :custom-networking-connection-open)
            (connection-open-failure gog::on-connection-open-failure :custom-networking-connection-open)
            (connection-closed gog::on-connection-closed :custom-networking-connection-close)
            (connection-data-received gog::on-connection-data-received :custom-networking-connection-data)
            (telemetry-event-send-success gog::on-telemetry-event-send-success :telemetry-event-send-listener)
            (telemetry-event-send-failure gog::on-telemetry-event-send-failure :telemetry-event-send-listener)
            (get-file-list-success gog::on-get-file-list-success :cloud-storage-get-file-list)
            (get-file-list-failure gog::on-get-file-list-failure :cloud-storage-get-file-list)
            (get-file-success gog::on-get-file-success :cloud-storage-get-file)
            (get-file-failure gog::on-get-file-failure :cloud-storage-get-file)
            (put-file-success gog::on-put-file-success :cloud-storage-put-file)
            (put-file-failure gog::on-put-file-failure :cloud-storage-put-file)
            (delete-file-success gog::on-delete-file-success :cloud-storage-delete-file)
            (delete-file-failure gog::on-delete-file-failure :cloud-storage-delete-file))
          do (setf (gethash k map) v)
             (setf (gethash k2 map) v))
    map))

(defun callback-listener-type (callback)
  (gethash callback *callback-listener-map*))
