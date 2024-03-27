(in-package #:org.shirakumo.fraf.gog-galaxy.cffi)

(defvar *here* #.(or *compile-file-pathname* *load-pathname* *default-pathname-defaults*))
(defvar *static* (make-pathname :name NIL :type NIL :defaults (merge-pathnames "static/" *here*)))
(pushnew *static* cffi:*foreign-library-directories*)

(cffi:define-foreign-library galaxy
  (:darwin (:or "libGalaxy64.dylib"))
  (:linux (:or "libGalaxy64.so"))
  (:windows (:or "Galaxy64.dll"))
  (T (:default "Galaxy64")))

(cffi:define-foreign-library galaxy-c
  (:darwin (:or "mac-amd64-galaxy-c.dylib"))
  (:linux (:or "lin-amd64-galaxy-c.so"))
  (:windows (:or "win-amd64-galaxy-c.dll"))
  (T (:default "galaxy-c")))

(cffi:defcenum id-type
  :unassigned
  :lobby
  :user)

(cffi:defcenum error-type
  :unauthorized-access
  :invalid-argument
  :invalid-state
  :runtime-error)

(cffi:defcenum auth-failure-reason
  :undefined
  :galaxy-service-not-available
  :galaxy-service-not-signed-in
  :connection-failure
  :no-license
  :invalid-credentials
  :galaxy-not-initialized
  :external-service-failure)

(cffi:defcenum operational-state
  (:signed-in #x0001)
  (:logged-on #x0002))

(cffi:defcenum avatar-type
  (:none #x0000)
  (:small #x0001)
  (:medium #x0002)
  (:large #x0004))

(cffi:defcenum persona-state
  :offline
  :online)

(cffi:defbitfield persona-state-change
  (:none #x0000)
  (:name #x0001)
  (:avatar #x0002)
  (:avatar-downloaded-image-small #x0004)
  (:avatar-downloaded-image-medium #x0008)
  (:avatar-downloaded-image-large #x0010)
  (:avatar-downloaded-image-any #x00ff))

(cffi:defcenum friend-invitation-failure-reason
  :undefined
  :user-does-not-exist
  :user-already-invited
  :user-already-friend
  :connection-failure)

(cffi:defcenum friend-invitation-respond-failure-reason
  :undefined
  :user-does-not-exist
  :friend-invitation-does-not-exist
  :user-already-friend
  :connection-failure)

(cffi:defcenum invitation-direction
  :incoming
  :outgoing)

(cffi:defcenum send-invitation-failure-reason
  :undefined
  :user-does-not-exist
  :receiver-does-not-allow-inviting
  :sender-does-not-allow-inviting
  :receiver-blocked
  :sender-blocked
  :connection-failure)

(cffi:defcenum user-find-failure-reason
  :undefined
  :user-not-found
  :connection-failure)

(cffi:defcenum chat-message-type
  :unknown
  :chat-message
  :game-invitation)

(cffi:defcenum chat-user-retrieve-failure-reason
  :undefined
  :forbidden
  :connection-failure)

(cffi:defcenum chat-message-send-failure-reason
  :undefined
  :forbidden
  :connection-failure)

(cffi:defcenum chat-message-retrieve-failure-reason
  :undefined
  :forbidden
  :connection-failure)

(cffi:defcenum lobby-type
  :private
  :friends-only
  :public
  :invisible-to-friends)

(cffi:defcenum lobby-topology-type
  :deprecated-fcm-host-migration
  :fcm
  :star
  :connectionless
  :fcm-ownership-transition)

(cffi:defcenum lobby-member-state-change
  (:entered #x0001)
  (:left #x0002)
  (:disconnected #x0004)
  (:kicked #x0008)
  (:banned #x0010))

(cffi:defcenum lobby-comparison-type
  :=
  :/=
  :>
  :>=
  :<
  :<=)

(cffi:defcenum lobby-create-result
  :success
  :error
  :connection-failure)

(cffi:defcenum lobby-enter-result
  :success
  :lobby-does-not-exist
  :lobby-is-full
  :error
  :connection-failure)

(cffi:defcenum lobby-list-result
  :success
  :error
  :connection-failure)

(cffi:defcenum lobby-leave-reason
  :undefined
  :user-left
  :lobby-closed
  :connection-lost)

(cffi:defcenum lobby-data-update-failure-reason
  :undefined
  :lobby-does-not-exist
  :connection-failure)

(cffi:defcenum lobby-data-retrieve-failure-reason
  :undefined
  :lobby-does-not-exist
  :connection-failure)

(cffi:defcenum nat-type
  :none
  :full-cone
  :address-restricted
  :port-restricted
  :symmetric
  :unknown)

(cffi:defcenum p2p-send-type
  :unreliable
  :reliable
  :unreliable-immediate
  :reliable-immediate)

(cffi:defcenum connection-type
  :none
  :direct
  :proxy)

(cffi:defcenum leaderboard-sort-method
  :none
  :ascending
  :descending)

(cffi:defcenum leaderboard-display-type
  :none
  :number
  :time-seconds
  :time-milliseconds)

(cffi:defcenum leaderboard-score-update-failure-reason
  :undefined
  :no-improvement
  :connection-failure)

(cffi:defcenum overlay-state
  :undefined
  :not-supported
  :disabled
  :failed-to-initialize
  :initialized)

(cffi:defcenum services-connection-state
  :undefined
  :connected
  :disconnected
  :auth-lost)

(cffi:defcenum dlc-check-failure-reason
  :undefined
  :galaxy-service-not-signed-in
  :connection-failure
  :external-service-failure)

(cffi:defcenum connection-open-failure-reason
  :undefined
  :connection-failure
  :unauthorized)

(cffi:defcenum close-reason
  :undefined)

(cffi:defcenum telemetry-event-failure-reason
  :undefined
  :client-forbidden
  :invalid-data
  :connection-failure
  :no-sampling-class-in-config
  :sampling-class-field-missing
  :event-sampled-out
  :sampling-result-already-exist
  :sampling-invalid-result-path)

(cffi:defcenum savegame-type
  :undefined
  :auto
  :quick
  :manual
  :point-of-no-return
  :endgame-save
  :checkpoint)

(cffi:defcenum get-file-list-failure-reason
  :undefined
  :unauthorized
  :forbidden
  :not-found
  :unavailable
  :aborted
  :connection-failure)

(cffi:defcenum get-file-failure-reason
  :undefined
  :unauthorized
  :forbidden
  :not-found
  :unavailable
  :aborted
  :connection-failure
  :buffer-too-small
  :write-func-error)

(cffi:defcenum put-file-failure-reason
  :undefined
  :unauthorized
  :forbidden
  :unavailable
  :aborted
  :connection-failure
  :read-func-error
  :quota-exceeded)

(cffi:defcenum delete-file-failure-reason
  :undefined
  :unauthorized
  :forbidden
  :not-found
  :unavailable
  :aborted
  :connection-failure
  :conflict)

(cffi:defcenum read-phase
  :checksum-calculating
  :uploading)

(cffi:defcenum listener-type
  :lobby-list
  :lobby-created
  :lobby-entered
  :lobby-left
  :lobby-data
  :lobby-member-state
  :lobby-owner-change
  :auth
  :lobby-message
  :networking
  :user-data
  :user-stats-and-achievements-retrieve
  :stats-and-achievements-store
  :achievement-change
  :leaderboards-retrieve
  :leaderboard-entries-retrieve
  :leaderboard-score-update-listener
  :persona-data-changed
  :rich-presence-change-listener
  :game-join-requested-listener
  :operational-state-change
  :friend-list-retrieve
  :encrypted-app-ticket-retrieve
  :access-token-change
  :leaderboard-retrieve
  :specific-user-data
  :invitation-send
  :rich-presence-listener
  :game-invitation-received-listener
  :notification-listener
  :lobby-data-retrieve
  :user-time-played-retrieve
  :other-session-start
  :file-share
  :shared-file-download
  :custom-networking-connection-open
  :custom-networking-connection-close
  :custom-networking-connection-data
  :overlay-initialization-state-change
  :overlay-visibility-change
  :chat-room-with-user-retrieve-listener
  :chat-room-message-send-listener
  :chat-room-messages-listener
  :friend-invitation-send-listener
  :friend-invitation-list-retrieve-listener
  :friend-invitation-listener
  :friend-invitation-respond-to-listener
  :friend-add-listener
  :friend-delete-listener
  :chat-room-messages-retrieve-listener
  :user-find-listener
  :nat-type-detection
  :sent-friend-invitation-list-retrieve-listener
  :lobby-data-update-listener
  :lobby-member-data-update-listener
  :user-information-retrieve-listener
  :rich-presence-retrieve-listener
  :services-connection-state-listener
  :telemetry-event-send-listener
  :cloud-storage-get-file-list
  :cloud-storage-get-file
  :cloud-storage-put-file
  :cloud-storage-delete-file
  :is-dlc-owned
  :playfab-create-openid-connection
  :playfab-login-with-openid-connect)

(cffi:defcenum failure-reason
  :undefined
  :connection-failure)

(cffi:defbitfield avatar-criteria
  :none
  :small
  :medium
  :large)

(cffi:defctype id :uint64)
(cffi:defctype interface :pointer)
(cffi:defctype session-id :uint64)
(cffi:defctype chat-room-id :uint64)
(cffi:defctype chat-message-id :uint64)
(cffi:defctype notification-id :uint64)
(cffi:defctype product-id :uint64)
(cffi:defctype shared-file-id :uint64)
(cffi:defctype connection-id :uint64)

(cffi:defcstruct (thread :class thread :conc-name thread-)
  (join :pointer)
  (joinable :pointer)
  (detach :pointer)
  (user :pointer))

(cffi:defcstruct (init-options :class init-options :conc-name init-options-)
  (client-id :string)
  (client-secret :string)
  (config-file-path :string)
  (storage-path :string)
  (allocator :pointer)
  (thread-factory :pointer)
  (host :string)
  (port :uint16))

(cffi:defcstruct (shutdown-options :class shutdown-options :conc-name shutdown-options-)
  (preserve-static-objects :bool))

(cffi:defcstruct (listener :class listener :conc-name listener-)
  (userptr :pointer)
  (on-auth-success :pointer)
  (on-auth-failure :pointer)
  (on-auth-lost :pointer)
  (on-other-session-started :pointer)
  (on-operational-state-changed :pointer)
  (on-user-data-updated :pointer)
  (on-specific-user-data-updated :pointer)
  (on-encrypted-app-ticket-retrieve-success :pointer)
  (on-encrypted-app-ticket-retrieve-failure :pointer)
  (on-access-token-changed :pointer)
  (on-play-fab-create-open-idconnection-success :pointer)
  (on-play-fab-create-open-idconnection-failure :pointer)
  (on-play-fab-login-with-open-idconnect-success :pointer)
  (on-play-fab-login-with-open-idconnect-failure :pointer)
  (on-persona-data-changed :pointer)
  (on-user-information-retrieve-success :pointer)
  (on-user-information-retrieve-failure :pointer)
  (on-friend-list-retrieve-success :pointer)
  (on-friend-list-retrieve-failure :pointer)
  (on-friend-invitation-send-success :pointer)
  (on-friend-invitation-send-failure :pointer)
  (on-friend-invitation-list-retrieve-success :pointer)
  (on-friend-invitation-list-retrieve-failure :pointer)
  (on-sent-friend-invitation-list-retrieve-success :pointer)
  (on-sent-friend-invitation-list-retrieve-failure :pointer)
  (on-friend-invitation-received :pointer)
  (on-friend-invitation-respond-to-success :pointer)
  (on-friend-invitation-respond-to-failure :pointer)
  (on-friend-added :pointer)
  (on-friend-delete-success :pointer)
  (on-friend-delete-failure :pointer)
  (on-rich-presence-change-success :pointer)
  (on-rich-presence-change-failure :pointer)
  (on-rich-presence-updated :pointer)
  (on-rich-presence-retrieve-success :pointer)
  (on-rich-presence-retrieve-failure :pointer)
  (on-game-join-requested :pointer)
  (on-game-invitation-received :pointer)
  (on-invitation-send-success :pointer)
  (on-invitation-send-failure :pointer)
  (on-user-find-success :pointer)
  (on-user-find-failure :pointer)
  (on-chat-room-with-user-retrieve-success :pointer)
  (on-chat-room-with-user-retrieve-failure :pointer)
  (on-chat-room-message-send-success :pointer)
  (on-chat-room-message-send-failure :pointer)
  (on-chat-room-messages-received :pointer)
  (on-chat-room-messages-retrieve-success :pointer)
  (on-chat-room-messages-retrieve-failure :pointer)
  (on-lobby-list :pointer)
  (on-lobby-created :pointer)
  (on-lobby-entered :pointer)
  (on-lobby-left :pointer)
  (on-lobby-data-updated :pointer)
  (on-lobby-data-update-success :pointer)
  (on-lobby-data-update-failure :pointer)
  (on-lobby-member-data-update-success :pointer)
  (on-lobby-member-data-update-failure :pointer)
  (on-lobby-data-retrieve-success :pointer)
  (on-lobby-data-retrieve-failure :pointer)
  (on-lobby-member-state-changed :pointer)
  (on-lobby-owner-changed :pointer)
  (on-lobby-message-received :pointer)
  (on-p2-ppacket-available :pointer)
  (on-nat-type-detection-success :pointer)
  (on-nat-type-detection-failure :pointer)
  (on-user-stats-and-achievements-retrieve-success :pointer)
  (on-user-stats-and-achievements-retrieve-failure :pointer)
  (on-user-stats-and-achievements-store-success :pointer)
  (on-user-stats-and-achievements-store-failure :pointer)
  (on-achievement-unlocked :pointer)
  (on-leaderboards-retrieve-success :pointer)
  (on-leaderboards-retrieve-failure :pointer)
  (on-leaderboard-entries-retrieve-success :pointer)
  (on-leaderboard-entries-retrieve-failure :pointer)
  (on-leaderboard-score-update-success :pointer)
  (on-leaderboard-score-update-failure :pointer)
  (on-leaderboard-retrieve-success :pointer)
  (on-leaderboard-retrieve-failure :pointer)
  (on-user-time-played-retrieve-success :pointer)
  (on-user-time-played-retrieve-failure :pointer)
  (on-overlay-visibility-changed :pointer)
  (on-overlay-state-changed :pointer)
  (on-notification-received :pointer)
  (on-connection-state-change :pointer)
  (on-dlc-check-success :pointer)
  (on-dlc-check-failure :pointer)
  (on-file-share-success :pointer)
  (on-file-share-failure :pointer)
  (on-shared-file-download-success :pointer)
  (on-shared-file-download-failure :pointer)
  (on-connection-open-success :pointer)
  (on-connection-open-failure :pointer)
  (on-connection-closed :pointer)
  (on-connection-data-received :pointer)
  (on-telemetry-event-send-success :pointer)
  (on-telemetry-event-send-failure :pointer)
  (on-get-file-list-success :pointer)
  (on-get-file-list-failure :pointer)
  (on-get-file-success :pointer)
  (on-get-file-failure :pointer)
  (on-put-file-success :pointer)
  (on-put-file-failure :pointer)
  (on-delete-file-success :pointer)
  (on-delete-file-failure :pointer))

(defun id-from-real-id (type value)
  (logior value (ash (cffi:foreign-enum-value 'id-type type) 56)))

(defun id-real-id (id)
  (logand id #xFFFFFFFFFFFFFF))

(defun id-type (id)
  (cffi:foreign-enum-keyword 'id-type (ash id -56)))

(defun id-valid-p (id)
  (/= 0 id))

(cffi:defcfun (make-allocator "gog_MakeAllocator") :pointer
  (malloc :pointer)
  (realloc :pointer)
  (free :pointer))

(cffi:defcfun (free-allocator "gog_FreeAllocator") :void
  (allocator :pointer))

(cffi:defcfun (make-thread-factory "gog_MakeThreadFactory") :pointer
  (creator :pointer))

(cffi:defcfun (free-thread-factory "gog_FreeThreadFactory") :void
  (factory :pointer))

(cffi:defcfun (init "gog_Init") :void
  (options :pointer))

(cffi:defcfun (shutdown "gog_Shutdown") :void)

(cffi:defcfun (shutdown-ex "gog_ShutdownEx") :void
  (options :pointer))

(cffi:defcfun (user "gog_User") interface)

(cffi:defcfun (friends "gog_Friends") interface)

(cffi:defcfun (chat "gog_Chat") interface)

(cffi:defcfun (matchmaking "gog_Matchmaking") interface)

(cffi:defcfun (networking "gog_Networking") interface)

(cffi:defcfun (stats "gog_Stats") interface)

(cffi:defcfun (utils "gog_Utils") interface)

(cffi:defcfun (apps "gog_Apps") interface)

(cffi:defcfun (storage "gog_Storage") interface)

(cffi:defcfun (custom-networking "gog_CustomNetworking") interface)

(cffi:defcfun (logger "gog_Logger") interface)

(cffi:defcfun (telemetry "gog_Telemetry") interface)

(cffi:defcfun (cloud-storage "gog_CloudStorage") interface)

(cffi:defcfun (process-data "gog_ProcessData") :void)

(cffi:defcfun (ierror-get-name "gog_IError_GetName") :string
  (error interface))

(cffi:defcfun (ierror-get-msg "gog_IError_GetMsg") :string
  (error interface))

(cffi:defcfun (ierror-get-type "gog_IError_GetType") error-type
  (error interface))

(cffi:defcfun (get-error "gog_GetError") interface)

(cffi:defcfun (iuser-signed-in "gog_IUser_SignedIn") :bool
  (user interface))

(cffi:defcfun (iuser-get-galaxy-id "gog_IUser_GetGalaxyID") ID
  (user interface))

(cffi:defcfun (iuser-sign-in-credentials "gog_IUser_SignInCredentials") :void
  (user interface)
  (login :string)
  (password :string)
  (listener interface))

(cffi:defcfun (iuser-sign-in-token "gog_IUser_SignInToken") :void
  (user interface)
  (refresh-token :string)
  (listener interface))

(cffi:defcfun (iuser-sign-in-launcher "gog_IUser_SignInLauncher") :void
  (user interface)
  (listener interface))

(cffi:defcfun (iuser-sign-in-steam "gog_IUser_SignInSteam") :void
  (user interface)
  (steam-app-ticket :pointer)
  (steam-app-ticket-size :uint32)
  (persona-name :string)
  (listener interface))

(cffi:defcfun (iuser-sign-in-galaxy "gog_IUser_SignInGalaxy") :void
  (user interface)
  (require-online :bool)
  (timeout :uint32)
  (listener interface))

(cffi:defcfun (iuser-sign-in-ps4 "gog_IUser_SignInPS4") :void
  (user interface)
  (ps4-client-id :string)
  (listener interface))

(cffi:defcfun (iuser-sign-in-xb1 "gog_IUser_SignInXB1") :void
  (user interface)
  (xbox-one-user-id :string)
  (listener interface))

(cffi:defcfun (iuser-sign-in-xbox "gog_IUser_SignInXbox") :void
  (user interface)
  (xbox-id :uint64)
  (listener interface))

(cffi:defcfun (iuser-sign-in-xblive "gog_IUser_SignInXBLive") :void
  (user interface)
  (token :string)
  (signature :string)
  (marketplace-id :string)
  (locale :string)
  (listener interface))

(cffi:defcfun (iuser-sign-in-anonymous "gog_IUser_SignInAnonymous") :void
  (user interface)
  (listener interface))

(cffi:defcfun (iuser-sign-in-anonymous-telemetry "gog_IUser_SignInAnonymousTelemetry") :void
  (user interface)
  (listener interface))

(cffi:defcfun (iuser-sign-in-server-key "gog_IUser_SignInServerKey") :void
  (user interface)
  (server-key :string)
  (listener interface))

(cffi:defcfun (iuser-sign-in-authorization-code "gog_IUser_SignInAuthorizationCode") :void
  (user interface)
  (authorization-code :string)
  (redirect-uri :string)
  (listener interface))

(cffi:defcfun (iuser-sign-out "gog_IUser_SignOut") :void
  (user interface))

(cffi:defcfun (iuser-request-user-data "gog_IUser_RequestUserData") :void
  (user interface)
  (user-id ID)
  (listener interface))

(cffi:defcfun (iuser-is-user-data-available "gog_IUser_IsUserDataAvailable") :bool
  (user interface)
  (user-id ID))

(cffi:defcfun (iuser-get-user-data "gog_IUser_GetUserData") :string
  (user interface)
  (key :string)
  (user-id ID))

(cffi:defcfun (iuser-get-user-data-copy "gog_IUser_GetUserDataCopy") :void
  (user interface)
  (key :string)
  (buffer :string)
  (buffer-length :uint32)
  (user-id ID))

(cffi:defcfun (iuser-set-user-data "gog_IUser_SetUserData") :void
  (user interface)
  (key :string)
  (value :string)
  (listener interface))

(cffi:defcfun (iuser-get-user-data-count "gog_IUser_GetUserDataCount") :uint32
  (user interface)
  (user-id ID))

(cffi:defcfun (iuser-get-user-data-by-index "gog_IUser_GetUserDataByIndex") :bool
  (user interface)
  (index :uint32)
  (key :string)
  (key-length :uint32)
  (value :string)
  (value-length :uint32)
  (user-id ID))

(cffi:defcfun (iuser-delete-user-data "gog_IUser_DeleteUserData") :void
  (user interface)
  (key :string)
  (listener interface))

(cffi:defcfun (iuser-is-logged-on "gog_IUser_IsLoggedOn") :bool
  (user interface))

(cffi:defcfun (iuser-request-encrypted-app-ticket "gog_IUser_RequestEncryptedAppTicket") :void
  (user interface)
  (data :pointer)
  (data-size :uint32)
  (listener interface))

(cffi:defcfun (iuser-get-encrypted-app-ticket "gog_IUser_GetEncryptedAppTicket") :void
  (user interface)
  (encrypted-app-ticket :pointer)
  (max-encrypted-app-ticket-size :uint32)
  (current-encrypted-app-ticket-size :pointer))

(cffi:defcfun (iuser-create-open-idconnection "gog_IUser_CreateOpenIDConnection") :void
  (user interface)
  (secret-key :string)
  (title-id :string)
  (connection-id :string)
  (ignore-nonce :bool)
  (listener interface))

(cffi:defcfun (iuser-login-with-open-idconnect "gog_IUser_LoginWithOpenIDConnect") :void
  (user interface)
  (title-id :string)
  (connection-id :string)
  (id-token :string)
  (create-account :bool)
  (encrypted-request :string)
  (player-secret :string)
  (listener interface))

(cffi:defcfun (iuser-get-session-id "gog_IUser_GetSessionID") session-id
  (user interface))

(cffi:defcfun (iuser-get-access-token "gog_IUser_GetAccessToken") :string
  (user interface))

(cffi:defcfun (iuser-get-access-token-copy "gog_IUser_GetAccessTokenCopy") :void
  (user interface)
  (buffer :string)
  (buffer-length :uint32))

(cffi:defcfun (iuser-get-refresh-token "gog_IUser_GetRefreshToken") :string
  (user interface))

(cffi:defcfun (iuser-get-refresh-token-copy "gog_IUser_GetRefreshTokenCopy") :void
  (user interface)
  (buffer :string)
  (buffer-length :uint32))

(cffi:defcfun (iuser-get-idtoken "gog_IUser_GetIDToken") :string
  (user interface))

(cffi:defcfun (iuser-get-idtoken-copy "gog_IUser_GetIDTokenCopy") :void
  (user interface)
  (buffer :string)
  (buffer-length :uint32))

(cffi:defcfun (iuser-report-invalid-access-token "gog_IUser_ReportInvalidAccessToken") :bool
  (user interface)
  (access-token :string)
  (info :string))

(cffi:defcfun (ifriends-get-default-avatar-criteria "gog_IFriends_GetDefaultAvatarCriteria") avatar-criteria
  (friend interface))

(cffi:defcfun (ifriends-set-default-avatar-criteria "gog_IFriends_SetDefaultAvatarCriteria") :void
  (friend interface)
  (default-avatar-criteria avatar-criteria))

(cffi:defcfun (ifriends-request-user-information "gog_IFriends_RequestUserInformation") :void
  (friend interface)
  (user-id ID)
  (avatar-criteria avatar-criteria)
  (listener interface))

(cffi:defcfun (ifriends-is-user-information-available "gog_IFriends_IsUserInformationAvailable") :bool
  (friend interface)
  (user-id ID))

(cffi:defcfun (ifriends-get-persona-name "gog_IFriends_GetPersonaName") :string
  (friend interface))

(cffi:defcfun (ifriends-get-persona-name-copy "gog_IFriends_GetPersonaNameCopy") :void
  (friend interface)
  (buffer :string)
  (buffer-length :uint32))

(cffi:defcfun (ifriends-get-persona-state "gog_IFriends_GetPersonaState") persona-state
  (friend interface))

(cffi:defcfun (ifriends-get-friend-persona-name "gog_IFriends_GetFriendPersonaName") :string
  (friend interface)
  (user-id ID))

(cffi:defcfun (ifriends-get-friend-persona-name-copy "gog_IFriends_GetFriendPersonaNameCopy") :void
  (friend interface)
  (user-id ID)
  (buffer :string)
  (buffer-length :uint32))

(cffi:defcfun (ifriends-get-friend-persona-state "gog_IFriends_GetFriendPersonaState") persona-state
  (friend interface)
  (user-id ID))

(cffi:defcfun (ifriends-get-friend-avatar-url "gog_IFriends_GetFriendAvatarUrl") :string
  (friend interface)
  (user-id ID)
  (avatar-type avatar-type))

(cffi:defcfun (ifriends-get-friend-avatar-url-copy "gog_IFriends_GetFriendAvatarUrlCopy") :void
  (friend interface)
  (user-id ID)
  (avatar-type avatar-type)
  (buffer :string)
  (buffer-length :uint32))

(cffi:defcfun (ifriends-get-friend-avatar-image-id "gog_IFriends_GetFriendAvatarImageID") :uint32
  (friend interface)
  (user-id ID)
  (avatar-type avatar-type))

(cffi:defcfun (ifriends-get-friend-avatar-image-rgba "gog_IFriends_GetFriendAvatarImageRGBA") :void
  (friend interface)
  (user-id ID)
  (avatar-type avatar-type)
  (buffer :pointer)
  (buffer-length :uint32))

(cffi:defcfun (ifriends-is-friend-avatar-image-rgbaavailable "gog_IFriends_IsFriendAvatarImageRGBAAvailable") :bool
  (friend interface)
  (user-id ID)
  (avatar-type avatar-type))

(cffi:defcfun (ifriends-request-friend-list "gog_IFriends_RequestFriendList") :void
  (friend interface)
  (listener interface))

(cffi:defcfun (ifriends-is-friend "gog_IFriends_IsFriend") :bool
  (friend interface)
  (user-id ID))

(cffi:defcfun (ifriends-get-friend-count "gog_IFriends_GetFriendCount") :uint32
  (friend interface))

(cffi:defcfun (ifriends-get-friend-by-index "gog_IFriends_GetFriendByIndex") ID
  (friend interface)
  (index :uint32))

(cffi:defcfun (ifriends-send-friend-invitation "gog_IFriends_SendFriendInvitation") :void
  (friend interface)
  (user-id ID)
  (listener interface))

(cffi:defcfun (ifriends-request-friend-invitation-list "gog_IFriends_RequestFriendInvitationList") :void
  (friend interface)
  (listener interface))

(cffi:defcfun (ifriends-request-sent-friend-invitation-list "gog_IFriends_RequestSentFriendInvitationList") :void
  (friend interface)
  (listener interface))

(cffi:defcfun (ifriends-get-friend-invitation-count "gog_IFriends_GetFriendInvitationCount") :uint32
  (friend interface))

(cffi:defcfun (ifriends-get-friend-invitation-by-index "gog_IFriends_GetFriendInvitationByIndex") :void
  (friend interface)
  (index :uint32)
  (user-id :pointer)
  (send-time :pointer))

(cffi:defcfun (ifriends-respond-to-friend-invitation "gog_IFriends_RespondToFriendInvitation") :void
  (friend interface)
  (user-id ID)
  (accept :bool)
  (listener interface))

(cffi:defcfun (ifriends-delete-friend "gog_IFriends_DeleteFriend") :void
  (friend interface)
  (user-id ID)
  (listener interface))

(cffi:defcfun (ifriends-set-rich-presence "gog_IFriends_SetRichPresence") :void
  (friend interface)
  (key :string)
  (value :string)
  (listener interface))

(cffi:defcfun (ifriends-delete-rich-presence "gog_IFriends_DeleteRichPresence") :void
  (friend interface)
  (key :string)
  (listener interface))

(cffi:defcfun (ifriends-clear-rich-presence "gog_IFriends_ClearRichPresence") :void
  (friend interface)
  (listener interface))

(cffi:defcfun (ifriends-request-rich-presence "gog_IFriends_RequestRichPresence") :void
  (friend interface)
  (user-id ID)
  (listener interface))

(cffi:defcfun (ifriends-get-rich-presence "gog_IFriends_GetRichPresence") :string
  (friend interface)
  (key :string)
  (user-id ID))

(cffi:defcfun (ifriends-get-rich-presence-copy "gog_IFriends_GetRichPresenceCopy") :void
  (friend interface)
  (key :string)
  (buffer :string)
  (buffer-length :uint32)
  (user-id ID))

(cffi:defcfun (ifriends-get-rich-presence-count "gog_IFriends_GetRichPresenceCount") :uint32
  (friend interface)
  (user-id ID))

(cffi:defcfun (ifriends-get-rich-presence-by-index "gog_IFriends_GetRichPresenceByIndex") :void
  (friend interface)
  (index :uint32)
  (key :string)
  (key-length :uint32)
  (value :string)
  (value-length :uint32)
  (user-id ID))

(cffi:defcfun (ifriends-get-rich-presence-key-by-index "gog_IFriends_GetRichPresenceKeyByIndex") :string
  (friend interface)
  (index :uint32)
  (user-id ID))

(cffi:defcfun (ifriends-get-rich-presence-key-by-index-copy "gog_IFriends_GetRichPresenceKeyByIndexCopy") :void
  (friend interface)
  (index :uint32)
  (buffer :string)
  (buffer-length :uint32)
  (user-id ID))

(cffi:defcfun (ifriends-show-overlay-invite-dialog "gog_IFriends_ShowOverlayInviteDialog") :void
  (friend interface)
  (connection-string :string))

(cffi:defcfun (ifriends-send-invitation "gog_IFriends_SendInvitation") :void
  (friend interface)
  (user-id ID)
  (connection-string :string)
  (listener interface))

(cffi:defcfun (ifriends-find-user "gog_IFriends_FindUser") :void
  (friend interface)
  (user-specifier :string)
  (listener interface))

(cffi:defcfun (ifriends-is-user-in-the-same-game "gog_IFriends_IsUserInTheSameGame") :bool
  (friend interface)
  (user-id ID))

(cffi:defcfun (ichat-request-chat-room-with-user "gog_IChat_RequestChatRoomWithUser") :void
  (chat interface)
  (user-id ID)
  (listener interface))

(cffi:defcfun (ichat-request-chat-room-messages "gog_IChat_RequestChatRoomMessages") :void
  (chat interface)
  (chat-room-id chat-room-id)
  (limit :uint32)
  (reference-message-id chat-message-id)
  (listener interface))

(cffi:defcfun (ichat-send-chat-room-message "gog_IChat_SendChatRoomMessage") :uint32
  (chat interface)
  (chat-room-id chat-room-id)
  (msg :string)
  (listener interface))

(cffi:defcfun (ichat-get-chat-room-message-by-index "gog_IChat_GetChatRoomMessageByIndex") :uint32
  (chat interface)
  (index :uint32)
  (message-id :pointer)
  (message-type :pointer)
  (sender-id :pointer)
  (send-time :pointer)
  (buffer :string)
  (buffer-length :uint32))

(cffi:defcfun (ichat-get-chat-room-member-count "gog_IChat_GetChatRoomMemberCount") :uint32
  (chat interface)
  (chat-room-id chat-room-id))

(cffi:defcfun (ichat-get-chat-room-member-user-idby-index "gog_IChat_GetChatRoomMemberUserIDByIndex") ID
  (chat interface)
  (chat-room-id chat-room-id)
  (index :uint32))

(cffi:defcfun (ichat-get-chat-room-unread-message-count "gog_IChat_GetChatRoomUnreadMessageCount") :uint32
  (chat interface)
  (chat-room-id chat-room-id))

(cffi:defcfun (ichat-mark-chat-room-as-read "gog_IChat_MarkChatRoomAsRead") :void
  (chat interface)
  (chat-room-id chat-room-id))

(cffi:defcfun (imatchmaking-create-lobby "gog_IMatchmaking_CreateLobby") :void
  (matchmaking interface)
  (lobby-type lobby-type)
  (max-members :uint32)
  (joinable :bool)
  (lobby-topology-type lobby-topology-type)
  (created interface)
  (entered interface))

(cffi:defcfun (imatchmaking-request-lobby-list "gog_IMatchmaking_RequestLobbyList") :void
  (matchmaking interface)
  (allow-full-lobbies :bool)
  (listener interface))

(cffi:defcfun (imatchmaking-add-request-lobby-list-result-count-filter "gog_IMatchmaking_AddRequestLobbyListResultCountFilter") :void
  (matchmaking interface)
  (limit :uint32))

(cffi:defcfun (imatchmaking-add-request-lobby-list-string-filter "gog_IMatchmaking_AddRequestLobbyListStringFilter") :void
  (matchmaking interface)
  (key-to-match :string)
  (value-to-match :string)
  (comparison-type lobby-comparison-type))

(cffi:defcfun (imatchmaking-add-request-lobby-list-numerical-filter "gog_IMatchmaking_AddRequestLobbyListNumericalFilter") :void
  (matchmaking interface)
  (key-to-match :string)
  (value-to-match :int32)
  (comparison-type lobby-comparison-type))

(cffi:defcfun (imatchmaking-add-request-lobby-list-near-value-filter "gog_IMatchmaking_AddRequestLobbyListNearValueFilter") :void
  (matchmaking interface)
  (key-to-match :string)
  (value-to-be-close-to :int32))

(cffi:defcfun (imatchmaking-get-lobby-by-index "gog_IMatchmaking_GetLobbyByIndex") ID
  (matchmaking interface)
  (index :uint32))

(cffi:defcfun (imatchmaking-join-lobby "gog_IMatchmaking_JoinLobby") :void
  (matchmaking interface)
  (lobby-id ID)
  (listener interface))

(cffi:defcfun (imatchmaking-leave-lobby "gog_IMatchmaking_LeaveLobby") :void
  (matchmaking interface)
  (lobby-id ID)
  (listener interface))

(cffi:defcfun (imatchmaking-set-max-num-lobby-members "gog_IMatchmaking_SetMaxNumLobbyMembers") :void
  (matchmaking interface)
  (lobby-id ID)
  (max-num-lobby-members :uint32)
  (listener interface))

(cffi:defcfun (imatchmaking-get-max-num-lobby-members "gog_IMatchmaking_GetMaxNumLobbyMembers") :uint32
  (matchmaking interface)
  (lobby-id ID))

(cffi:defcfun (imatchmaking-get-num-lobby-members "gog_IMatchmaking_GetNumLobbyMembers") :uint32
  (matchmaking interface)
  (lobby-id ID))

(cffi:defcfun (imatchmaking-get-lobby-member-by-index "gog_IMatchmaking_GetLobbyMemberByIndex") ID
  (matchmaking interface)
  (lobby-id ID)
  (index :uint32))

(cffi:defcfun (imatchmaking-set-lobby-type "gog_IMatchmaking_SetLobbyType") :void
  (matchmaking interface)
  (lobby-id ID)
  (lobby-type lobby-type)
  (listener interface))

(cffi:defcfun (imatchmaking-get-lobby-type "gog_IMatchmaking_GetLobbyType") lobby-type
  (matchmaking interface)
  (lobby-id ID))

(cffi:defcfun (imatchmaking-set-lobby-joinable "gog_IMatchmaking_SetLobbyJoinable") :void
  (matchmaking interface)
  (lobby-id ID)
  (joinable :bool)
  (listener interface))

(cffi:defcfun (imatchmaking-is-lobby-joinable "gog_IMatchmaking_IsLobbyJoinable") :bool
  (matchmaking interface)
  (lobby-id ID))

(cffi:defcfun (imatchmaking-request-lobby-data "gog_IMatchmaking_RequestLobbyData") :void
  (matchmaking interface)
  (lobby-id ID)
  (listener interface))

(cffi:defcfun (imatchmaking-get-lobby-data "gog_IMatchmaking_GetLobbyData") :string
  (matchmaking interface)
  (lobby-id ID)
  (key :string))

(cffi:defcfun (imatchmaking-get-lobby-data-copy "gog_IMatchmaking_GetLobbyDataCopy") :void
  (matchmaking interface)
  (lobby-id ID)
  (key :string)
  (buffer :string)
  (buffer-length :uint32))

(cffi:defcfun (imatchmaking-set-lobby-data "gog_IMatchmaking_SetLobbyData") :void
  (matchmaking interface)
  (lobby-id ID)
  (key :string)
  (value :string)
  (listener interface))

(cffi:defcfun (imatchmaking-get-lobby-data-count "gog_IMatchmaking_GetLobbyDataCount") :uint32
  (matchmaking interface)
  (lobby-id ID))

(cffi:defcfun (imatchmaking-get-lobby-data-by-index "gog_IMatchmaking_GetLobbyDataByIndex") :bool
  (matchmaking interface)
  (lobby-id ID)
  (index :uint32)
  (key :string)
  (key-length :uint32)
  (value :string)
  (value-length :uint32))

(cffi:defcfun (imatchmaking-delete-lobby-data "gog_IMatchmaking_DeleteLobbyData") :void
  (matchmaking interface)
  (lobby-id ID)
  (key :string)
  (listener interface))

(cffi:defcfun (imatchmaking-get-lobby-member-data "gog_IMatchmaking_GetLobbyMemberData") :string
  (matchmaking interface)
  (lobby-id ID)
  (member-id ID)
  (key :string))

(cffi:defcfun (imatchmaking-get-lobby-member-data-copy "gog_IMatchmaking_GetLobbyMemberDataCopy") :void
  (matchmaking interface)
  (lobby-id ID)
  (member-id ID)
  (key :string)
  (buffer :string)
  (buffer-length :uint32))

(cffi:defcfun (imatchmaking-set-lobby-member-data "gog_IMatchmaking_SetLobbyMemberData") :void
  (matchmaking interface)
  (lobby-id ID)
  (key :string)
  (value :string)
  (listener interface))

(cffi:defcfun (imatchmaking-get-lobby-member-data-count "gog_IMatchmaking_GetLobbyMemberDataCount") :uint32
  (matchmaking interface)
  (lobby-id ID)
  (member-id ID))

(cffi:defcfun (imatchmaking-get-lobby-member-data-by-index "gog_IMatchmaking_GetLobbyMemberDataByIndex") :bool
  (matchmaking interface)
  (lobby-id ID)
  (member-id ID)
  (index :uint32)
  (key :string)
  (key-length :uint32)
  (value :string)
  (value-length :uint32))

(cffi:defcfun (imatchmaking-delete-lobby-member-data "gog_IMatchmaking_DeleteLobbyMemberData") :void
  (matchmaking interface)
  (lobby-id ID)
  (key :string)
  (listener interface))

(cffi:defcfun (imatchmaking-get-lobby-owner "gog_IMatchmaking_GetLobbyOwner") ID
  (matchmaking interface)
  (lobby-id ID))

(cffi:defcfun (imatchmaking-send-lobby-message "gog_IMatchmaking_SendLobbyMessage") :bool
  (matchmaking interface)
  (lobby-id ID)
  (data :pointer)
  (data-size :uint32))

(cffi:defcfun (imatchmaking-get-lobby-message "gog_IMatchmaking_GetLobbyMessage") :uint32
  (matchmaking interface)
  (lobby-id ID)
  (message-id :uint32)
  (sender-id :pointer)
  (msg :pointer)
  (msg-length :uint32))

(cffi:defcfun (inetworking-send-p2p-packet "gog_INetworking_SendP2PPacket") :bool
  (networking interface)
  (galaxy-id ID)
  (data :pointer)
  (data-size :uint32)
  (send-type p2p-send-type)
  (channel :uint8))

(cffi:defcfun (inetworking-peek-p2p-packet "gog_INetworking_PeekP2PPacket") :bool
  (networking interface)
  (dest :pointer)
  (dest-size :uint32)
  (out-msg-size :pointer)
  (out-galaxy-id :pointer)
  (channel :uint8))

(cffi:defcfun (inetworking-is-p2p-packet-available "gog_INetworking_IsP2PPacketAvailable") :bool
  (networking interface)
  (out-msg-size :pointer)
  (channel :uint8))

(cffi:defcfun (inetworking-read-p2p-packet "gog_INetworking_ReadP2PPacket") :bool
  (networking interface)
  (dest :pointer)
  (dest-size :uint32)
  (out-msg-size :pointer)
  (out-galaxy-id :pointer)
  (channel :uint8))

(cffi:defcfun (inetworking-pop-p2p-packet "gog_INetworking_PopP2PPacket") :void
  (networking interface)
  (channel :uint8))

(cffi:defcfun (inetworking-get-ping-with "gog_INetworking_GetPingWith") :int
  (networking interface)
  (galaxy-id ID))

(cffi:defcfun (inetworking-request-nat-type-detection "gog_INetworking_RequestNatTypeDetection") :void
  (networking interface))

(cffi:defcfun (inetworking-get-nat-type "gog_INetworking_GetNatType") nat-type
  (networking interface))

(cffi:defcfun (inetworking-get-connection-type "gog_INetworking_GetConnectionType") connection-type
  (networking interface)
  (user-id ID))

(cffi:defcfun (istats-request-user-stats-and-achievements "gog_IStats_RequestUserStatsAndAchievements") :void
  (stats interface)
  (user-id ID)
  (listener interface))

(cffi:defcfun (istats-get-stat-int "gog_IStats_GetStatInt") :int32
  (stats interface)
  (name :string)
  (user-id ID))

(cffi:defcfun (istats-get-stat-float "gog_IStats_GetStatFloat") :float
  (stats interface)
  (name :string)
  (user-id ID))

(cffi:defcfun (istats-set-stat-int "gog_IStats_SetStatInt") :void
  (stats interface)
  (name :string)
  (value :int32))

(cffi:defcfun (istats-set-stat-float "gog_IStats_SetStatFloat") :void
  (stats interface)
  (name :string)
  (value :float))

(cffi:defcfun (istats-update-avg-rate-stat "gog_IStats_UpdateAvgRateStat") :void
  (stats interface)
  (name :string)
  (count-this-session :float)
  (session-length :double))

(cffi:defcfun (istats-get-achievement "gog_IStats_GetAchievement") :void
  (stats interface)
  (name :string)
  (unlocked :pointer)
  (unlock-time :pointer)
  (user-id ID))

(cffi:defcfun (istats-set-achievement "gog_IStats_SetAchievement") :void
  (stats interface)
  (name :string))

(cffi:defcfun (istats-clear-achievement "gog_IStats_ClearAchievement") :void
  (stats interface)
  (name :string))

(cffi:defcfun (istats-store-stats-and-achievements "gog_IStats_StoreStatsAndAchievements") :void
  (stats interface)
  (listener interface))

(cffi:defcfun (istats-reset-stats-and-achievements "gog_IStats_ResetStatsAndAchievements") :void
  (stats interface)
  (listener interface))

(cffi:defcfun (istats-get-achievement-display-name "gog_IStats_GetAchievementDisplayName") :string
  (stats interface)
  (name :string))

(cffi:defcfun (istats-get-achievement-display-name-copy "gog_IStats_GetAchievementDisplayNameCopy") :void
  (stats interface)
  (name :string)
  (buffer :string)
  (buffer-length :uint32))

(cffi:defcfun (istats-get-achievement-description "gog_IStats_GetAchievementDescription") :string
  (stats interface)
  (name :string))

(cffi:defcfun (istats-get-achievement-description-copy "gog_IStats_GetAchievementDescriptionCopy") :void
  (stats interface)
  (name :string)
  (buffer :string)
  (buffer-length :uint32))

(cffi:defcfun (istats-is-achievement-visible "gog_IStats_IsAchievementVisible") :bool
  (stats interface)
  (name :string))

(cffi:defcfun (istats-is-achievement-visible-while-locked "gog_IStats_IsAchievementVisibleWhileLocked") :bool
  (stats interface)
  (name :string))

(cffi:defcfun (istats-request-leaderboards "gog_IStats_RequestLeaderboards") :void
  (stats interface)
  (listener interface))

(cffi:defcfun (istats-get-leaderboard-display-name "gog_IStats_GetLeaderboardDisplayName") :string
  (stats interface)
  (name :string))

(cffi:defcfun (istats-get-leaderboard-display-name-copy "gog_IStats_GetLeaderboardDisplayNameCopy") :void
  (stats interface)
  (name :string)
  (buffer :string)
  (buffer-length :uint32))

(cffi:defcfun (istats-get-leaderboard-sort-method "gog_IStats_GetLeaderboardSortMethod") leaderboard-sort-method
  (stats interface)
  (name :string))

(cffi:defcfun (istats-get-leaderboard-display-type "gog_IStats_GetLeaderboardDisplayType") leaderboard-display-type
  (stats interface)
  (name :string))

(cffi:defcfun (istats-request-leaderboard-entries-global "gog_IStats_RequestLeaderboardEntriesGlobal") :void
  (stats interface)
  (name :string)
  (range-start :uint32)
  (range-end :uint32)
  (listener interface))

(cffi:defcfun (istats-request-leaderboard-entries-around-user "gog_IStats_RequestLeaderboardEntriesAroundUser") :void
  (stats interface)
  (name :string)
  (count-before :uint32)
  (count-after :uint32)
  (user-id ID)
  (listener interface))

(cffi:defcfun (istats-request-leaderboard-entries-for-users "gog_IStats_RequestLeaderboardEntriesForUsers") :void
  (stats interface)
  (name :string)
  (user-array :pointer)
  (user-array-size :uint32)
  (listener interface))

(cffi:defcfun (istats-get-requested-leaderboard-entry "gog_IStats_GetRequestedLeaderboardEntry") :void
  (stats interface)
  (index :uint32)
  (rank :pointer)
  (score :pointer)
  (user-id :pointer))

(cffi:defcfun (istats-get-requested-leaderboard-entry-with-details "gog_IStats_GetRequestedLeaderboardEntryWithDetails") :void
  (stats interface)
  (index :uint32)
  (rank :pointer)
  (score :pointer)
  (details :pointer)
  (details-size :uint32)
  (out-details-size :pointer)
  (user-id :pointer))

(cffi:defcfun (istats-set-leaderboard-score "gog_IStats_SetLeaderboardScore") :void
  (stats interface)
  (name :string)
  (score :int32)
  (force-update :bool)
  (listener interface))

(cffi:defcfun (istats-set-leaderboard-score-with-details "gog_IStats_SetLeaderboardScoreWithDetails") :void
  (stats interface)
  (name :string)
  (score :int32)
  (details :pointer)
  (details-size :uint32)
  (force-update :bool)
  (listener interface))

(cffi:defcfun (istats-get-leaderboard-entry-count "gog_IStats_GetLeaderboardEntryCount") :uint32
  (stats interface)
  (name :string))

(cffi:defcfun (istats-find-leaderboard "gog_IStats_FindLeaderboard") :void
  (stats interface)
  (name :string)
  (listener interface))

(cffi:defcfun (istats-find-or-create-leaderboard "gog_IStats_FindOrCreateLeaderboard") :void
  (stats interface)
  (name :string)
  (display-name :string)
  (sort-method leaderboard-sort-method)
  (display-type leaderboard-display-type)
  (listener interface))

(cffi:defcfun (istats-request-user-time-played "gog_IStats_RequestUserTimePlayed") :void
  (stats interface)
  (user-id ID)
  (listener interface))

(cffi:defcfun (istats-get-user-time-played "gog_IStats_GetUserTimePlayed") :uint32
  (stats interface)
  (user-id ID))

(cffi:defcfun (iutils-get-image-size "gog_IUtils_GetImageSize") :void
  (utils interface)
  (image-id :uint32)
  (width :pointer)
  (height :pointer))

(cffi:defcfun (iutils-get-image-rgba "gog_IUtils_GetImageRGBA") :void
  (utils interface)
  (image-id :uint32)
  (buffer :pointer)
  (buffer-length :uint32))

(cffi:defcfun (iutils-register-for-notification "gog_IUtils_RegisterForNotification") :void
  (utils interface)
  (type :string))

(cffi:defcfun (iutils-get-notification "gog_IUtils_GetNotification") :uint32
  (utils interface)
  (notification-id notification-id)
  (consumable :pointer)
  (type :string)
  (type-length :uint32)
  (content :pointer)
  (content-size :uint32))

(cffi:defcfun (iutils-show-overlay-with-web-page "gog_IUtils_ShowOverlayWithWebPage") :void
  (utils interface)
  (url :string))

(cffi:defcfun (iutils-is-overlay-visible "gog_IUtils_IsOverlayVisible") :bool
  (utils interface))

(cffi:defcfun (iutils-get-overlay-state "gog_IUtils_GetOverlayState") overlay-state
  (utils interface))

(cffi:defcfun (iutils-disable-overlay-popups "gog_IUtils_DisableOverlayPopups") :void
  (utils interface)
  (popup-group :string))

(cffi:defcfun (iutils-get-gog-services-connection-state "gog_IUtils_GetGogServicesConnectionState") services-connection-state
  (utils interface))

(cffi:defcfun (iapps-is-dlc-installed "gog_IApps_IsDlcInstalled") :bool
  (apps interface)
  (product-id product-id))

(cffi:defcfun (iapps-is-dlc-owned "gog_IApps_IsDlcOwned") :void
  (apps interface)
  (product-id product-id)
  (listener interface))

(cffi:defcfun (iapps-get-current-game-language "gog_IApps_GetCurrentGameLanguage") :string
  (apps interface)
  (product-id product-id))

(cffi:defcfun (iapps-get-current-game-language-copy "gog_IApps_GetCurrentGameLanguageCopy") :void
  (apps interface)
  (buffer :string)
  (buffer-length :uint32)
  (product-id product-id))

(cffi:defcfun (iapps-get-current-game-language-code "gog_IApps_GetCurrentGameLanguageCode") :string
  (apps interface)
  (product-id product-id))

(cffi:defcfun (iapps-get-current-game-language-code-copy "gog_IApps_GetCurrentGameLanguageCodeCopy") :void
  (apps interface)
  (buffer :string)
  (buffer-length :uint32)
  (product-id product-id))

(cffi:defcfun (istorage-file-write "gog_IStorage_FileWrite") :void
  (storage interface)
  (file-name :string)
  (data :pointer)
  (data-size :uint32))

(cffi:defcfun (istorage-file-read "gog_IStorage_FileRead") :uint32
  (storage interface)
  (file-name :string)
  (data :pointer)
  (data-size :uint32))

(cffi:defcfun (istorage-file-delete "gog_IStorage_FileDelete") :void
  (storage interface)
  (file-name :string))

(cffi:defcfun (istorage-file-exists "gog_IStorage_FileExists") :bool
  (storage interface)
  (file-name :string))

(cffi:defcfun (istorage-get-file-size "gog_IStorage_GetFileSize") :uint32
  (storage interface)
  (file-name :string))

(cffi:defcfun (istorage-get-file-timestamp "gog_IStorage_GetFileTimestamp") :uint32
  (storage interface)
  (file-name :string))

(cffi:defcfun (istorage-get-file-count "gog_IStorage_GetFileCount") :uint32
  (storage interface))

(cffi:defcfun (istorage-get-file-name-by-index "gog_IStorage_GetFileNameByIndex") :string
  (storage interface)
  (index :uint32))

(cffi:defcfun (istorage-get-file-name-copy-by-index "gog_IStorage_GetFileNameCopyByIndex") :void
  (storage interface)
  (index :uint32)
  (buffer :string)
  (buffer-length :uint32))

(cffi:defcfun (istorage-file-share "gog_IStorage_FileShare") :void
  (storage interface)
  (file-name :string)
  (listener interface))

(cffi:defcfun (istorage-download-shared-file "gog_IStorage_DownloadSharedFile") :void
  (storage interface)
  (shared-file-id shared-file-id)
  (listener interface))

(cffi:defcfun (istorage-get-shared-file-name "gog_IStorage_GetSharedFileName") :string
  (storage interface)
  (shared-file-id shared-file-id))

(cffi:defcfun (istorage-get-shared-file-name-copy "gog_IStorage_GetSharedFileNameCopy") :void
  (storage interface)
  (shared-file-id shared-file-id)
  (buffer :string)
  (buffer-length :uint32))

(cffi:defcfun (istorage-get-shared-file-size "gog_IStorage_GetSharedFileSize") :uint32
  (storage interface)
  (shared-file-id shared-file-id))

(cffi:defcfun (istorage-get-shared-file-owner "gog_IStorage_GetSharedFileOwner") ID
  (storage interface)
  (shared-file-id shared-file-id))

(cffi:defcfun (istorage-shared-file-read "gog_IStorage_SharedFileRead") :uint32
  (storage interface)
  (shared-file-id shared-file-id)
  (data :pointer)
  (data-size :uint32)
  (offset :uint32))

(cffi:defcfun (istorage-shared-file-close "gog_IStorage_SharedFileClose") :void
  (storage interface)
  (shared-file-id shared-file-id))

(cffi:defcfun (istorage-get-downloaded-shared-file-count "gog_IStorage_GetDownloadedSharedFileCount") :uint32
  (storage interface))

(cffi:defcfun (istorage-get-downloaded-shared-file-by-index "gog_IStorage_GetDownloadedSharedFileByIndex") shared-file-id
  (storage interface)
  (index :uint32))

(cffi:defcfun (icustom-networking-open-connection "gog_ICustomNetworking_OpenConnection") :void
  (customnetworking interface)
  (connection-string :string)
  (listener interface))

(cffi:defcfun (icustom-networking-close-connection "gog_ICustomNetworking_CloseConnection") :void
  (customnetworking interface)
  (connection-id connection-id)
  (listener interface))

(cffi:defcfun (icustom-networking-send-data "gog_ICustomNetworking_SendData") :void
  (customnetworking interface)
  (connection-id connection-id)
  (data :pointer)
  (data-size :uint32))

(cffi:defcfun (icustom-networking-get-available-data-size "gog_ICustomNetworking_GetAvailableDataSize") :uint32
  (customnetworking interface)
  (connection-id connection-id))

(cffi:defcfun (icustom-networking-peek-data "gog_ICustomNetworking_PeekData") :void
  (customnetworking interface)
  (connection-id connection-id)
  (dest :pointer)
  (data-size :uint32))

(cffi:defcfun (icustom-networking-read-data "gog_ICustomNetworking_ReadData") :void
  (customnetworking interface)
  (connection-id connection-id)
  (dest :pointer)
  (data-size :uint32))

(cffi:defcfun (icustom-networking-pop-data "gog_ICustomNetworking_PopData") :void
  (customnetworking interface)
  (connection-id connection-id)
  (data-size :uint32))

(cffi:defcfun (itelemetry-add-string-param "gog_ITelemetry_AddStringParam") :void
  (telemetry interface)
  (name :string)
  (value :string))

(cffi:defcfun (itelemetry-add-int-param "gog_ITelemetry_AddIntParam") :void
  (telemetry interface)
  (name :string)
  (value :int32))

(cffi:defcfun (itelemetry-add-float-param "gog_ITelemetry_AddFloatParam") :void
  (telemetry interface)
  (name :string)
  (value :double))

(cffi:defcfun (itelemetry-add-bool-param "gog_ITelemetry_AddBoolParam") :void
  (telemetry interface)
  (name :string)
  (value :bool))

(cffi:defcfun (itelemetry-add-object-param "gog_ITelemetry_AddObjectParam") :void
  (telemetry interface)
  (name :string))

(cffi:defcfun (itelemetry-add-array-param "gog_ITelemetry_AddArrayParam") :void
  (telemetry interface)
  (name :string))

(cffi:defcfun (itelemetry-close-param "gog_ITelemetry_CloseParam") :void
  (telemetry interface))

(cffi:defcfun (itelemetry-clear-params "gog_ITelemetry_ClearParams") :void
  (telemetry interface))

(cffi:defcfun (itelemetry-set-sampling-class "gog_ITelemetry_SetSamplingClass") :void
  (telemetry interface)
  (name :string))

(cffi:defcfun (itelemetry-send-telemetry-event "gog_ITelemetry_SendTelemetryEvent") :uint32
  (telemetry interface)
  (event-type :string)
  (listener interface))

(cffi:defcfun (itelemetry-send-anonymous-telemetry-event "gog_ITelemetry_SendAnonymousTelemetryEvent") :uint32
  (telemetry interface)
  (event-type :string)
  (listener interface))

(cffi:defcfun (itelemetry-get-visit-id "gog_ITelemetry_GetVisitID") :string
  (telemetry interface))

(cffi:defcfun (itelemetry-get-visit-idcopy "gog_ITelemetry_GetVisitIDCopy") :void
  (telemetry interface)
  (buffer :string)
  (buffer-length :uint32))

(cffi:defcfun (itelemetry-reset-visit-id "gog_ITelemetry_ResetVisitID") :void
  (telemetry interface))

(cffi:defcfun (icloud-storage-get-file-list "gog_ICloudStorage_GetFileList") :void
  (cloudstorage interface)
  (container :string)
  (listener interface))

(cffi:defcfun (icloud-storage-get-file-name-by-index "gog_ICloudStorage_GetFileNameByIndex") :string
  (cloudstorage interface)
  (index :uint32))

(cffi:defcfun (icloud-storage-get-file-size-by-index "gog_ICloudStorage_GetFileSizeByIndex") :uint32
  (cloudstorage interface)
  (index :uint32))

(cffi:defcfun (icloud-storage-get-file-timestamp-by-index "gog_ICloudStorage_GetFileTimestampByIndex") :uint32
  (cloudstorage interface)
  (index :uint32))

(cffi:defcfun (icloud-storage-get-file-hash-by-index "gog_ICloudStorage_GetFileHashByIndex") :string
  (cloudstorage interface)
  (index :uint32))

(cffi:defcfun (icloud-storage-get-file-callback "gog_ICloudStorage_GetFileCallback") :void
  (cloudstorage interface)
  (container :string)
  (name :string)
  (user-param :pointer)
  (write-func :pointer)
  (listener interface))

(cffi:defcfun (icloud-storage-get-file "gog_ICloudStorage_GetFile") :void
  (cloudstorage interface)
  (container :string)
  (name :string)
  (data-buffer :pointer)
  (buffer-length :uint32)
  (listener interface))

(cffi:defcfun (icloud-storage-get-file-metadata "gog_ICloudStorage_GetFileMetadata") :void
  (cloudstorage interface)
  (container :string)
  (name :string)
  (listener interface))

(cffi:defcfun (icloud-storage-put-file-callback "gog_ICloudStorage_PutFileCallback") :void
  (cloudstorage interface)
  (container :string)
  (name :string)
  (user-param :pointer)
  (read-func :pointer)
  (rewind-func :pointer)
  (listener interface)
  (savegame-type savegame-type)
  (time-stamp :uint32)
  (hash :string))

(cffi:defcfun (icloud-storage-put-file "gog_ICloudStorage_PutFile") :void
  (cloudstorage interface)
  (container :string)
  (name :string)
  (buffer :pointer)
  (buffer-length :uint32)
  (listener interface)
  (savegame-type savegame-type)
  (time-stamp :uint32)
  (hash :string))

(cffi:defcfun (icloud-storage-calculate-hash-callback "gog_ICloudStorage_CalculateHashCallback") :void
  (cloudstorage interface)
  (user-param :pointer)
  (read-func :pointer)
  (rewind-func :pointer)
  (hash-buffer :string)
  (hash-buffer-size :uint32))

(cffi:defcfun (icloud-storage-calculate-hash "gog_ICloudStorage_CalculateHash") :void
  (cloudstorage interface)
  (buffer :pointer)
  (buffer-length :uint32)
  (hash-buffer :string)
  (hash-buffer-size :uint32))

(cffi:defcfun (icloud-storage-delete-file "gog_ICloudStorage_DeleteFile") :void
  (cloudstorage interface)
  (container :string)
  (name :string)
  (listener interface)
  (expected-hash :string))

(cffi:defcfun (icloud-storage-open-savegame "gog_ICloudStorage_OpenSavegame") :void
  (cloudstorage interface))

(cffi:defcfun (icloud-storage-close-savegame "gog_ICloudStorage_CloseSavegame") :void
  (cloudstorage interface))

(cffi:defcfun (ilistener-registrar-register "gog_IListenerRegistrar_Register") :void
  (registrar interface)
  (type listener-type)
  (listener interface))

(cffi:defcfun (ilistener-registrar-unregister "gog_IListenerRegistrar_Unregister") :void
  (registrar interface)
  (type listener-type)
  (listener interface))

(cffi:defcfun (listener-registrar "gog_ListenerRegistrar") interface)

(cffi:defcfun (game-server-listener-registrar "gog_GameServerListenerRegistrar") interface)

(cffi:defcfun (make-listener "gog_MakeListener") interface
  (listener :pointer)
  (type listener-type))

(cffi:defcfun (free-listener "gog_FreeListener") :void
  (listener interface)
  (type listener-type))
