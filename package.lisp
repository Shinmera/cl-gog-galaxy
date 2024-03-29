(defpackage #:org.shirakumo.fraf.gog-galaxy.cffi
  (:use #:cl)
  (:export
   #:galaxy
   #:galaxy-c
   #:id-type
   #:error-type
   #:auth-failure-reason
   #:operational-state
   #:avatar-type
   #:persona-state
   #:persona-state-change
   #:friend-invitation-failure-reason
   #:friend-invitation-respond-failure-reason
   #:invitation-direction
   #:send-invitation-failure-reason
   #:user-find-failure-reason
   #:chat-message-type
   #:chat-user-retrieve-failure-reason
   #:chat-message-send-failure-reason
   #:chat-message-retrieve-failure-reason
   #:lobby-type
   #:lobby-topology-type
   #:lobby-member-state-change
   #:lobby-comparison-type
   #:lobby-create-result
   #:lobby-enter-result
   #:lobby-list-result
   #:lobby-leave-reason
   #:lobby-data-update-failure-reason
   #:lobby-data-retrieve-failure-reason
   #:nat-type
   #:p2p-send-type
   #:connection-type
   #:leaderboard-sort-method
   #:leaderboard-display-type
   #:leaderboard-score-update-failure-reason
   #:overlay-state
   #:services-connection-state
   #:dlc-check-failure-reason
   #:connection-open-failure-reason
   #:close-reason
   #:telemetry-event-failure-reason
   #:savegame-type
   #:get-file-list-failure-reason
   #:get-file-failure-reason
   #:put-file-failure-reason
   #:delete-file-failure-reason
   #:read-phase
   #:listener-type
   #:failure-reason
   #:id
   #:interface
   #:session-id
   #:avatar-criteria
   #:chat-room-id
   #:chat-message-id
   #:notification-id
   #:product-id
   #:shared-file-id
   #:connection-id
   #:thread
   #:init-options
   #:shutdown-options
   #:listener
   #:thread-join
   #:thread-joinable
   #:thread-detach
   #:thread-user
   #:init-options-client-id
   #:init-options-client-secret
   #:init-options-config-file-path
   #:init-options-storage-path
   #:init-options-allocator
   #:init-options-thread-factory
   #:init-options-host
   #:init-options-port
   #:shutdown-options-preserve-static-objects
   #:listener-userptr
   #:listener-on-auth-success
   #:listener-on-auth-failure
   #:listener-on-auth-lost
   #:listener-on-other-session-started
   #:listener-on-operational-state-changed
   #:listener-on-user-data-updated
   #:listener-on-specific-user-data-updated
   #:listener-on-encrypted-app-ticket-retrieve-success
   #:listener-on-encrypted-app-ticket-retrieve-failure
   #:listener-on-access-token-changed
   #:listener-on-play-fab-create-open-idconnection-success
   #:listener-on-play-fab-create-open-idconnection-failure
   #:listener-on-play-fab-login-with-open-idconnect-success
   #:listener-on-play-fab-login-with-open-idconnect-failure
   #:listener-on-persona-data-changed
   #:listener-on-user-information-retrieve-success
   #:listener-on-user-information-retrieve-failure
   #:listener-on-friend-list-retrieve-success
   #:listener-on-friend-list-retrieve-failure
   #:listener-on-friend-invitation-send-success
   #:listener-on-friend-invitation-send-failure
   #:listener-on-friend-invitation-list-retrieve-success
   #:listener-on-friend-invitation-list-retrieve-failure
   #:listener-on-sent-friend-invitation-list-retrieve-success
   #:listener-on-sent-friend-invitation-list-retrieve-failure
   #:listener-on-friend-invitation-received
   #:listener-on-friend-invitation-respond-to-success
   #:listener-on-friend-invitation-respond-to-failure
   #:listener-on-friend-added
   #:listener-on-friend-delete-success
   #:listener-on-friend-delete-failure
   #:listener-on-rich-presence-change-success
   #:listener-on-rich-presence-change-failure
   #:listener-on-rich-presence-updated
   #:listener-on-rich-presence-retrieve-success
   #:listener-on-rich-presence-retrieve-failure
   #:listener-on-game-join-requested
   #:listener-on-game-invitation-received
   #:listener-on-invitation-send-success
   #:listener-on-invitation-send-failure
   #:listener-on-user-find-success
   #:listener-on-user-find-failure
   #:listener-on-chat-room-with-user-retrieve-success
   #:listener-on-chat-room-with-user-retrieve-failure
   #:listener-on-chat-room-message-send-success
   #:listener-on-chat-room-message-send-failure
   #:listener-on-chat-room-messages-received
   #:listener-on-chat-room-messages-retrieve-success
   #:listener-on-chat-room-messages-retrieve-failure
   #:listener-on-lobby-list
   #:listener-on-lobby-created
   #:listener-on-lobby-entered
   #:listener-on-lobby-left
   #:listener-on-lobby-data-updated
   #:listener-on-lobby-data-update-success
   #:listener-on-lobby-data-update-failure
   #:listener-on-lobby-member-data-update-success
   #:listener-on-lobby-member-data-update-failure
   #:listener-on-lobby-data-retrieve-success
   #:listener-on-lobby-data-retrieve-failure
   #:listener-on-lobby-member-state-changed
   #:listener-on-lobby-owner-changed
   #:listener-on-lobby-message-received
   #:listener-on-p2-ppacket-available
   #:listener-on-nat-type-detection-success
   #:listener-on-nat-type-detection-failure
   #:listener-on-user-stats-and-achievements-retrieve-success
   #:listener-on-user-stats-and-achievements-retrieve-failure
   #:listener-on-user-stats-and-achievements-store-success
   #:listener-on-user-stats-and-achievements-store-failure
   #:listener-on-achievement-unlocked
   #:listener-on-leaderboards-retrieve-success
   #:listener-on-leaderboards-retrieve-failure
   #:listener-on-leaderboard-entries-retrieve-success
   #:listener-on-leaderboard-entries-retrieve-failure
   #:listener-on-leaderboard-score-update-success
   #:listener-on-leaderboard-score-update-failure
   #:listener-on-leaderboard-retrieve-success
   #:listener-on-leaderboard-retrieve-failure
   #:listener-on-user-time-played-retrieve-success
   #:listener-on-user-time-played-retrieve-failure
   #:listener-on-overlay-visibility-changed
   #:listener-on-overlay-state-changed
   #:listener-on-notification-received
   #:listener-on-connection-state-change
   #:listener-on-dlc-check-success
   #:listener-on-dlc-check-failure
   #:listener-on-file-share-success
   #:listener-on-file-share-failure
   #:listener-on-shared-file-download-success
   #:listener-on-shared-file-download-failure
   #:listener-on-connection-open-success
   #:listener-on-connection-open-failure
   #:listener-on-connection-closed
   #:listener-on-connection-data-received
   #:listener-on-telemetry-event-send-success
   #:listener-on-telemetry-event-send-failure
   #:listener-on-get-file-list-success
   #:listener-on-get-file-list-failure
   #:listener-on-get-file-success
   #:listener-on-get-file-failure
   #:listener-on-put-file-success
   #:listener-on-put-file-failure
   #:listener-on-delete-file-success
   #:listener-on-delete-file-failure
   #:id-from-real-id
   #:id-real-id
   #:id-type
   #:id-valid-p
   #:make-allocator
   #:free-allocator
   #:make-thread-factory
   #:free-thread-factor
   #:init
   #:shutdown
   #:shutdown-ex
   #:user
   #:friends
   #:chat
   #:matchmaking
   #:networking
   #:stats
   #:utils
   #:apps
   #:storage
   #:custom-networking
   #:logger
   #:telemetry
   #:cloud-storage
   #:process-data
   #:ierror-get-name
   #:ierror-get-msg
   #:ierror-get-type
   #:get-error
   #:iuser-signed-in
   #:iuser-get-galaxy-id
   #:iuser-sign-in-credentials
   #:iuser-sign-in-token
   #:iuser-sign-in-launcher
   #:iuser-sign-in-steam
   #:iuser-sign-in-galaxy
   #:iuser-sign-in-ps4
   #:iuser-sign-in-xb1
   #:iuser-sign-in-xbox
   #:iuser-sign-in-xblive
   #:iuser-sign-in-anonymous
   #:iuser-sign-in-anonymous-telemetry
   #:iuser-sign-in-server-key
   #:iuser-sign-in-authorization-code
   #:iuser-sign-out
   #:iuser-request-user-data
   #:iuser-is-user-data-available
   #:iuser-get-user-data
   #:iuser-get-user-data-copy
   #:iuser-set-user-data
   #:iuser-get-user-data-count
   #:iuser-get-user-data-by-index
   #:iuser-delete-user-data
   #:iuser-is-logged-on
   #:iuser-request-encrypted-app-ticket
   #:iuser-get-encrypted-app-ticket
   #:iuser-create-open-idconnection
   #:iuser-login-with-open-idconnect
   #:iuser-get-session-id
   #:iuser-get-access-token
   #:iuser-get-access-token-copy
   #:iuser-get-refresh-token
   #:iuser-get-refresh-token-copy
   #:iuser-get-idtoken
   #:iuser-get-idtoken-copy
   #:iuser-report-invalid-access-token
   #:ifriends-get-default-avatar-criteria
   #:ifriends-set-default-avatar-criteria
   #:ifriends-request-user-information
   #:ifriends-is-user-information-available
   #:ifriends-get-persona-name
   #:ifriends-get-persona-name-copy
   #:ifriends-get-persona-state
   #:ifriends-get-friend-persona-name
   #:ifriends-get-friend-persona-name-copy
   #:ifriends-get-friend-persona-state
   #:ifriends-get-friend-avatar-url
   #:ifriends-get-friend-avatar-url-copy
   #:ifriends-get-friend-avatar-image-id
   #:ifriends-get-friend-avatar-image-rgba
   #:ifriends-is-friend-avatar-image-rgbaavailable
   #:ifriends-request-friend-list
   #:ifriends-is-friend
   #:ifriends-get-friend-count
   #:ifriends-get-friend-by-index
   #:ifriends-send-friend-invitation
   #:ifriends-request-friend-invitation-list
   #:ifriends-request-sent-friend-invitation-list
   #:ifriends-get-friend-invitation-count
   #:ifriends-get-friend-invitation-by-index
   #:ifriends-respond-to-friend-invitation
   #:ifriends-delete-friend
   #:ifriends-set-rich-presence
   #:ifriends-delete-rich-presence
   #:ifriends-clear-rich-presence
   #:ifriends-request-rich-presence
   #:ifriends-get-rich-presence
   #:ifriends-get-rich-presence-copy
   #:ifriends-get-rich-presence-count
   #:ifriends-get-rich-presence-by-index
   #:ifriends-get-rich-presence-key-by-index
   #:ifriends-get-rich-presence-key-by-index-copy
   #:ifriends-show-overlay-invite-dialog
   #:ifriends-send-invitation
   #:ifriends-find-user
   #:ifriends-is-user-in-the-same-game
   #:ichat-request-chat-room-with-user
   #:ichat-request-chat-room-messages
   #:ichat-send-chat-room-message
   #:ichat-get-chat-room-message-by-index
   #:ichat-get-chat-room-member-count
   #:ichat-get-chat-room-member-user-idby-index
   #:ichat-get-chat-room-unread-message-count
   #:ichat-mark-chat-room-as-read
   #:imatchmaking-create-lobby
   #:imatchmaking-request-lobby-list
   #:imatchmaking-add-request-lobby-list-result-count-filter
   #:imatchmaking-add-request-lobby-list-string-filter
   #:imatchmaking-add-request-lobby-list-numerical-filter
   #:imatchmaking-add-request-lobby-list-near-value-filter
   #:imatchmaking-get-lobby-by-index
   #:imatchmaking-join-lobby
   #:imatchmaking-leave-lobby
   #:imatchmaking-set-max-num-lobby-members
   #:imatchmaking-get-max-num-lobby-members
   #:imatchmaking-get-num-lobby-members
   #:imatchmaking-get-lobby-member-by-index
   #:imatchmaking-set-lobby-type
   #:imatchmaking-get-lobby-type
   #:imatchmaking-set-lobby-joinable
   #:imatchmaking-is-lobby-joinable
   #:imatchmaking-request-lobby-data
   #:imatchmaking-get-lobby-data
   #:imatchmaking-get-lobby-data-copy
   #:imatchmaking-set-lobby-data
   #:imatchmaking-get-lobby-data-count
   #:imatchmaking-get-lobby-data-by-index
   #:imatchmaking-delete-lobby-data
   #:imatchmaking-get-lobby-member-data
   #:imatchmaking-get-lobby-member-data-copy
   #:imatchmaking-set-lobby-member-data
   #:imatchmaking-get-lobby-member-data-count
   #:imatchmaking-get-lobby-member-data-by-index
   #:imatchmaking-delete-lobby-member-data
   #:imatchmaking-get-lobby-owner
   #:imatchmaking-send-lobby-message
   #:imatchmaking-get-lobby-message
   #:inetworking-send-p2p-packet
   #:inetworking-peek-p2p-packet
   #:inetworking-is-p2p-packet-available
   #:inetworking-read-p2p-packet
   #:inetworking-pop-p2p-packet
   #:inetworking-get-ping-with
   #:inetworking-request-nat-type-detection
   #:inetworking-get-nat-type
   #:inetworking-get-connection-type
   #:istats-request-user-stats-and-achievements
   #:istats-get-stat-int
   #:istats-get-stat-float
   #:istats-set-stat-int
   #:istats-set-stat-float
   #:istats-update-avg-rate-stat
   #:istats-get-achievement
   #:istats-set-achievement
   #:istats-clear-achievement
   #:istats-store-stats-and-achievements
   #:istats-reset-stats-and-achievements
   #:istats-get-achievement-display-name
   #:istats-get-achievement-display-name-copy
   #:istats-get-achievement-description
   #:istats-get-achievement-description-copy
   #:istats-is-achievement-visible
   #:istats-is-achievement-visible-while-locked
   #:istats-request-leaderboards
   #:istats-get-leaderboard-display-name
   #:istats-get-leaderboard-display-name-copy
   #:istats-get-leaderboard-sort-method
   #:istats-get-leaderboard-display-type
   #:istats-request-leaderboard-entries-global
   #:istats-request-leaderboard-entries-around-user
   #:istats-request-leaderboard-entries-for-users
   #:istats-get-requested-leaderboard-entry
   #:istats-get-requested-leaderboard-entry-with-details
   #:istats-set-leaderboard-score
   #:istats-set-leaderboard-score-with-details
   #:istats-get-leaderboard-entry-count
   #:istats-find-leaderboard
   #:istats-find-or-create-leaderboard
   #:istats-request-user-time-played
   #:istats-get-user-time-played
   #:iutils-get-image-size
   #:iutils-get-image-rgba
   #:iutils-register-for-notification
   #:iutils-get-notification
   #:iutils-show-overlay-with-web-page
   #:iutils-is-overlay-visible
   #:iutils-get-overlay-state
   #:iutils-disable-overlay-popups
   #:iutils-get-gog-services-connection-state
   #:iapps-is-dlc-installed
   #:iapps-is-dlc-owned
   #:iapps-get-current-game-language
   #:iapps-get-current-game-language-copy
   #:iapps-get-current-game-language-code
   #:iapps-get-current-game-language-code-copy
   #:istorage-file-write
   #:istorage-file-read
   #:istorage-file-delete
   #:istorage-file-exists
   #:istorage-get-file-size
   #:istorage-get-file-timestamp
   #:istorage-get-file-count
   #:istorage-get-file-name-by-index
   #:istorage-get-file-name-copy-by-index
   #:istorage-file-share
   #:istorage-download-shared-file
   #:istorage-get-shared-file-name
   #:istorage-get-shared-file-name-copy
   #:istorage-get-shared-file-size
   #:istorage-get-shared-file-owner
   #:istorage-shared-file-read
   #:istorage-shared-file-close
   #:istorage-get-downloaded-shared-file-count
   #:istorage-get-downloaded-shared-file-by-index
   #:icustom-networking-open-connection
   #:icustom-networking-close-connection
   #:icustom-networking-send-data
   #:icustom-networking-get-available-data-size
   #:icustom-networking-peek-data
   #:icustom-networking-read-data
   #:icustom-networking-pop-data
   #:itelemetry-add-string-param
   #:itelemetry-add-int-param
   #:itelemetry-add-float-param
   #:itelemetry-add-bool-param
   #:itelemetry-add-object-param
   #:itelemetry-add-array-param
   #:itelemetry-close-param
   #:itelemetry-clear-params
   #:itelemetry-set-sampling-class
   #:itelemetry-send-telemetry-event
   #:itelemetry-send-anonymous-telemetry-event
   #:itelemetry-get-visit-id
   #:itelemetry-get-visit-idcopy
   #:itelemetry-reset-visit-id
   #:icloud-storage-get-file-list
   #:icloud-storage-get-file-name-by-index
   #:icloud-storage-get-file-size-by-index
   #:icloud-storage-get-file-timestamp-by-index
   #:icloud-storage-get-file-hash-by-index
   #:icloud-storage-get-file-callback
   #:icloud-storage-get-file
   #:icloud-storage-get-file-metadata
   #:icloud-storage-put-file-callback
   #:icloud-storage-put-file
   #:icloud-storage-calculate-hash-callback
   #:icloud-storage-calculate-hash
   #:icloud-storage-delete-file
   #:icloud-storage-open-savegame
   #:icloud-storage-close-savegame
   #:ilistener-registrar-register
   #:ilistener-registrar-unregister
   #:listener-registrar
   #:game-server-listener-registrar
   #:make-listener
   #:free-listener))

(defpackage #:org.shirakumo.fraf.gog-galaxy
  (:use #:cl)
  (:local-nicknames
   (#:gog #:org.shirakumo.fraf.gog-galaxy.cffi))
  ;; conditions.lisp
  (:export
   #:gog-error
   #:name
   #:message
   #:kind
   #:check-error)
  ;; c-object.lisp
  (:export
   #:c-object
   #:handle
   #:free
   #:interface)
  ;; id.lisp
  (:export
   #:id-registered-object
   #:id
   #:user
   #:ensure-user
   #:self-p
   #:lobby
   #:ensure-lobby)
  ;; listener.lisp
  (:export
   #:*listener-timeout*
   #:listener
   #:register-for
   #:unregister-from
   #:global-listener
   #:define-global-listener
   #:dynamic-listener
   #:with-listener
   #:with-listener*
   #:auth-success
   #:auth-failure
   #:auth-lost
   #:other-session-started
   #:operational-state-changed
   #:user-data-updated
   #:specific-user-data-updated
   #:encrypted-app-ticket-retrieve-success
   #:encrypted-app-ticket-retrieve-failure
   #:access-token-changed
   #:play-fab-create-open-idconnection-success
   #:play-fab-create-open-idconnection-failure
   #:play-fab-login-with-open-idconnect-success
   #:play-fab-login-with-open-idconnect-failure
   #:persona-data-changed
   #:user-information-retrieve-success
   #:user-information-retrieve-failure
   #:friend-list-retrieve-success
   #:friend-list-retrieve-failure
   #:friend-invitation-send-success
   #:friend-invitation-send-failure
   #:friend-invitation-list-retrieve-success
   #:friend-invitation-list-retrieve-failure
   #:sent-friend-invitation-list-retrieve-success
   #:sent-friend-invitation-list-retrieve-failure
   #:friend-invitation-received
   #:friend-invitation-respond-to-success
   #:friend-invitation-respond-to-failure
   #:friend-added
   #:friend-delete-success
   #:friend-delete-failure
   #:rich-presence-change-success
   #:rich-presence-change-failure
   #:rich-presence-updated
   #:rich-presence-retrieve-success
   #:rich-presence-retrieve-failure
   #:game-join-requested
   #:game-invitation-received
   #:invitation-send-success
   #:invitation-send-failure
   #:user-find-success
   #:user-find-failure
   #:chat-room-with-user-retrieve-success
   #:chat-room-with-user-retrieve-failure
   #:chat-room-message-send-success
   #:chat-room-message-send-failure
   #:chat-room-messages-received
   #:chat-room-messages-retrieve-success
   #:chat-room-messages-retrieve-failure
   #:lobby-list
   #:lobby-created
   #:lobby-entered
   #:lobby-left
   #:lobby-data-updated
   #:lobby-data-update-success
   #:lobby-data-update-failure
   #:lobby-member-data-update-success
   #:lobby-member-data-update-failure
   #:lobby-data-retrieve-success
   #:lobby-data-retrieve-failure
   #:lobby-member-state-changed
   #:lobby-owner-changed
   #:lobby-message-received
   #:p2-ppacket-available
   #:nat-type-detection-success
   #:nat-type-detection-failure
   #:user-stats-and-achievements-retrieve-success
   #:user-stats-and-achievements-retrieve-failure
   #:user-stats-and-achievements-store-success
   #:user-stats-and-achievements-store-failure
   #:achievement-unlocked
   #:leaderboards-retrieve-success
   #:leaderboards-retrieve-failure
   #:leaderboard-entries-retrieve-success
   #:leaderboard-entries-retrieve-failure
   #:leaderboard-score-update-success
   #:leaderboard-score-update-failure
   #:leaderboard-retrieve-success
   #:leaderboard-retrieve-failure
   #:user-time-played-retrieve-success
   #:user-time-played-retrieve-failure
   #:overlay-visibility-changed
   #:overlay-state-changed
   #:notification-received
   #:connection-state-change
   #:dlc-check-success
   #:dlc-check-failure
   #:file-share-success
   #:file-share-failure
   #:shared-file-download-success
   #:shared-file-download-failure
   #:connection-open-success
   #:connection-open-failure
   #:connection-closed
   #:connection-data-received
   #:telemetry-event-send-success
   #:telemetry-event-send-failure
   #:get-file-list-success
   #:get-file-list-failure
   #:get-file-success
   #:get-file-failure
   #:put-file-success
   #:put-file-failure
   #:delete-file-success
   #:delete-file-failure)
  ;; wrapper.lisp
  (:export
   #:init
   #:shutdown
   #:initialized-p
   #:process-data)
  ;; apps.lisp
  (:export
   #:apps
   #:dlc-installed-p
   #:dlc-owned-p
   #:current-game-language
   #:current-game-language-code)
  ;; chat.lisp
  (:export
   #:chat-message
   #:chatroom
   #:id
   #:kind
   #:sender
   #:send-time
   #:text
   #:chatroom
   #:id
   #:messages
   #:send-message
   #:get-message
   #:members
   #:unread-messages
   #:mark-read
   #:chat
   #:get-chatroom)
  ;; cloud-storage.lisp
  (:export
   #:cloud-storage
   #:get-file
   #:list-files
   #:put-file
   #:remove-file
   #:open-savegame
   #:close-savegame)
  ;; friends.lisp
  (:export
   #:friend-p
   #:in-same-game-p
   #:persona-name
   #:persona-state
   #:avatar-url
   #:avatar
   #:befriend
   #:unfriend
   #:rich-presence
   #:invite
   #:friend-request
   #:id
   #:send-time
   #:accept
   #:reject
   #:friends
   #:default-avatar-sizes
   #:list-friends
   #:list-friend-requests
   #:find-user
   #:show-invite-overlay)
  ;; matchmaking.lisp
  (:export
   #:joinable-p
   #:kind
   #:join
   #:leave
   #:max-members
   #:member-count
   #:list-members
   #:owner
   #:send-message
   #:get-message
   #:data
   #:data-field
   #:member-data
   #:member-data-field
   #:matchmaking
   #:create-lobby
   #:list-lobbies)
  ;; networking.lisp
  (:export
   #:networking
   #:nat-type
   #:connection-type
   #:ping-with
   #:send-packet
   #:read-packet
   #:peek-packet
   #:pop-packet
   #:packet-available-p
   #:connection
   #:id
   #:send-data
   #:read-data
   #:peek-data
   #:pop-data
   #:available-data
   #:custom-networking
   #:open-connection)
  ;; stats.lisp
  (:export
   #:achievement
   #:name
   #:achieved-p
   #:visible-p
   #:display-name
   #:description
   #:leaderboard-entry
   #:leaderboard
   #:rank
   #:score
   #:details
   #:user
   #:leaderboard
   #:name
   #:display-name
   #:sort-method
   #:display-type
   #:update
   #:list-entries
   #:stats
   #:achievement
   #:stat
   #:store
   #:reset
   #:find-leaderboard)
  ;; storage.lisp
  (:export
   #:shared-file
   #:id
   #:file-name
   #:file-size
   #:owner
   #:get-file
   #:storage
   #:put-file
   #:get-file
   #:list-files
   #:file-exists-p
   #:remove-file
   #:share-file
   #:list-shared-files)
  ;; telemetry.lisp
  (:export
   #:telemetry
   #:visit-id
   #:reset-visit-id
   #:send-event)
  ;; user.lisp
  (:export
   #:users
   #:signed-in-p
   #:sign-in
   #:sign-out
   #:logged-on-p
   #:session-id
   #:access-token
   #:refresh-token
   #:id-token
   #:user-data)
  ;; utils.lisp
  (:export
   #:utils
   #:show-webpage
   #:overlay-visible-p
   #:overlay-state
   #:connection-state))
