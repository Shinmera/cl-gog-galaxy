(defpackage #:org.shirakumo.fraf.gog-galaxy.cffi
  (:use #:cl)
  (:export
   #:galaxy
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
   #:init-options-galaxy-allocator
   #:init-options-galaxy-thread-factory
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
   #:inetworking-send-p2-ppacket
   #:inetworking-peek-p2-ppacket
   #:inetworking-is-p2-ppacket-available
   #:inetworking-read-p2-ppacket
   #:inetworking-pop-p2-ppacket
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
  (:export))
