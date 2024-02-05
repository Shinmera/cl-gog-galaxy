(in-package #:org.shirakumo.fraf.gog-galaxy)

(defclass chat-message ()
  ((chatroom :initarg :chatroom :reader chatroom)
   (id :initarg :id :reader id)
   (type :initarg :type :reader kind)
   (sender :initarg :sender :reader sender)
   (send-time :initarg :send-time :reader send-time)
   (text :initarg :text :reader text)))

(defclass chatroom ()
  ((id :initarg :id :reader id)))

(defmethod messages ((chatroom chatroom) &key (limit 100) reference)
  (with-listener* (listener)
        (gog ichat-request-chat-room-messages (handle 'chat) (id chatroom) limit reference listener)
    (chat-room-messages-retrieve-success (chatroom-id message-count longest)
      (when (= (id chatroom) chatroom-id)
        (loop for i from 0 below message-count
              collect (get-message i chatroom))))
    (chat-room-messages-retrieve-failure (chatroom-id failure)
      (when (= (id chatroom) chatroom-id)
        (error "Failed to fetch messages: ~a" failure)))))

(defmethod send-message ((text string) (chatroom chatroom))
  (with-listener* (listener)
        (gog ichat-send-chat-room-message (handle 'chat) (id chatroom) text listener)
    (chat-room-message-send-success (chatroom-id index message-id send-time)
      (when (= (id chatroom) chatroom-id)
        (return-from listener (make-instance 'chat-message
                                             :chatroom chatroom
                                             :id message-id
                                             :type :message
                                             :sender (ensure-user T)
                                             :send-time (to-universal-time send-time)
                                             :text text))))
    (chat-room-message-send-failure (chatroom-id index failure)
      (when (= (id chatroom) chatroom-id)
        (error "Failed to send message: ~a" failure)))))

(defmethod get-message (i (chatroom chatroom))
  (cffi:with-foreign-objects ((id 'gog:chat-message-id)
                              (type 'gog:chat-message-type)
                              (sender 'gog:id)
                              (time :uint32)
                              (buffer :char 512))
    (let ((length (gog ichat-get-chat-room-message-by-index (handle 'chat) i id type sender time buffer 512)))
      (make-instance 'chat-message
                     :chatroom chatroom
                     :id (cffi:mem-ref id 'gog:chat-message-id)
                     :type (cffi:mem-ref type 'gog:chat-message-type)
                     :sender (ensure-user (cffi:mem-ref sender 'gog:id))
                     :send-time (to-universal-time (cffi:mem-ref time :uint32))
                     :text (cffi:foreign-string-to-lisp buffer :count length)))))

(defmethod members ((chatroom chatroom))
  (let* ((interface (handle 'chat))
         (count (gog ichat-get-chat-room-member-count interface (id chatroom))))
    (loop for i from 0 below count
          for id = (gog ichat-get-chat-room-member-user-idby-index interface (id chatroom) i)
          collect (ensure-user id))))

(defmethod unread-messages ((chatroom chatroom))
  (gog ichat-get-chat-room-unread-message-count (handle 'chat) (id chatroom)))

(defmethod mark-read ((chatroom chatroom))
  (gog ichat-mark-chat-room-as-read (handle 'chat) (id chatroom)))

(define-interface chat gog:chat
  (get-chatroom (user)
    (let ((user (ensure-user user)))
      (with-listener* (listener)
            (gog ichat-request-chat-room-with-user handle (id user) listener)
        (chat-room-with-user-retrieve-success (r-user chatroom-id)
          (when (eq user r-user)
            (return-from listener (make-instance 'chatroom :id chatroom-id))))
        (chat-room-with-user-retrieve-failure (r-user failure)
          (when (eq user r-user)
            (error "Failed to retrieve chatroom: ~a" failure)))))))
