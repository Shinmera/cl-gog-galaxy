(in-package #:org.shirakumo.fraf.gog-galaxy)

(define-interface networking gog:networking
  (nat-type ()
    (gog inetworking-request-nat-type-detection handle)
    (gog inetworking-get-nat-type handle))

  (connection-type (user)
    (gog inetworking-get-connection-type handle (id (ensure-user user))))

  (ping-with (user)
    (gog inetworking-get-ping-with handle (id (ensure-user user))))

  (send-packet (user data &key (type :reliable) (channel 0))
    (cffi:with-pointer-to-vector-data (ptr data)
      (gog inetworking-send-p2p-packet handle (id (ensure-user user)) ptr (length data) type channel)))

  (read-packet (user data &key (channel 0))
    (cffi:with-pointer-to-vector-data (ptr data)
      (cffi:with-foreign-objects ((read :uint32)
                                  (sender 'gog:id))
        (when (gog inetworking-read-p2p-packet handle ptr (length data) read sender channel)
          (values (cffi:mem-ref read :uint32)
                  (ensure-user (cffi:mem-ref sender 'gog:id)))))))

  (peek-packet (user data &key (channel 0))
    (cffi:with-pointer-to-vector-data (ptr data)
      (cffi:with-foreign-objects ((read :uint32)
                                  (sender 'gog:id))
        (when (gog inetworking-peek-p2p-packet handle ptr (length data) read sender channel)
          (values (cffi:mem-ref read :uint32)
                  (ensure-user (cffi:mem-ref sender 'gog:id)))))))

  (pop-packet (&key (channel 0))
    (gog inetworking-pop-p2p-packet handle channel))

  (packet-available-p (&key (channel 0))
    (cffi:with-foreign-objects ((read :uint32))
      (when (gog inetworking-is-p2p-packet-available handle read channel)
        (cffi:mem-ref read :uint32)))))

(defclass connection ()
  ((id :initarg :id :reader id)))

(defmethod close ((connection connection) &key abort)
  (declare (ignore abort))
  (gog:icustom-networking-close-connection (handle 'custom-networking) (id connection)))

(defmethod send-data ((connection connection) data)
  (cffi:with-pointer-to-vector-data (ptr data)
    (gog icustom-networking-send-data (handle 'custom-networking) (id connection) ptr (length data))))

(defmethod read-data ((connection connection) data)
  (cffi:with-pointer-to-vector-data (ptr data)
    (gog icustom-networking-read-data (handle 'custom-networking) (id connection) ptr (length data))))

(defmethod peek-data ((connection connection) data)
  (cffi:with-pointer-to-vector-data (ptr data)
    (gog icustom-networking-peek-data (handle 'custom-networking) (id connection) ptr (length data))))

(defmethod pop-data ((connection connection) size)
  (gog icustom-networking-pop-data (handle 'custom-networking) (id connection) size))

(defmethod available-data ((connection connection))
  (gog icustom-networking-get-available-data-size (handle 'custom-networking) (id connection)))

(define-interface custom-networking gog:custom-networking
  (open-connection (string)
    (with-listener* (listener)
          (gog:icustom-networking-open-connection handle string listener)
      (connection-open-success (r-string id)
        (when (string= string r-string)
          (return-from listener (make-instance 'connection :id id))))
      (connection-open-failure (r-string failure)
        (when (string= string r-string)
          (error "Failed to open connection: ~a" failure))))))
