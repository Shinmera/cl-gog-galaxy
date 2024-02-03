(in-package #:org.shirakumo.fraf.gog-galaxy)

(define-interface networking gog:networking
  (nat-type ()
    (gog inetworking-request-nat-type-detection interface)
    (gog inetworking-get-nat-type interface))

  (connection-type (user)
    (gog inetworking-get-connection-type interface (id (ensure-user user))))

  (ping-with (user)
    (gog inetworking-get-ping-with interface (id (ensure-user user))))

  (send-packet (user data &key (type :reliable) (channel 0))
    (cffi:with-pointer-to-vector-data (ptr data)
      (gog inetworking-send-p2p-packet interface (id (ensure-user user)) ptr (length data) type channel)))

  (read-packet (user data &key (channel 0))
    (cffi:with-pointer-to-vector-data (ptr data)
      (cffi:with-foreign-objects ((read :uint32)
                                  (sender 'gog:id))
        (when (gog inetworking-read-p2p-packet interface ptr (length data) read sender channel)
          (values (cffi:mem-ref read :uint32)
                  (ensure-user (cffi:mem-ref sender 'gog:id)))))))

  (peek-packet (user data &key (channel 0))
    (cffi:with-pointer-to-vector-data (ptr data)
      (cffi:with-foreign-objects ((read :uint32)
                                  (sender 'gog:id))
        (when (gog inetworking-peek-p2p-packet interface ptr (length data) read sender channel)
          (values (cffi:mem-ref read :uint32)
                  (ensure-user (cffi:mem-ref sender 'gog:id)))))))

  (pop-packet (&key (channel 0))
    (gog inetworking-pop-p2p-packet interface channel))

  (packet-available-p (&key (channel 0))
    (cffi:with-foreign-objects ((read :uint32))
      (when (gog inetworking-is-p2p-packet-available interface read channel)
        (cffi:mem-ref read :uint32)))))

(define-interface custom-networking gog:custom-networking)
