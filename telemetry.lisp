(in-package #:org.shirakumo.fraf.gog-galaxy)

(define-interface telemetry gog:telemetry
  (visit-id ()
    (gog itelemetry-get-visit-id handle))

  (reset-visit-id ()
    (gog itelemetry-reset-visit-id handle))

  (send-event (data event-type &key anonymous sampling-class)
    (gog:itelemetry-clear-params handle)
    (labels ((process (data &optional (key (cffi:null-pointer)))
               (etypecase data
                 (string
                  (gog:itelemetry-add-string-param handle key data))
                 (integer
                  (gog:itelemetry-add-int-param handle key data))
                 (rational
                  (gog:itelemetry-add-float-param handle key (float data 0d0)))
                 ((member NIL T)
                  (gog:itelemetry-add-bool-param handle key data))
                 (hash-table
                  (gog:itelemetry-add-object-param handle key)
                  (loop for key being the hash-keys of data using (hash-value value)
                        do (process value key))
                  (gog:itelemetry-close-param handle))
                 (vector
                  (gog:itelemetry-add-array-param handle key)
                  (loop for value across data
                        do (process value))
                  (gog:itelemetry-close-param handle)))))
      (process data))
    (when sampling-class
      (gog itelemetry-set-sampling-class handle sampling-class))
    (let ((index NIL))
      (with-listener* (listener)
            (setf index (if anonymous
                            (gog itelemetry-send-anonymous-telemetry-event handle event-type listener)
                            (gog itelemetry-send-telemetry-event handle event-type listener)))
        (telemetry-event-send-success (r-event-type r-index)
          (when (and (string= event-type r-event-type) (= index r-index))
            (return-from listener index)))
        (telemetry-event-send-failure (r-event-type r-index failure)
          (when (and (string= event-type r-event-type) (= index r-index))
            (error "Failed to send telemetry: ~a" failure)))))))
