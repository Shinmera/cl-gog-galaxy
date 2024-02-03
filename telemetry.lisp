(in-package #:org.shirakumo.fraf.gog-galaxy)

(define-interface telemetry gog:telemetry
  (visit-id ()
    (gog itelemetry-get-visit-id interface))

  (reset-visit-id ()
    (gog itelemetry-reset-visit-id interface))

  (send-event (data event-type &key anonymous sampling-class)
    (gog:itelemetry-clear-params interface)
    (labels ((process (data &optional (key (cffi:null-pointer)))
               (etypecase data
                 (string
                  (gog:itelemetry-add-string-param interface key data))
                 (integer
                  (gog:itelemetry-add-int-param interface key data))
                 (rational
                  (gog:itelemetry-add-float-param interface key (float data 0d0)))
                 ((member NIL T)
                  (gog:itelemetry-add-bool-param interface key data))
                 (hash-table
                  (gog:itelemetry-add-object-param interface key)
                  (loop for key being the hash-keys of data using (hash-value value)
                        do (process value key))
                  (gog:itelemetry-close-param interface))
                 (vector
                  (gog:itelemetry-add-array-param interface key)
                  (loop for value across data
                        do (process value))
                  (gog:itelemetry-close-param interface)))))
      (process data))
    (when sampling-class
      (gog itelemetry-set-sampling-class interface sampling-class))
    (let ((index NIL))
      (with-listener* (listener)
            (setf index (if anonymous
                            (gog itelemetry-send-anonymous-telemetry-event interface event-type listener)
                            (gog itelemetry-send-telemetry-event interface event-type listener)))
        (telemetry-event-send-success (r-event-type r-index)
          (when (and (string= event-type r-event-type) (= index r-index))
            (return-from listener index)))
        (telemetry-event-send-failure (r-event-type r-index failure)
          (when (and (string= event-type r-event-type) (= index r-index))
            (error "Failed to send telemetry: ~a" failure)))))))
