(in-package #:org.shirakumo.fraf.gog-galaxy)

(define-condition gog-error (error)
  ((name :initarg :name :reader name)
   (message :initarg :message :initform NIL :reader message)
   (type :initarg :type :initform NIL :reader kind))
  (:report (lambda (c s) (format s "GOG Galaxy API failed with~@[ ~a~]~@[ (~a)~]~@[:~%  ~a~]"
                                 (name c) (kind c) (message c)))))

(defun gog-error (name &optional message &rest format-args)
  (error 'gog-error :name name :message (when message (format NIL "~?" message format-args))))

(defun check-error (&optional value)
  (let ((err (gog:get-error)))
    (if (cffi:null-pointer-p err)
        value
        (error 'gog-error
               :name (gog:ierror-get-name err)
               :message (gog:ierror-get-msg err)
               :type (gog:ierror-get-type err)))))

(defmacro gog (call &rest args)
  `(check-error (,(find-symbol (symbol-name call) '#:org.shirakumo.fraf.gog-galaxy.cffi) ,@args)))

(defun warn* (datum &rest args)
  (let ((condition (etypecase datum
                     (string (make-condition 'simple-warning :format-control datum :format-arguments args))
                     (symbol (apply #'make-condition 'datum args))
                     (condition datum))))
    (format *error-output* "~&WARNING: ~a~%" condition)
    (warn condition)))
