(in-package #:org.shirakumo.fraf.gog-galaxy)

(define-condition gog-error (error)
  ((name :initarg :name :reader name)
   (message :initarg :message :reader message)
   (type :initarg :type :reader kind))
  (:report (lambda (c s) (format s "GOG Galaxy API failed with ~a (~a):~%  ~a"
                                 (name c) (kind c) (message c)))))

(defun check-error ()
  (let ((err (gog:get-error)))
    (unless (cffi:null-pointer-p err)
      (error 'gog-error
             :name (gog:ierror-get-name err)
             :message (gog:ierror-get-msg err)
             :type (gog:ierror-get-type err)))))

(defvar *init* NIL)

(defun init (client-id client-secret &key config-file-path storage-path host port)
  #+(or windows darwin)
  (unless *init*
    (flet ((coerce-path (path)
             (etypecase path
               (NIL (cffi:null-pointer))
               (string path)
               (pathname (uiop:native-namestring path)))))
      (unless (cffi:foreign-library-loaded-p 'gog:galaxy)
        (cffi:load-foreign-library 'gog:galaxy))
      (cffi:with-foreign-object (options '(:struct gog:init-options))
        (setf (gog:init-options-client-id options) client-id)
        (setf (gog:init-options-client-secret options) client-secret)
        (setf (gog:init-option-config-file-path options) (coerce-path config-file-path))
        (setf (gog:init-option-storage-path options) (coerce-path storage-path))
        (setf (gog:init-option-allocator options) (cffi:null-pointer))
        (setf (gog:init-option-thread-factory options) (cffi:null-pointer))
        (setf (gog:init-option-host options) (or host (cffi:null-pointer)))
        (setf (gog:init-option-port options) (or port 0))
        (gog:init options)
        (check-error)
        (setf *init* T))))
  #-(or windows darwin)
  NIL)

(defun shutdown (&key preserve-static-objects)
  (when *init*
    (cffi:with-foreign-object (options '(:struct gog:shutdown-options))
      (setf (gog:shutdown-options-preserve-static-objects options) preserve-static-objects)
      (gog:shutdown-ex options)
      (check-error)
      (setf *init* NIL))))

(defun initialized-p ()
  *init*)
