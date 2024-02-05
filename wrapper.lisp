(in-package #:org.shirakumo.fraf.gog-galaxy)

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
      (unless (cffi:foreign-library-loaded-p 'gog:galaxy-c)
        (cffi:load-foreign-library 'gog:galaxy-c))
      (cffi:with-foreign-object (options '(:struct gog:init-options))
        (setf (gog:init-options-client-id options) client-id)
        (setf (gog:init-options-client-secret options) client-secret)
        (setf (gog:init-options-config-file-path options) (coerce-path config-file-path))
        (setf (gog:init-options-storage-path options) (coerce-path storage-path))
        (setf (gog:init-options-allocator options) (cffi:null-pointer))
        ;; TODO: lord, not bothering with this for now.
        (setf (gog:init-options-thread-factory options) (cffi:null-pointer))
        (setf (gog:init-options-host options) (or host (cffi:null-pointer)))
        (setf (gog:init-options-port options) (or port 0))
        (gog init options)
        (setf *init* T)))
    (loop for class being the hash-keys of *global-listeners* using (hash-value instance)
          do (if instance
                 (register-for T instance)
                 (make-instance class))))
  #-(or windows darwin)
  NIL)

(defun shutdown (&key preserve-static-objects)
  (when *init*
    (cffi:with-foreign-object (options '(:struct gog:shutdown-options))
      (loop for v being the hash-values of *global-listeners*
            do (when v (free v)))
      (setf (gog:shutdown-options-preserve-static-objects options) preserve-static-objects)
      (loop for v being the hash-values of *c-object-table*
            do (free v))
      (clrhash *interface-table*)
      (gog shutdown-ex options)
      (setf *init* NIL))))

(defun initialized-p ()
  *init*)

(defun process-data ()
  (gog process-data))
