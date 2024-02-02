(in-package #:org.shirakumo.fraf.gog-galaxy.cffi)

(defvar *here* #.(or *compile-file-pathname* *load-pathname* *default-pathname-defaults*))
(defvar *static* (make-pathname :name NIL :type NIL :defaults (merge-pathnames "static/" *here*)))
(pushnew *static* cffi:*foreign-library-directories*)

(cffi:define-foreign-library galaxy
  (:darwin (:or "mac-amd64-galaxy-c.dylib"))
  (:linux (:or "lin-amd64-galaxy-c.so"))
  (:windows (:or "win-amd64-galaxy-c.dll"))
  (T (:default "galaxy-c")))

(cffi:defcstruct init-options
  (client-id :string)
  (client-secret :string)
  (config-file-path :string)
  (storage-path :string)
  (galaxy-allocator :pointer)
  (galaxy-thread-factory :pointer)
  (host :string)
  (port :uint16))

(cffi:defcstruct shutdown-options
  (preserve-static-objects :bool))

(cffi:defcenum error-type
  :unauthorized-access
  :invalid-argument
  :invalid-state
  :runtime-error)

(cffi:defcfun (get-error "GOG_GetError") :pointer)

(cffi:defcfun (error-name "GOG_IError_GetName") :string
  (error :pointer))

(cffi:defcfun (error-message "GOG_IError_GetMsg") :string
  (error :pointer))

(cffi:defcfun (error-type "GOG_IError_GetType") error-type
  (error :pointer))
