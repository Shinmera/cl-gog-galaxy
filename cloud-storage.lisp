(in-package #:org.shirakumo.fraf.gog-galaxy)

(cffi:defcallback write-file :int ((file :pointer) (data :pointer) (size :int))
  (cffi:foreign-funcall "fwrite" :pointer data :size 1 :size size :pointer file :size))

(cffi:defcallback read-file :int ((file :pointer) (data :pointer) (size :int))
  (cffi:foreign-funcall "fread" :pointer data :size 1 :size size :pointer file :size))

(cffi:defcallback rewind-file :int ((file :pointer) (phase gog:read-phase))
  (cffi:foreign-funcall "fseek" :pointer file :long 0 :int 1 :size))

(defmacro with-file ((file path mode) &body body)
  `(let ((,file (cffi:foreign-funcall "fopen" :string ,path :string ,mode :pointer)))
     (when (cffi:null-pointer-p ,file)
       (gog-error NIL "Failed to open file ~a" ,path))
     (unwind-protect
          (let ((,file ,file)) ,@body)
       (cffi:foreign-funcall "fclose" :pointer ,file))))

(define-interface cloud-storage gog:cloud-storage
  (get-file (path &key (name (file-namestring path)) (container "default"))
    (with-file (file path "wb")
      (with-listener* (listener)
            (gog icloud-storage-get-file-callback handle container name file (cffi:callback write-file) listener)
        (get-file-success (r-container r-name file-size type id)
          (when (and (string= container r-container) (string= name r-name))
            (return-from listener (values file file-size type id))))
        (get-file-failure (r-container r-name failure)
          (when (and (string= container r-container) (string= name r-name))
            (gog-error failure))))))

  (list-files (&key (container "default"))
    (with-listener* (listener)
          (gog icloud-storage-get-file-list handle container listener)
      (get-file-list-success (count quota quota-used)
        (return-from listener
          (values (loop for i from 0 below count
                        collect (gog icloud-storage-get-file-name-by-index handle i))
                  quota quota-used)))
      (get-file-list-failure (failure)
        (gog-error failure))))

  (put-file (path &key (container "default") (name (file-namestring path)) (type :undefined) (hash (cffi:null-pointer)))
    (with-file (file path "rb")
      (with-listener* (listener)
            (gog icloud-storage-put-file-callback handle container name file (cffi:callback read-file) (cffi:callback rewind-file) listener type
                 (to-unix-time (file-write-date path)) hash)
        (put-file-success (r-container r-name)
          (when (and (string= container r-container) (string= name r-name))
            (return-from listener T)))
        (put-file-failure (r-container r-name failure)
          (when (and (string= container r-container) (string= name r-name))
            (gog-error failure))))))

  (remove-file (name &key (container "default") (hash (cffi:null-pointer)))
    (with-listener* (listener)
          (gog icloud-storage-delete-file handle container name listener hash)
      (put-file-success (r-container r-name)
        (when (and (string= container r-container) (string= name r-name))
          (return-from listener T)))
      (put-file-failure (r-container r-name failure)
        (when (and (string= container r-container) (string= name r-name))
          (gog-error failure)))))

  (open-savegame ()
    (gog icloud-storage-open-savegame handle))

  (close-savegame ()
    (gog icloud-storage-close-savegame handle)))
