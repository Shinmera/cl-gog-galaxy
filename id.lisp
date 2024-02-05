(in-package #:org.shirakumo.fraf.gog-galaxy)

(defvar *id-object-table* (make-hash-table :test 'eql))

(defclass id-registered-object ()
  ((id :initarg :id :reader id)))

(defmethod initialize-instance :before ((object id-registered-object) &key id)
  (when (gethash id *id-object-table*)
    (error "An object with id ~16,'0x is already registered!" id)))

(defmethod initialize-instance :after ((object id-registered-object) &key)
  (setf (gethash (id object) *id-object-table*) object))

(defclass user (id-registered-object)
  ())

(defun ensure-user (id)
  (etypecase id
    (integer
     (unless (eq :user (gog:id-type id))
       (error "ID ~16,'0x is not a user id!" id))
     (or (gethash id *id-object-table*)
         (make-instance 'user :id id)))
    (user
     id)
    ((eql T)
     (ensure-user (gog iuser-get-galaxy-id (gog:user))))
    (string
     (find-user T id))))

(defclass lobby (id-registered-object)
  ())

(defun ensure-lobby (id)
  (etypecase id
    (integer
     (unless (eq :lobby (gog:id-type id))
       (error "ID ~16,'0x is not a lobby id!" id))
     (or (gethash id *id-object-table*)
         (make-instance 'lobby :id id)))
    (lobby
     id)))
