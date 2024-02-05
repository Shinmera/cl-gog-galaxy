(in-package #:org.shirakumo.fraf.gog-galaxy)

(defclass achievement ()
  ((name :initarg :name :reader name)
   (achieved-p :initarg :achieved-p :accessor achieved-p)))

(defmethod visible-p ((achievement achievement))
  (gog istats-is-achievement-visible (handle 'stats) (name achievement)))

(defmethod display-name ((achievement achievement))
  (gog istats-get-achievement-display-name (handle 'stats) (name achievement)))

(defmethod description ((achievement achievement))
  (gog istats-get-achievement-description (handle 'stats) (name achievement)))

(defmethod (setf achieved-p) :before (value (achievement achievement))
  (if value
      (gog istats-set-achievement (handle 'stats) (name achievement))
      (gog istats-clear-achievement (handle 'stats) (name achievement))))

(define-interface stats gog:stats
  (achievement (name &optional (user T))
    (cffi:with-foreign-objects ((unlocked-p :bool) (unlock-time :uint32))
      (gog istats-get-achievement interface name unlocked-p unlock-time (id (ensure-user user)))
      (make-instance 'achievement :name name :achieved-p (cffi:mem-ref unlocked-p :bool))))

  (stat (name &optional (user T))
    (gog istats-get-stat-float interface name (id (ensure-user user))))

  ((setf stat) (value name)
    (gog istats-set-stat-float interface name (float value 0f0))
    value)

  (store ()
    (gog istats-store-stats-and-achievements interface (cffi:null-pointer)))

  (reset ()
    (gog istats-reset-stats-and-achievements interface (cffi:null-pointer))))
