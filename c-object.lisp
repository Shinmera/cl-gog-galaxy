(in-package #:org.shirakumo.fraf.gog-galaxy)

(defvar *c-object-table* (make-hash-table :test 'eql))

(defun pointer->object (pointer)
  (let ((address (etypecase pointer
                   (cffi:foreign-pointer (cffi:pointer-address pointer))
                   (integer pointer))))
    (gethash address *c-object-table*)))

(defun (setf pointer->object) (object pointer)
  (let ((address (etypecase pointer
                   (cffi:foreign-pointer (cffi:pointer-address pointer))
                   (integer pointer))))
    (if object
        (setf (gethash address *c-object-table*) object)
        (remhash address *c-object-table*))))

(defclass c-object ()
  ((handle :initarg :handle :initform NIL :accessor handle)))

(defmethod initialize-instance ((object c-object) &rest initargs)
  (call-next-method)
  (unless (handle object)
    (setf (handle object) (apply #'allocate-handle object initargs))))

(defmethod initialize-instance :around ((object c-object) &key handle)
  (if handle
      (call-next-method)
      (let ((ok NIL))
        (unwind-protect
             (prog1 (call-next-method)
               (setf ok T))
          (unless ok (free object))))))

(defmethod print-object ((c-object c-object) stream)
  (print-unreadable-object (c-object stream :type T)
    (format stream "@~d" (handle c-object))))

(defmethod handle (thing)
  (etypecase thing
    (integer thing)
    (cffi:foreign-pointer thing)))

(defmethod free ((object c-object))
  (let ((handle (when (slot-boundp object 'handle) (handle object))))
    (when handle
      (setf (handle object) NIL)
      (funcall (free-handle-function object handle)))))

(defgeneric allocate-handle (c-managed-object &key &allow-other-keys))
(defgeneric free-handle-function (c-managed-object handle))

(defclass c-registered-object (c-object)
  ())

(defmethod initialize-instance :after ((object c-registered-object) &key)
  (setf (pointer->object (handle object)) object))

(defmethod free-handle-function :around ((object c-registered-object) handle)
  (let ((next (call-next-method)))
    (lambda ()
      (setf (pointer->object handle) NIL)
      (funcall next))))

(defmethod free ((object c-registered-object))
  (when (slot-boundp object 'handle)
    (setf (pointer->object (handle object)) NIL)))

(defvar *interface-table* (make-hash-table :test 'eq))

(defclass interface (c-registered-object)
  ())

(defmethod initialize-instance :before ((object interface) &key)
  (when (gethash (type-of object) *interface-table*)
    (error "An instance fo the interface ~s already exists!"
           (type-of object))))

(defmethod initialize-instance :after ((object interface) &key)
  (setf (gethash (type-of object) *interface-table*) object))

(defmethod free-handle-function ((interface interface) handle)
  (lambda ()))

(defmethod interface ((name symbol))
  (or (gethash name *interface-table*)
      (make-instance name)))

(defmethod handle ((name symbol))
  (handle (interface name)))

(defun recall-arguments (lambda-list)
  (loop with kargs = NIL
        for arg in lambda-list
        if (find arg LAMBDA-LIST-KEYWORDS)
        do (if (eq arg '&key) (setf kargs T))
        else
        append (let ((name (if (listp arg) (first arg) arg)))
                 (if kargs
                     (list (intern (symbol-name name) "KEYWORD") name)
                     (list name)))))

(defmacro define-interface (name init &rest methods)
  `(progn (defclass ,name (interface) ())
          
          (defmethod allocate-handle ((interface ,name) &key)
            (,init))
          
          ,@(loop for (method args . body) in methods
                  append (if (listp method)
                             `((defmethod ,method (,(first args) (interface (eql T)) ,@(rest args))
                                 (setf (,(second method) (interface ',name) ,@(recall-arguments (rest args)))
                                       ,(first args)))
                               (defmethod ,method (,(first args) (interface ,name) ,@(rest args))
                                 (let ((handle (handle interface)))
                                   (declare (ignorable handle))
                                   ,@body)))
                             `((defmethod ,method ((interface (eql T)) ,@args)
                                 (,method (interface ',name) ,@(recall-arguments args)))
                               (defmethod ,method ((interface ,name) ,@args)
                                 (let ((handle (handle interface)))
                                   (declare (ignorable handle))
                                   ,@body)))))))

(trivial-indent:define-indentation define-interface (6 6 &rest (&whole 2 6 &body)))

(defun to-unix-time (universal)
  (- universal (encode-universal-time 0 0 0 1 1 1970 0)))

(defun to-universal-time (unix)
  (+ unix (encode-universal-time 0 0 0 1 1 1970 0)))
