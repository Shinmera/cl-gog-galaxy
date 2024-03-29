(ql:quickload :cl-ppcre)

(defvar *sdk-dir*)
(defvar *here* #.(or *compile-file-pathname* *load-pathname*))

(defun find-listener-classes ()
  (let ((out (make-string-output-stream)))
    (sb-ext:run-program "/usr/bin/grep" (list* "-hi" " : public GalaxyTypeAwareListener"
                                               (mapcar #'uiop:native-namestring (uiop:directory-files *sdk-dir*)))
                        :directory *sdk-dir* :output out)
    (with-input-from-string (i (get-output-stream-string out))
      (loop for line = (read-line i NIL NIL)
            while line
            collect (cl-ppcre:register-groups-bind (name) (" *class (\\w+) :" line)
                      name)))))

(defun find-listener-definition (name)
  (let ((out (make-string-output-stream)))
    (sb-ext:run-program "/usr/bin/ag" (list "-l" (format NIL "~a : public GalaxyTypeAwareListener" name)) :directory *sdk-dir* :output out)
    (let* ((filename (string-trim '(#\Linefeed) (get-output-stream-string out)))
           (src (uiop:read-file-string (merge-pathnames filename *sdk-dir*))))
      (or (cl-ppcre:register-groups-bind (type clsdef) ((format NIL "~a : public GalaxyTypeAwareListener<([A-Z_]+)>\\s*\\{((?:.|\\n)*?)\\n		\\};" name) src)
            (let ((methods ()))
              (cl-ppcre:do-register-groups (name args) ("virtual void (\\w+)\\(((?:.|\\n)*?)\\) = 0;" clsdef)
                (push (list* name (loop for arg in (cl-ppcre:split ",\\s*" args)
                                        collect (cl-ppcre:split "\\s+" arg)))
                      methods))
              (list* name type methods)))
          (error "Can't find listener ~a" name)))))

(defun find-listener-delegate (name)
  (let* ((src (uiop:read-file-string (make-pathname :name "gog" :type "h" :defaults *here*))))
    (or (cl-ppcre:register-groups-bind (args) ((format NIL "void \\(\\*~a\\)\\((.*?)\\);" name) src)
          (list* name (loop for arg in (cl-ppcre:split ", *" args)
                            collect (cl-ppcre:split " +" arg))))
        (error "Can't find listener delegate ~a" name))))

(defun generate-listener-delegate (iface name args &optional (stream *standard-output*))
  (let ((gog-args (loop for arg in args
                        for type = (butlast arg)
                        for name = (car (last arg))
                        collect (if (string= (first type) "FailureReason")
                                    (format NIL "galaxy::api::~a::FailureReason ~a" iface name)
                                    (format NIL "~{~a~^ ~}" arg))))
        (cast-args (loop for arg in args
                         for def-arg in (cddr (find-listener-delegate name))
                         for type = (butlast arg)
                         for name = (car (last arg))
                         collect (cond ((string= "gog_ID" (first def-arg))
                                        (format NIL "(gog_ID)~a.ToUint64()" name))
                                       (T
                                        (format NIL "(~{~a~^ ~})~a" (butlast def-arg) name))))))
    (format stream "  void ~a(~{~a~^, ~}) override {
    if(!this->listener.~a) return;
    this->listener.~a(this->listener.userptr~{, ~a~});
  }" name gog-args name name cast-args)))

(defun generate-listener-definition (iface &optional (stream *standard-output*))
  (destructuring-bind (iface enum &rest methods) (find-listener-definition iface)
    (format stream "class _~a : public galaxy::api::~a {
public:
  struct gog_listener listener;"
            iface iface)
    (dolist (method methods)
      (format stream "~&~%")
      (generate-listener-delegate iface (car method) (cdr method) stream))
    (format stream "~%};

gog_Interface make_~a(struct gog_listener *listener){
  _~:*~a *list = new _~:*~a();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_~:*~a(gog_Interface listener){
  delete (_~:*~a*)listener;
}"
            iface)
    (list enum iface)))

(defun generate-listener (&optional (stream *standard-output*))
  (format stream "#include <galaxy/GalaxyApi.h>
#include <cstring>
#include \"gog.h\"

using namespace galaxy::api;

typedef gog_Interface (*makefun)(struct gog_listener *listener);
typedef void (*freefun)(gog_Interface listener);

makefun gog_listener_make[GOG_LISTENER_TYPE_END] = {0};
freefun gog_listener_free[GOG_LISTENER_TYPE_END] = {0};

GOG_EXPORT gog_Interface gog_MakeListener(gog_listener *listener, enum gog_ListenerType type){
  return gog_listener_make[type](listener);
}

GOG_EXPORT void gog_FreeListener(gog_Interface listener, enum gog_ListenerType type){
  gog_listener_free[type](listener);
}")
  (let ((listeners (loop for iface in (find-listener-classes)
                         do (format stream "~&~%")
                         collect (generate-listener-definition iface stream))))
    (format stream "

struct __iface_init {
  __iface_init() {
    ~{~{~
    gog_listener_make[~a] = make_~a;~:*~:*
    gog_listener_free[~a] = free_~a;
    ~}~}
  }
};
__iface_init __iface_init_;" listeners)))

(defun generate-listener-file ()
  (with-open-file (stream (make-pathname :name "listener" :type "cpp" :defaults *here*)
                          :direction :output :if-exists :supersede)
    (generate-listener stream)))

(defun find-interface-def-args (iface name)
  (let* ((filename (format NIL "~a.h" iface))
         (src (uiop:read-file-string (merge-pathnames filename *sdk-dir*))))
    (or (cl-ppcre:register-groups-bind (args) ((format NIL "virtual (?:.*?)~a\\(((?:.|\\n)*?)\\)(?: const)? = 0;" name) src)
          (return-from find-interface-def-args
            (loop for arg in (cl-ppcre:split ", *" args)
                  collect (or (cl-ppcre:register-groups-bind (arg) ("^(?:\\W|\\n)*(.*?)(?: = .*)?(?:\\W|\\n)*$" arg)
                                arg)
                              (error "Can't parse ~s" arg)))))
        (error "Can't find method for ~a::~a" iface name))))

(defun parse-interface-line (line)
  (or (cl-ppcre:register-groups-bind (rettype iface method args) ("^ *(.+?) +gog_([^_ ]+)_(\\w+) *\\((.*)\\); *$" line)
        (let ((args (cl-ppcre:split ", *" args)))
          (list iface method (if (string/= "void" rettype) rettype) args
                (loop for out-arg in (find-interface-def-args iface method)
                      for in-arg in (rest args)
                      collect (format NIL "(~{~a~^ ~})~a"
                                      (butlast (cl-ppcre:split " +" out-arg))
                                      (car (last (cl-ppcre:split " +" in-arg))))))))
      (error "Failed to parse line:~%  ~a" line)))

(defun interface-methods ()
  (with-open-file (s #p"~/a")
    (with-open-file (o (merge-pathnames "interfaces.cpp" *here*) :direction :output :if-exists :supersede)
      (format o "#include <galaxy/GalaxyApi.h>
#include \"gog.h\"

using namespace galaxy::api;
typedef galaxy::api::ICloudStorage::RewindFunc RewindFunc;
typedef galaxy::api::ICloudStorage::ReadFunc ReadFunc;
typedef galaxy::api::ICloudStorage::WriteFunc WriteFunc;
~%")
      (loop for line = (read-line s NIL NIL)
            while line
            do (with-simple-restart (continue "Ignore the definition line.")
                 (destructuring-bind (iface method rettype cargs gargs) (parse-interface-line line)
                   (format o "GOG_EXPORT ~:[void~;~:*~a~] gog_~a_~a(~{~a~^, ~}){
  ~:[~;return ~:*(~a)~]((galaxy::api::~a*)~a)->~a(~{~a~^, ~})~@[~a~];
}~%~%" rettype iface method cargs rettype iface (subseq (first cargs) (length "gog_Interface ")) method gargs
(when (string= "gog_ID" rettype) ".ToUint64()"))))))))
