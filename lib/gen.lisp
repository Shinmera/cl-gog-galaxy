(ql:quickload :cl-ppcre)

(defvar *sdk-dir*)
(defvar *here* #.(or *compile-file-pathname* *load-pathname*))

(defun find-listener-defs ()
  (let ((out (make-string-output-stream)))
    (sb-ext:run-program "/usr/bin/grep" (list* "-hi" " : public GalaxyTypeAwareListener"
                                               (mapcar #'uiop:native-namestring (uiop:directory-files *sdk-dir*)))
                        :directory *sdk-dir* :output out)
    (with-input-from-string (i (get-output-stream-string out))
      (loop for line = (read-line i NIL NIL)
            while line
            collect (cl-ppcre:register-groups-bind (name) (" *class (\\w+) :" line)
                      name)))))

(defun find-listener-def (name)
  (let ((out (make-string-output-stream)))
    (sb-ext:run-program "/usr/bin/ag" (list "-l" (format NIL "void ~a\\(" name)) :directory *sdk-dir* :output out)
    (let* ((filename (string-trim '(#\Linefeed) (get-output-stream-string out)))
           (src (uiop:read-file-string (merge-pathnames filename *sdk-dir*))))
      (or (cl-ppcre:register-groups-bind (iface args) ((format NIL "class (\\w+) : (?:(?! : )(?:.|\\n))*void ~a\\(((?:.|\\n)*?)\\) = 0;" name) src)
            (return-from find-listener-def
              (list iface (loop for arg in (cl-ppcre:split ", *" args)
                                collect (cl-ppcre:split " +" arg)))))
          (error "Can't find method for ~a" name)))))

(defun parse-listener-line (line)
  (or (cl-ppcre:register-groups-bind (name args) (" *void +(?:\\(\\*)(\\w+)\\)?\\((.*)\\); *$" line)
        (let ((args (rest (loop for arg in (cl-ppcre:split ", *" args)
                                collect (cl-ppcre:split " +" arg)))))
          (destructuring-bind (iface def-args) (find-listener-def name)
            (list name
                  (loop for arg in def-args
                        for type = (butlast arg)
                        for name = (car (last arg))
                        collect (if (string= (first type) "FailureReason")
                                    (format NIL "galaxy::api::~a::FailureReason ~a" iface name)
                                    (format NIL "~{~a~^ ~}" arg)))
                  (loop for arg in args
                        for def in def-args
                        for type = (butlast arg)
                        for name = (first (last def))
                        collect (if (string= "gog_ID" (first type))
                                    (format NIL "(gog_ID)~a.ToUint64()" name)
                                    (format NIL "(~{~a~^ ~})~a" type name)))))))
      (error "Failed to parse line~%  ~a" line)))

(defun listener-methods ()
  (with-open-file (s #p"~/a")
    (with-open-file (o (merge-pathnames "listener.cpp" *here*) :direction :output :if-exists :supersede)
      (format o "#include <galaxy/GalaxyApi.h>
#include <cstring>
#include \"gog.h\"

using namespace galaxy::api;

class Listener : ~{~%    public ~a~^,~} {
public:
  struct gog_listener listener;" (find-listener-defs))
      (loop for line = (read-line s NIL NIL)
            while line
            do (destructuring-bind (name gog-args cast-args) (parse-listener-line line)
                 (format o "~%~%void ~a(~{~a~^, ~}) override {
  if(!this->listener.~a) return;
  this->listener.~a(this->listener.userptr~{, ~a~});
}"
                         name gog-args name name cast-args)))
      (format o "
};

GOG_EXPORT gog_Interface gog_MakeListener(struct gog_listener *listener){
  Listener *list = new Listener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list
  return (gog_Interface)list;
}

GOG_EXPORT void gog_FreeListener(gog_Interface listener){
  delete (Listener*)listener;
}"))))

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
