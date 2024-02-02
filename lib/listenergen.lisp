(ql:quickload :cl-ppcre)

(defvar *sdk-dir*)

(defun find-def-args (name)
  (let ((out (make-string-output-stream)))
    (sb-ext:run-program "/usr/bin/ag" (list "-l" (format NIL "void ~a\\(" name)) :directory *sdk-dir* :output out)
    (let* ((filename (string-trim '(#\Linefeed) (get-output-stream-string out)))
           (src (uiop:read-file-string (merge-pathnames filename *sdk-dir*))))
      (cl-ppcre:register-groups-bind (args) ((format NIL "void ~a\\(((?:.|\\n)*?)\\) = 0;" name) src)
        args))))

(defun parse-line (line)
  (cl-ppcre:register-groups-bind (name args) (" *void +(\\w+)\\((.*)\\); *$" line)
    (let ((args (loop for arg in (cl-ppcre:split ", *" args)
                      collect (cl-ppcre:split " +" arg))))
      (values name
              (or (find-def-args name) (error "FUCK"))
              (loop for arg in args
                    collect (format NIL "(~{~a~^ ~})~a" (butlast arg) (first (last arg))))))))

(defun edit ()
  (with-open-file (s #p "~/a")
    (loop for line = (read-line s NIL NIL)
          while line
          do (multiple-value-bind (name gog-args cast-args)
                 (parse-line line)
               (format T "void ~a(~a) override {
    if(!this->listener.~a) return;
    this->listener.~a(this->listener.userptr~{, ~a~});
  }~%~%"
                       name gog-args name name cast-args)))))
