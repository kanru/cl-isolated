;;;; Eval-bot loader

(flet ((probe-load (path &optional (default (user-homedir-pathname)))
         (let ((path (merge-pathnames path default)))
           (when (probe-file path) (load path))))
       (funcallstr (string &rest args)
         (apply (read-from-string string) args)))
  (or (probe-load #p"quicklisp/setup.lisp")
      (probe-load #p".quicklisp/setup.lisp")
      (let ((url "http://beta.quicklisp.org/quicklisp.lisp")
            (init (nth-value 1 (progn
                                 (require :sb-posix)
                                 (funcallstr "sb-posix:mkstemp"
                                             "/tmp/quicklisp-XXXXXX")))))
        (unwind-protect
             (progn
               (sb-ext:run-program "wget" (list "-O" init "--" url)
                                   :search t :output t)
               (when (probe-load init)
                 (funcallstr "quicklisp-quickstart:install")))
          (delete-file init)))))

(pushnew (make-pathname :directory (pathname-directory *load-pathname*))
         asdf:*central-registry*)

(ql:quickload '("swank" "eval-bot"))

(in-package #:eval-bot)

(definitions-load)

(defparameter *freenode*
  (make-client :server "irc.freenode.net"
               :nickname "clbot"
               :username "clbot"
               :realname "Common Lisp bot"
               :listen-targets nil
               :auto-join nil))

(loop :for port :from 50000 :upto 50050
      :do (handler-case
              (return (swank:create-server :port port :dont-close t))
            (sb-bsd-sockets:address-in-use-error () nil)))
