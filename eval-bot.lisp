;;;; Eval-bot --- An IRC bot for CL eval and help

;; Copyright (C) 2012-2013 Teemu Likonen <tlikonen@iki.fi>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public
;; License along with this program. If not, see
;; <http://www.gnu.org/licenses/>.

(defpackage #:eval-bot
  (:use #:cl #:general))

(in-package #:eval-bot)

(declaim (optimize (safety 3)))

;;; Misc

(defvar *enabled* t)

(defun enable () (setf *enabled* t))
(defun disable () (setf *enabled* nil))

;;; Clients

(defclass client (trivial-irc:client)
  ((send-queue :reader send-queue :initform (make-instance 'queue))
   (input-queue :reader input-queue :initform (make-instance 'queue))
   (listen-targets :accessor listen-targets :initarg :listen-targets
                   :initform nil)
   (auto-join :accessor auto-join :initarg :auto-join :initform nil)
   (send-queue-thread :accessor send-queue-thread :initform nil)
   (input-queue-thread :accessor input-queue-thread :initform nil)
   (server-message-thread :accessor server-message-thread :initform nil)
   (nick-uniq-count :accessor nick-uniq-count :initform 1)))

(defun connectedp (client)
  (ignore-errors (trivial-irc:connected-p client)))

(defun make-client (&key server (port 6667) password
                      nickname username realname listen-targets auto-join)
  (make-instance 'client :server server :port port :password password
                 :nickname nickname :username username :realname realname
                 :listen-targets listen-targets :auto-join auto-join))

;;; Messages

(defvar *eval-prefix* ",")
(defvar *command-prefix* "!")
(defvar *local-stream* *terminal-io*)
(defvar *irc-message-max-length* 400)

(defclass message ()
  ((timestamp :reader timestamp :initform (get-universal-time))))

(defclass server-message (message)
  ((command :reader command :initarg :command)
   (prefix :reader prefix :initarg :prefix)
   (arguments :reader arguments :initarg :arguments)))

(defclass server-privmsg (server-message)
  ((tell-intro :reader tell-intro :initarg :tell-intro :initform nil)))
(defclass server-privmsg-eval (server-privmsg) nil)
(defclass server-privmsg-cmd (server-privmsg) nil)

(defclass client-message (message) nil)

(defclass client-privmsg (client-message)
  ((command :reader command :initform "PRIVMSG")
   (target :accessor target :initarg :target)
   (contents :accessor contents :initarg :contents)))
(defclass client-action (client-privmsg) nil)

(defconstant +action-char+ (code-char 1))

(defun action-to-string (action-string)
  (let ((len (length action-string)))
    (when (and (>= len 9)
               (string= (format nil "~CACTION " +action-char+)
                        (subseq action-string 0 8))
               (char= +action-char+ (elt action-string (1- len))))
      (subseq action-string 8 (1- len)))))

(defun string-to-action (string)
  (if (action-to-string string)
      string
      (format nil "~CACTION ~A~C" +action-char+ string +action-char+)))

(defun iso-time (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (sec min hour date month year day dst tz)
      (decode-universal-time universal-time 0)
    (declare (ignore dst day tz))
    (format nil "~D~2,'0D~2,'0DT~2,'0D~2,'0D~2,'0DZ"
            year month date hour min sec)))

(defun truncate-message (string)
  ;; FIXME: This counts CL character objects but IRC protocol is about
  ;; bytes.
  (if (> (length string) *irc-message-max-length*)
      (concatenate 'string (subseq string 0 (- *irc-message-max-length* 3))
                   "...")
      string))

(defgeneric send (context message))

(defmethod send ((client client) (message client-privmsg))
  (trivial-irc:send-privmsg client (target message)
                            (truncate-message (contents message))))

(defmethod send ((client client) (message client-action))
  (trivial-irc:send-privmsg client (target message)
                            (string-to-action (truncate-message
                                               (contents message)))))

(defmethod send ((target stream) (message server-message))
  (format target "~&~A ~A ~{~A~^ ~}~%"
          (command message) (prefix message) (arguments message)))

(defmethod send ((target stream) (message server-privmsg))
  (format target "~&~A [~A] <~A> (~A)~%> ~A~%"
          (command message)
          (iso-time (timestamp message))
          (prefix message)
          (first (arguments message))
          (second (arguments message))))

(defmethod send ((target stream) (message client-privmsg))
  (format target "~&~A~A [~A] *ME* (~A)~%> ~A~%"
          (command message)
          (if (typep message 'client-action) "/ACTION" "")
          (iso-time (timestamp message))
          (target message)
          (truncate-message (contents message))))

(defmethod send ((target stream) (message string))
  (format target "~&~A~%" message))

(defmethod send ((target (eql :terminal)) message)
  (send *local-stream* message))

(defun bot-message (format-string &rest args)
  (apply #'format nil format-string args))

(defun bot-comment (format-string &rest args)
  (apply #'format nil (concatenate 'string ";; " format-string)
         args))

;;; General maintainer

(defvar *maintainer-thread* nil)
(defvar *maintainer-interval* 60)

(defun start-maintainer ()
  (when (and (bt:threadp *maintainer-thread*)
             (bt:thread-alive-p *maintainer-thread*))
    (bt:destroy-thread *maintainer-thread*))
  (setf *maintainer-thread*
        (with-thread ("eval-bot maintainer")
          (loop (sleep *maintainer-interval*)
                (ignore-errors
                  (delete-unused-packages))))))

;;; IRC

(defvar *default-quit-message* "clbot")

(defun irc-raw (client raw-message)
  (trivial-irc:send-raw-message client raw-message))

(defun irc-quit (client &optional (message *default-quit-message*))
  (queue-clear (input-queue client))
  (queue-clear (send-queue client))
  (trivial-irc:disconnect client :message message))

(defun irc-join (client channel &optional password)
  (irc-raw client (format nil "JOIN ~A~@[ ~A~]" channel password)))

(defun irc-part (client channel &optional message)
  (irc-raw client (format nil "PART ~A~@[ :~A~]" channel message)))

(defun irc-mode (client target &rest args)
  (irc-raw client (format nil "MODE ~A ~{~A~^ ~}" target args)))

(defun irc-nick (client nick)
  (irc-raw client (format nil "NICK ~A" nick)))

(defun irc-msg (client target message)
  (let ((msg (make-instance 'client-privmsg :target target :contents message)))
    (queue-add (send-queue client) msg)
    (send :terminal msg)))

(defun start-server-message-handler (client)
  (let ((thread (server-message-thread client)))
    (when (and (bt:threadp thread)
               (bt:thread-alive-p thread))
      (bt:destroy-thread thread)))
  (setf (server-message-thread client)
        (with-thread ("server message handler")
          (block nil
            (handler-bind

                ((trivial-irc:connection-lost
                  (lambda (c)
                    (send :terminal (format nil "~A" (type-of c)))
                    (ignore-errors (irc-quit client))
                    (with-thread ("eval-bot reconnect")
                      #+sbcl (declare (sb-ext:muffle-conditions style-warning))
                      (sleep 5)
                      (irc-connect client))
                    (return)))

                 (t (lambda (c)
                      (send :terminal (format nil "~A: ~A" (type-of c) c))
                      (sleep 2)
                      (invoke-restart 'ignore))))

              (loop :while (connectedp client)
                    :do (with-simple-restart (ignore "Continue handling")
                          (trivial-irc:receive-message client))))))))

(defun irc-connect (client)
  (loop :for slot :in '(server-message-thread send-queue-thread
                        input-queue-thread)
        :for value := (slot-value client slot)
        :if (and (bt:threadp value)
                 (bt:thread-alive-p value))
        :do
        (bt:destroy-thread value)
        (setf (slot-value client slot) nil))

  (setf (nick-uniq-count client) 1)
  (queue-clear (input-queue client))
  (queue-clear (send-queue client))

  (handler-case (trivial-irc:connect client)
    (trivial-irc:connection-failed ()
      (send :terminal "Connection failed.")
      (ignore-errors (irc-quit client))
      (with-thread ("eval-bot reconnect")
        (sleep 5)
        (irc-connect client))
      (return-from irc-connect)))

  (when (connectedp client)
    (start-server-message-handler client)
    client))

;;; Handlers

(defvar *eval-timeout* .2)

(defun clean-string (string)
  (delete-if-not #'graphic-char-p
                 (substitute-if #\Space (lambda (char)
                                          (member char '(#\Newline #\Tab)))
                                string)))

(defun sandbox-repl (sandbox-name string &optional (stream *standard-output*))
  (update-sandbox-usage sandbox-name)
  (let ((sandbox-impl:*sandbox* sandbox-name))
    (sandbox-impl:repl string stream)))

(defun sandbox-init (sandbox-name)
  (update-sandbox-usage sandbox-name)
  (unless (find-package sandbox-name)
    (let ((sandbox-impl:*sandbox* sandbox-name))
      (sandbox-impl:reset))))

(defun send-message-or-tell-intro (client message)
  (let ((intro (tell-intro message)))
    (if intro
        (progn (send :terminal intro)
               (queue-add (send-queue client) intro))
        (send :terminal message))))

(defgeneric handle-input-message (client message))

(defmethod handle-input-message ((client client) (message server-privmsg-eval))
  (let ((target (first (arguments message)))
        (contents (subseq (second (arguments message))
                          (length *eval-prefix*)))
        (sandbox-name (user-to-sandbox-name (prefix message))))

    (send-message-or-tell-intro client message)
    (sandbox-init sandbox-name)

    (with-thread ("eval and print" :timeout *eval-timeout*)
      (let ((string (clean-string (with-output-to-string (stream)
                                    (sandbox-repl sandbox-name
                                                  contents stream)))))
        (when (plusp (length string))
          (let ((msg (make-instance 'client-privmsg :target target
                                    :contents string)))
            (send :terminal msg)
            (queue-add (send-queue client) msg))))

      :timeout
      (let ((msg (make-instance 'client-privmsg :target target
                                :contents (bot-comment "EVAL-TIMEOUT"))))
        (send :terminal msg)
        (queue-add (send-queue client) msg)))))

(defun nth-word (n string)
  (let ((words (split-sequence:split-sequence
                #\Space string :remove-empty-subseqs nil)))
    (loop :with word-number := -1
          :for (word . rest) :on words
          :if (plusp (length word)) :do (incf word-number)
          :if (and (plusp (length word)) (= n word-number))
          :return
          (values word (string-left-trim
                        " " (with-output-to-string (s)
                              (loop :for (item . rest) :on rest
                                    :do (princ item s)
                                    :if rest :do (princ #\Space s)))))
          :finally (return (values nil nil)))))

(defun match-prefix-p (prefix string)
  (string= prefix string :end2 (min (length prefix) (length string))))

(defvar *command-help-strings*
  (let ((fmt
         '(";; ~Aexpression ...           Eval expression(s) in your package."
           ";; ~Ahelp                     This help message."
           ";; ~Asource                   Show the URL to bot's source code."
           ";; ~Atell <target> <command>  Send <command>'s output to <target>."))
        strings)
    (push (bot-message (first fmt) *eval-prefix*) strings)
    (loop :for line :in (rest fmt)
          :do (push (bot-message line *command-prefix*) strings))
    (nreverse strings)))

(defun cmd-help (client target)
  (send :terminal (format nil "[Sending help strings to ~A.]" target))
  (loop :for line :in *command-help-strings*
        :for msg := (make-instance 'client-privmsg
                                   :target target
                                   :contents (bot-message line))
        :do (queue-add (send-queue client) msg)))

(defvar *source-code-url* "https://github.com/tlikonen/cl-eval-bot")

(defun cmd-source (client target)
  (let* ((new (make-instance 'client-privmsg
                             :target target
                             :contents (bot-message "Bot's source code: ~A"
                                                    *source-code-url*))))
    (send :terminal new)
    (queue-add (send-queue client) new)))

(defun cmd-tell (client message line)
  (let ((word1 (nth-word 1 line))
        (word2 (nth-word 2 line))
        (rest (nth-value 1 (nth-word 1 line))))
    (when (and word1 word2
               (or (match-prefix-p *command-prefix* word2)
                   (match-prefix-p *eval-prefix* word2)))
      (send :terminal message)
      (handle-input-message
       client (make-instance
               'server-privmsg
               :command (command message)
               :prefix (prefix message)
               :arguments (list word1 rest)
               :tell-intro (make-instance
                            'client-privmsg
                            :target word1
                            :contents (bot-message "User \"~A\" tells: ~A"
                                                   (trivial-irc:prefix-nickname
                                                    (prefix message))
                                                   rest)))))))

(defmethod handle-input-message ((client client) (message server-privmsg-cmd))
  (let ((target (first (arguments message)))
        (line (subseq (second (arguments message)) (length *command-prefix*))))

    (cond

      ((string-equal "help" (nth-word 0 line))
       (send-message-or-tell-intro client message)
       (cmd-help client target))

      ((string-equal "source" (nth-word 0 line))
       (send-message-or-tell-intro client message)
       (cmd-source client target))

      ((and (string-equal "tell" (nth-word 0 line))
            (not (tell-intro message)))
       (cmd-tell client message line)))))

(defvar *max-input-queue-length* 10)

(defmethod handle-input-message ((client client) (message server-privmsg))
  (let* ((contents (second (arguments message)))
         (type (cond ((match-prefix-p *eval-prefix* contents)
                      'server-privmsg-eval)
                     ((match-prefix-p *command-prefix* contents)
                      'server-privmsg-cmd))))

    (when type
      (let ((new (make-instance type :command (command message)
                                :prefix (prefix message)
                                :arguments (arguments message)
                                :tell-intro (tell-intro message))))
        (cond ((tell-intro new)
               (queue-add (input-queue client) new))
              ((< (queue-length (input-queue client))
                  *max-input-queue-length*)
               (queue-add (input-queue client) new)))))))

(defvar *send-queue-interval* .5)
(defvar *input-queue-interval* .1)

(defun start-queue-handlers (client)
  (with-slots (input-queue-thread send-queue-thread) client
    (when (and (bt:threadp input-queue-thread)
               (bt:thread-alive-p input-queue-thread))
      (bt:destroy-thread input-queue-thread))
    (when (and (bt:threadp send-queue-thread)
               (bt:thread-alive-p send-queue-thread))
      (bt:destroy-thread send-queue-thread))

    (setf send-queue-thread
          (with-thread ("send queue handler")
            (loop :for msg := (queue-pop (send-queue client))
                  :while (connectedp client)
                  :do (sleep *send-queue-interval*)
                  :if (typep msg 'client-privmsg)
                  :do (send client msg)))

          input-queue-thread
          (with-thread ("input queue handler")
            (loop :for msg := (queue-pop (input-queue client))
                  :while (connectedp client)
                  :do (sleep *input-queue-interval*)
                  :if (typep msg 'server-privmsg)
                  :do (with-thread ("input message handler")
                        (handle-input-message client msg)))))))

(defun connection-established (client)
  (start-maintainer)
  (start-queue-handlers client)
  (ignore-errors
    (loop :for ch :in (auto-join client)
          :do (sleep 1)
          (cond ((stringp ch)
                 (irc-join client ch))
                ((and (consp ch)
                      (stringp (car ch))
                      (stringp (cdr ch)))
                 (irc-join client (car ch) (cdr ch)))))))

(defmethod trivial-irc:handle ((command (eql :privmsg))
                               (client client) prefix arguments)
  (let ((target (first arguments))
        (contents (second arguments)))

    (when (and *enabled*
               (member target (listen-targets client) :test #'string-equal)
               (not (action-to-string contents)))
      (handle-input-message
       client (make-instance 'server-privmsg
                             :command (symbol-name command)
                             :prefix prefix :arguments arguments)))))

(defmethod trivial-irc:handle ((command (eql :err_nicknameinuse))
                               (client client) prefix arguments)
  (irc-nick client (format nil "~A~A" (trivial-irc:nickname client)
                           (incf (nick-uniq-count client))))
  (call-next-method))

(defmethod trivial-irc:handle ((command (eql :err_nickcollision))
                               (client client) prefix arguments)
  (irc-nick client (format nil "~A~A" (trivial-irc:nickname client)
                           (incf (nick-uniq-count client))))
  (call-next-method))

(defmethod trivial-irc:handle ((command (eql :rpl_endofmotd))
                               (client client) prefix arguments)
  (with-thread ("connection established")
    (sleep 3)
    (handler-case (connection-established client)
      (t (c)
        (send :terminal (princ-to-string (type-of c)))
        (send :terminal (princ-to-string c)))))
  (call-next-method))

(defvar *ignored-server-messages*
  '("JOIN" "PART" "QUIT"))

(defmethod trivial-irc:handle ((command symbol)
                               (client client) prefix arguments)
  (unless (member (symbol-name command) *ignored-server-messages*
                  :test #'string-equal)
    (send :terminal (make-instance 'server-message
                                   :command (symbol-name command)
                                   :prefix prefix :arguments arguments))))
