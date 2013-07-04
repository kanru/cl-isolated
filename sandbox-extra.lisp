;;;; Eval-bot --- An IRC bot for evaluating Common Lisp expressions

;; Copyright (C) 2013 Teemu Likonen <tlikonen@iki.fi>
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

(cl:defpackage #:sandbox-extra)
(cl:in-package #:sandbox-extra)

(cl:declaim (cl:optimize (cl:safety 3)))

(cl:defmacro export-and-lock (cl:&body symbols)
  `(cl:progn ,@(cl:loop :for s :in symbols
                        :append
                        `((cl:setf (cl:get ',s :sandbox-locked) cl:t)
                          (cl:export (cl:list ',s))))))

(export-and-lock
  help tell)

(cl:define-symbol-macro help
    (cl:signal 'common:extra-command :command "help" :arguments cl:nil))

(cl:defmacro tell (nick sexp cl:&rest ignored)
  (cl:declare (cl:ignore ignored))
  `(cl:let* ((form (cl:let ((cl:*print-case* :downcase))
                     (sandbox-cl:prin1-to-string ',sexp)))
             (values (cl:with-output-to-string (stream)
                       (sandbox-impl:repl form stream))))
     (cl:signal 'common:extra-command :command "tell"
                :arguments (cl:list ',nick form values))))
