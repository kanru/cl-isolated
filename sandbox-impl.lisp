;;;; Eval-bot --- An IRC bot for evaluating Common Lisp expressions

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

(defpackage #:sandbox-impl
  (:use #:cl)
  (:import-from #:alexandria #:with-gensyms #:circular-tree-p)
  (:export #:repl #:reset #:translate-form #:*sandbox* #:sandbox-error
           #:disabled-feature))

(in-package #:sandbox-impl)

(declaim (optimize (safety 3)))

(defvar *sandbox* "SANDBOX/LOCAL")
(defvar *msg-value-prefix* "=> ")
(defvar *msg-error-prefix* ";; ")
(defvar *max-elements* 500)

(define-condition sandbox-error (error) nil
  (:report "Sandbox error."))

(define-condition unsupported-type (sandbox-error)
  ((type :initarg :type :reader unsupported-type))
  (:report (lambda (c s)
             (format s "Type ~A is not supported." (unsupported-type c)))))

(define-condition disabled-feature (sandbox-error)
  ((name :initarg :name :reader disabled-feature-name))
  (:report (lambda (c s)
             (format s "The feature ~A is disabled."
                     (disabled-feature-name c)))))

(define-condition circular-list (sandbox-error) nil
  (:report "Circular list was detected."))

(define-condition dimension-error (sandbox-error) nil
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Array or list dimensions too large (max ~D elements)."
                     *max-elements*))))

(define-condition all-read (sandbox-error) nil)

(define-condition sandbox-package-error (sandbox-error) nil)

(defmacro with-sandbox-env (&body body)
  (with-gensyms (input output two-way)
    `(with-open-stream (,input (make-string-input-stream
                                "This is the standard input stream!"))
       (with-open-stream (,output (make-broadcast-stream))
         (with-open-stream (,two-way (make-two-way-stream ,input ,output))
           (with-standard-io-syntax
             (let ((*standard-output* ,output)
                   (*error-output* ,output)
                   (*trace-output* ,output)
                   (*standard-input* ,input)
                   (*debug-io* ,two-way)
                   (*query-io* ,two-way)
                   (*terminal-io* ,two-way)
                   (*package* (find-package *sandbox*))
                   (*features* nil)
                   (*print-length* 50)
                   (*print-level* 10)
                   (*print-readably* nil)
                   (*read-eval* nil)
                   (*default-pathname-defaults*
                    (make-pathname :directory '(:absolute "home" "sandbox")
                                   :name nil :type nil)))
               ,@body)))))))

(defvar *allowed-extra-symbols* nil)

(defun translate-form (form)
  (when (and (consp form)
             (circular-tree-p form))
    (error 'circular-list))
  (let ((cons-count 0))
    (labels ((translate (form)
               (typecase form
                 (cons (if (> (incf cons-count) *max-elements*)
                           (error 'dimension-error)
                           (cons (translate (car form))
                                 (translate (cdr form)))))
                 (number form)
                 (character form)
                 (pathname form)
                 (array (if (> (array-total-size form) *max-elements*)
                            (error 'dimension-error)
                            (let ((arr (make-array (array-dimensions form)
                                                   :element-type
                                                   (array-element-type form))))
                              (dotimes (i (array-total-size arr) arr)
                                (setf (row-major-aref arr i)
                                      (translate-form
                                       (row-major-aref form i)))))))
                 (keyword form)
                 (symbol (if (member form *allowed-extra-symbols*)
                             form
                             (intern (symbol-name form) *sandbox*)))
                 (t (error 'unsupported-type :type (type-of form))))))
      (translate form))))

(defun msge (stream format-string &rest params)
  (apply #'format stream (concatenate 'string "~&" *msg-error-prefix*
                                      format-string "~%")
         params))

(defun msgv (stream format-string &rest params)
  (apply #'format stream (concatenate 'string "~&" *msg-value-prefix*
                                      format-string "~%")
         params))

(defun sandbox-print (values &optional (stream *standard-output*))
  (if values
      (msgv stream "~{~S~^, ~}" values)
      (msge stream "No value"))
  nil)

(defun reset ()
  (ignore-errors
    (delete-package *sandbox*))
  (make-package *sandbox* :use '(#:sandbox-cl #:sandbox-extra))
  (loop :for name :in '("+" "++" "+++" "*" "**" "***" "/" "//" "///" "-")
        :do (eval `(defparameter ,(intern name *sandbox*) nil)))
  (loop :for fn :in '(+ - * /)
        :for symbol := (intern (symbol-name fn) *sandbox*)
        :do (setf (get symbol :sandbox-locked) t)
        (eval `(defun ,symbol (&rest args)
                 (apply ',fn args))))
  *sandbox*)

(defun repl (string &optional (stream *standard-output*))
  (unless (or (find-package *sandbox*) (reset))
    (msge stream "SANDBOX-PACKAGE-ERROR: Sandbox package not found.")
    (return-from repl nil))

  (with-sandbox-env
    (with-input-from-string (s string)

      (flet ((sread (stream)
               (translate-form (handler-case (read stream)
                                 (end-of-file ()
                                   (signal 'all-read)))))

             (ssetq (name value)
               (setf (symbol-value (find-symbol (string-upcase name) *sandbox*))
                     value))

             (muffle (c)
               (declare (ignore c))
               (when (find-restart 'muffle-warning)
                 (muffle-warning))))

        (let (form values)

          (handler-case
              (handler-bind ((warning #'muffle))
                (loop (setf values (multiple-value-list
                                    (eval (prog1 (setf form (sread s))
                                            (ssetq "-" form)))))))

            (common:extra-command (c)
              (signal c))

            (all-read ()
              (sandbox-print values stream))

            (undefined-function (c)
              (msge stream "~A: The function ~A is undefined."
                    (type-of c) (cell-error-name c)))

            (end-of-file (c)
              (msge stream "~A" (type-of c)))

            (reader-error ()
              (msge stream "READER-ERROR"))

            (package-error ()
              (msge stream "PACKAGE-ERROR"))

            (stream-error (c)
              (msge stream "~A" (type-of c)))

            (storage-condition ()
              (msge stream "STORAGE-CONDITION"))

            (t (c)
              (msge stream "~A: ~A" (type-of c) c)))

          (flet ((svalue (string)
                   (symbol-value (find-symbol string *sandbox*))))
            (ssetq "///" (svalue "//"))
            (ssetq "//"  (svalue "/"))
            (ssetq "/"   values)
            (ssetq "***" (svalue "**"))
            (ssetq "**"  (svalue "*"))
            (ssetq "*"   (first values))
            (ssetq "+++" (svalue "++"))
            (ssetq "++"  (svalue "+"))
            (ssetq "+"   form))))))
  nil)
