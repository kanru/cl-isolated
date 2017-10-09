;;;; Sandbox --- A restricted environment for evaluating Common Lisp
;;;; expressions

;; Copyright (C) 2012-2013 Teemu Likonen <tlikonen@iki.fi>
;; Copyright (C) 2014 Kan-Ru Chen <kanru@kanru.info>
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

(defpackage #:sandbox
  (:use #:cl #:sandbox-impl)
  (:export #:*sandbox* #:*sandbox-homedir-pathname*
           #:read-eval-print #:reset
	   #:*msg-value-prefix*
	   #:*msg-value-formatter*
	   #:*msg-error-prefix*
	   #:*msg-no-value-message*))

(in-package #:sandbox)

(declaim (optimize (safety 3)))

(defparameter *msg-value-prefix* "=> ")
(defparameter *msg-error-prefix* ";; ")
(defparameter *msg-value-formatter* "~{~S~^, ~}")
(defparameter *msg-no-value-message* "No values.")

(define-condition all-read () ())

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
      (msgv stream *msg-value-formatter* values)
      (msge stream *msg-no-value-message*))
  nil)

(defun reset ()
  (ignore-errors
    (delete-package *sandbox*))
  (make-package *sandbox* :use '(#:sandbox-cl))
  (loop :for name :in '("+" "++" "+++" "*" "**" "***" "/" "//" "///" "-")
        :do (eval `(defparameter ,(intern name *sandbox*) nil)))
  (loop :for fn :in '(+ - * /)
        :for symbol := (intern (symbol-name fn) *sandbox*)
        :do (setf (get symbol :sandbox-locked) t)
        (eval `(defun ,symbol (&rest args)
                 (apply ',fn args))))
  *sandbox*)

(defun read-eval-print (string &optional (stream *standard-output*))
  (unless (or (find-package *sandbox*) (reset))
    (msge stream "SANDBOX-PACKAGE-ERROR: Sandbox package not found.")
    (return-from read-eval-print nil))

  (with-sandbox-env
    (with-input-from-string (s string)
      (flet ((sread (sexp)
               (translate-form
		sexp))
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
		(setf values (loop for sexp = (read s nil)
				while sexp
				collect
				  (eval
				   (prog1
				       (setf form (sread sexp))
				     (ssetq "-" form))) ))
		(sandbox-print values stream))
	    
            (all-read ()
              (sandbox-print values stream))
	    
            (undefined-function (c)
              (msge stream "~A: The function ~A is undefined."
                    (type-of c) (cell-error-name c)))

            (end-of-file ()
              (msge stream "SYNTAX ERROR: POSSIBLE MISSING ( or ) or \" "))

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
