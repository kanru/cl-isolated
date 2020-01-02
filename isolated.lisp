;;;; Isolated --- A isolated environment for evaluating Common Lisp
;;;; expressions

;; Copyright (C) 2014, 2020 Kan-Ru Chen <kanru@kanru.info>
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

(defpackage #:isolated
  (:use #:cl #:isolated-impl)
  (:export #:*env* #:*isolated-homedir-pathname*
           #:read-eval-print #:reset))

(in-package #:isolated)

(declaim (optimize (safety 3)))

(defvar *msg-value-prefix* "=> ")
(defvar *msg-error-prefix* ";; ")

(define-condition all-read () ())

(defun msge (stream format-string &rest params)
  (apply #'format stream (concatenate 'string "~&" *msg-error-prefix*
                                      format-string "~%")
         params))

(defun msgv (stream format-string &rest params)
  (apply #'format stream (concatenate 'string "~&" *msg-value-prefix*
                                      format-string "~%")
         params))

(defun isolated-print (values &optional (stream *standard-output*))
  (if values
      (msgv stream "~{~S~^, ~}" values)
      (msge stream "No value"))
  nil)

(defun reset ()
  (ignore-errors
    (delete-package *env*))
  (make-package *env* :use '(#:isolated-cl))
  (loop :for name :in '("+" "++" "+++" "*" "**" "***" "/" "//" "///" "-")
        :do (eval `(defparameter ,(intern name *env*) nil)))
  (loop :for fn :in '(+ - * /)
        :for symbol := (intern (symbol-name fn) *env*)
        :do (setf (get symbol :isolated-locked) t)
        (eval `(defun ,symbol (&rest args)
                 (apply ',fn args))))
  *env*)

(defun read-eval-print (string &optional (stream *standard-output*))
  (unless (or (find-package *env*) (reset))
    (msge stream "ISOLATED-PACKAGE-ERROR: Isolated package not found.")
    (return-from read-eval-print nil))

  (with-isolated-env
    (with-input-from-string (s string)

      (flet ((sread (stream)
               (translate-form (handler-case (read stream)
                                 (end-of-file ()
                                   (signal 'all-read)))))

             (ssetq (name value)
               (setf (symbol-value (find-symbol (string-upcase name) *env*))
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

            (all-read ()
              (isolated-print values stream))

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
                   (symbol-value (find-symbol string *env*))))
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
