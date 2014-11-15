;;;; Sandbox --- A restricted environment for evaluating Common Lisp
;;;; expressions

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
  (:export #:*sandbox* #:*sandbox-homedir-pathname*
           #:with-sandbox-env #:translate-form
           #:sandbox-error #:disabled-feature))

(in-package #:sandbox-impl)

(declaim (optimize (safety 3)))

(defvar *sandbox* "SANDBOX/LOCAL")
(defvar *sandbox-homedir-pathname*
  (make-pathname :directory '(:absolute "home" "sandbox")
                 :name nil :type nil))
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
                   (*default-pathname-defaults* *sandbox-homedir-pathname*))
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
