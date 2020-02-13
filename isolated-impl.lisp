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

(defpackage #:isolated-impl
  (:use #:cl)
  (:import-from #:alexandria #:with-gensyms #:circular-tree-p)
  (:export #:*env* #:*isolated-homedir-pathname*
           #:with-isolated-env #:translate-form
           #:isolated-error #:disabled-feature
	   #:isolated-allowed-symbols #:set-allowed-symbol
	   #:allow-symbols #:allow-package-symbols))

(in-package #:isolated-impl)

(declaim (optimize (safety 3)))

(defvar *env* "ISOLATED/LOCAL")
(defvar *isolated-homedir-pathname*
  (make-pathname :directory '(:absolute "home" "isolated")
                 :name nil :type nil))
(defvar *max-elements* 500)

(define-condition isolated-error (error) nil
  (:report "Isolated error."))

(define-condition unsupported-type (isolated-error)
  ((type :initarg :type :reader unsupported-type))
  (:report (lambda (c s)
             (format s "Type ~A is not supported." (unsupported-type c)))))

(define-condition disabled-feature (isolated-error)
  ((name :initarg :name :reader disabled-feature-name))
  (:report (lambda (c s)
             (format s "The feature ~A is disabled."
                     (disabled-feature-name c)))))

(define-condition circular-list (isolated-error) nil
  (:report "Circular list was detected."))

(define-condition dimension-error (isolated-error) nil
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Array or list dimensions too large (max ~D elements)."
                     *max-elements*))))

(defmacro with-isolated-env (&body body)
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
                   (*package* (find-package *env*))
                   (*features* nil)
                   (*print-length* 50)
                   (*print-level* 10)
                   (*print-readably* nil)
                   (*read-eval* nil)
                   (*default-pathname-defaults* *isolated-homedir-pathname*))
               ,@body)))))))

(defvar *allowed-isolated-symbols* nil)
(defvar *allowed-isolated-functions* nil)

(defun isolated-allowed-symbols ()
  (loop :for symbol :being :the :symbol :in (find-package 'isolated-cl)
     :when (not (get symbol :isolated-locked))
     :do
       (if (fboundp symbol)
	   (pushnew symbol *allowed-isolated-functions*)
	   (pushnew symbol *allowed-isolated-symbols*))))

(defparameter *allowed-packages-symbols* nil)
(defparameter *allowed-packages-functions* nil)

(defun set-allowed-symbol (symbol)
  
  (if (fboundp symbol)
      (pushnew symbol *allowed-packages-functions*)
      (pushnew symbol *allowed-packages-symbols*)))

(defun get-package-symbols (packages &optional excluded-symbols)
  (let (symbols)
    (dolist (package packages)
      (do-external-symbols (s (find-package package))
	(unless (find s excluded-symbols :test 'equalp)
	  (pushnew s symbols))))
    symbols))

(defun allow-symbols (symbols)
  (dolist (symbol symbols)
      (set-allowed-symbol symbol)))

(defun allow-package-symbols (packages &optional excluded-symbols)
  (dolist (package packages)
    (do-external-symbols (symbol (find-package package))
      (unless (find symbol excluded-symbols :test 'equalp)
	(set-allowed-symbol symbol)))))

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
                 (symbol (if (fboundp form)
			     (or (find form *allowed-isolated-functions*)
				 (find form *allowed-packages-functions*)
				 (error 'undefined-function :name form))
			     (if (or (find form *allowed-isolated-symbols*)
				     (find form *allowed-packages-symbols*))
				 form
				 (intern (symbol-name form) *env*))))
                 (t (error 'unsupported-type :type (type-of form))))))
      (translate form))))
