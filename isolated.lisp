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

(isolated-allowed-symbols)

(defun reset ()
  (setf isolated-impl::*allowed-isolated-symbols* nil)
  (setf isolated-impl::*allowed-isolated-functions* nil)
  (setf isolated-impl::*allowed-packages-symbols* nil)
  (setf isolated-impl::*allowed-packages-functions* nil)

  (isolated-allowed-symbols)
   
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

(defun read-no-eval (forms &key packages exclude-symbols)
  "Returns forms and/or any messages."
  (unless (or (find-package *env*) (reset))    
    (return-from read-no-eval "ISOLATED-PACKAGE-ERROR: Isolated package not found."))

  (allow-package-symbols packages exclude-symbols)

  (let ((validated-forms)
	(msg))

    (labels ((sexp-read (sexps)
	       (let (values)
		 (if (listp (car sexps))		     
		     (dolist (sexp sexps)		 
		       (push (translate-form sexp) values))
		     (push (translate-form sexps) values))
		 (reverse values)))

	     (sread (string)
	       (let (values)
		 (with-input-from-string (s string)		 
		   (loop for sexp = (read s nil)
		      while sexp
		      do
			(if (listp (car sexp))
			    (dolist (sexpx sexp)
			      (push (translate-form sexpx)
				    values))
			    (push (translate-form sexp)
				  values))))
		 (reverse values))))
      
      (setf validated-forms
	    (if (stringp forms)
		(sread forms)
		(sexp-read forms))))
    (values validated-forms msg)))

(defun read-eval (forms &key packages exclude-symbols)
  "Returns eval values and/or any messages."

  (unless (or (find-package *env*) (reset))
    (return-from read-eval (values nil "ISOLATED-PACKAGE-ERROR: Isolated package not found.")))

  (allow-package-symbols packages exclude-symbols)

  (with-isolated-env
    (let ((values)
	  (msg))
       
      (flet ((sexp-read (sexps)
	       (let (values)
		 (if (listp (car sexps))
		     (dolist (sexp sexps)
		       (push (multiple-value-list
			      (eval
			       (translate-form sexp)))
			     values))
		     (push (multiple-value-list
			    (eval
			     (translate-form sexps)))
			   values))   
		 (reverse values)))
	     (sread (string)
	       (let (values) 
		 (with-input-from-string (s string)		  
		   (loop for sexp = (read s nil)
		      while sexp
		      do
			(multiple-value-list
			 (if (listp (car sexp))
			     (dolist (sexpx sexp)
			       (push (multiple-value-list
				      (eval
				       (translate-form sexpx)))
				     values))
			     (push (multiple-value-list
				    (eval
				     (translate-form sexp)))
				   values)))))
		 (reverse values))))
	(setf values (if (stringp forms)
			 (sread forms)
			 (sexp-read forms))))
      (values values msg))))


(defun ssetq (name value)
  (setf (symbol-value (find-symbol (string-upcase name) *env*))
		       value))


(defun read-eval-print (forms &optional (stream *standard-output*))
  (unless (or (find-package *env*) (reset))
    (msge stream "ISOLATED-PACKAGE-ERROR: Isolated package not found.")
    (return-from read-eval-print nil))

  (with-isolated-env
    (let (form)
	
      (flet ((sexp-read (sexps)
	       (let (values)
		 (if (listp (car sexps))
		     (dolist (sexp sexps)
		       (push (multiple-value-list
			      (eval
			       (translate-form sexp)))
			     values))
		     (push (multiple-value-list
			    (eval
			     (translate-form sexps)))
			   values))   
		 (reverse values)))

	     (sread (string)
	       (let (values) 
		 (with-input-from-string (s string)		  
		   (loop for sexp = (read s nil)
		      while sexp
		      do
			(multiple-value-list
			 (if (listp (car sexp))			       
			     (dolist (sexpx sexp)
			       (setf form (translate-form sexpx))
			       (push (multiple-value-list
				      (eval
				       (prog1
					   form
					 (ssetq "-" form))))
				     values))
			     (progn
			       (setf form (translate-form sexp))
			       (push (multiple-value-list
				      (eval
				       (prog1
					   form
					 (ssetq "-" form))
				       ))
				     values))))))
		 (reverse values)))

	     (muffle (c)
	       (declare (ignore c))
	       (when (find-restart 'muffle-warning)
		 (muffle-warning))))

	(let (form values)

	  (handler-case
	      (handler-bind ((warning #'muffle))

		(setf values (if (stringp forms)
				 (sread forms)
				 (sexp-read forms)))
		(dolist (value values)
		  (isolated-print value stream)))

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
