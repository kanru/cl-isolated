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

(defpackage #:common
  (:use #:cl)
  (:export #:with-thread #:update-sandbox-usage #:delete-unused-packages
           #:delete-all-packages #:list-user-sandbox-packages
           #:user-to-sandbox-name
           #:queue #:queue-add #:queue-pop #:queue-clear
           #:queue-length #:extra-command #:command #:arguments))

(in-package #:common)


(define-condition extra-command ()
  ((command :reader command :initarg :command)
   (arguments :reader arguments :initarg :arguments)))

;;; Threads

(defmacro with-thread ((name &key timeout) &body body)
  (let ((main (gensym "MAIN-THREAD"))
        (time (gensym "TIMEOUT"))
        (main-body (loop :for form :in body
                         :until (eql form :timeout)
                         :collect form))
        (timeout-body (rest (member :timeout body))))
    `(let ((,time ,timeout))
       (assert (or (not ,time) (and (realp ,time) (not (minusp ,time))))
               nil "TIMEOUT must be a non-negative real number or NIL.")
       (let ((,main (bt:make-thread (lambda () ,@main-body) :name ,name)))
         (when ,time
           (bt:make-thread (lambda ()
                             (sleep ,time)
                             (when (bt:thread-alive-p ,main)
                               (bt:destroy-thread ,main)
                               ,@timeout-body))
                           :name (format nil "~A timeout"
                                         (bt:thread-name ,main))))
         ,main))))

;;; Maintenance

(defclass sandbox-package ()
  ((lock :reader lock :initform (bt:make-lock "sandbox-package"))
   (last-use :accessor last-use)))

(defvar *sandbox-usage* (make-hash-table :test #'equal))

(defun update-sandbox-usage (sandbox-name)
  (let ((package (gethash sandbox-name *sandbox-usage*)))
    (unless (typep package 'sandbox-package)
      (setf package (make-instance 'sandbox-package)
            (gethash sandbox-name *sandbox-usage*) package))
    (bt:with-lock-held ((lock (gethash sandbox-name *sandbox-usage*)))
      (setf (last-use package) (get-universal-time)))))

(defvar *max-sandbox-age* (* 15 60))

(defun delete-unused-packages ()
  (let ((current-time (get-universal-time)))
    (maphash (lambda (package-name package)
               (bt:with-lock-held ((lock package))
                 (when (or (not (find-package package-name))
                           (> (- current-time (last-use package))
                              *max-sandbox-age*))
                   (remhash package-name *sandbox-usage*)
                   (delete-package package-name))))
             *sandbox-usage*)))

(defun delete-all-packages ()
  (maphash (lambda (package-name package)
             (bt:with-lock-held ((lock package))
               (remhash package-name *sandbox-usage*)
               (delete-package package-name)))
           *sandbox-usage*))

(defvar *sandbox-package-prefix* "SANDBOX/")

(defun list-user-sandbox-packages ()
  (remove-if-not (lambda (item)
                   (string= *sandbox-package-prefix*
                            (subseq item 0
                                    (min (length item)
                                         (length *sandbox-package-prefix*)))))
                 (list-all-packages)
                 :key #'package-name))

(defun user-to-sandbox-name (user)
  (let ((excl (position #\! user)))
    (string-upcase (concatenate 'string
                                *sandbox-package-prefix*
                                (subseq user (or excl 0))))))

;;; Queues

(defclass queue ()
  ((queue-first :accessor queue-first :initform nil)
   (queue-last :accessor queue-last :initform nil)
   (queue-length :accessor queue-length :initform 0)
   (lock :reader lock :initform (bt:make-lock "queue"))))

(defun queue-add (queue item)
  (bt:with-lock-held ((lock queue))
    (with-slots (queue-first queue-last queue-length) queue
      (let ((new-cons (cons item nil)))
        (if (and queue-first queue-last)
            (setf (cdr queue-last) new-cons)
            (setf queue-first new-cons))
        (setf queue-last new-cons)
        (incf queue-length)
        item))))

(defun queue-pop (queue)
  (bt:with-lock-held ((lock queue))
    (with-slots (queue-first queue-last queue-length) queue
      (if queue-first
          (let ((first (pop queue-first)))
            (decf queue-length)
            (unless queue-first
              (setf queue-last nil))
            (values first t))
          (values nil nil)))))

(defun queue-clear (queue)
  (bt:with-lock-held ((lock queue))
    (setf (queue-first queue) nil
          (queue-last queue) nil
          (queue-length queue) 0)))
