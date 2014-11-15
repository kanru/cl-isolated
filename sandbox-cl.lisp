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

(cl:defpackage #:sandbox-cl (:use))
(cl:in-package #:sandbox-cl)

(cl:declaim (cl:optimize (cl:safety 3)))

;;; Helpers for defining, importing and exporting

(cl:defmacro import-export-symbols (cl:&body symbols)
  `(cl:loop :for symbol :in ',symbols
            :do
            (cl:shadowing-import (cl:list symbol))
            (cl:export (cl:list symbol))))

(cl:defmacro sdefparameter (name initial-value)
  `(cl:progn
     (cl:export (cl:list ',name))
     (cl:defparameter ,name ,initial-value)))

(cl:defmacro sdefun (name lambda-list cl:&body body)
  `(cl:progn
     (cl:setf (cl:get ',name :sandbox-locked) cl:t)
     (cl:export (cl:list ',name))
     (cl:defun ,name ,lambda-list ,@body)))

(cl:defmacro sdefmacro (name lambda-list cl:&body body)
  `(cl:progn
     (cl:setf (cl:get ',name :sandbox-locked) cl:t)
     (cl:export (cl:list ',name))
     (cl:defmacro ,name ,lambda-list ,@body)))

(cl:defmacro disabled-features (cl:&body symbols)
  `(cl:loop :for symbol :in ',symbols
            :do
            (cl:setf (cl:get symbol :sandbox-locked) cl:t)
            (cl:export (cl:list symbol))
            (cl:let ((name (cl:symbol-name symbol)))

              (cl:setf (cl:macro-function symbol)
                       (cl:lambda (cl:&rest ignored)
                         (cl:declare (cl:ignore ignored))
                         `(cl:error 'sandbox-impl:disabled-feature
                                    :name ,name))

                       (cl:symbol-function symbol)
                       (cl:lambda (cl:&rest ignored)
                         (cl:declare (cl:ignore ignored))
                         (cl:error 'sandbox-impl:disabled-feature
                                   :name name)))

              (cl:eval `(cl:defsetf ,symbol (cl:&rest ignored) (values-ignored)
                          (cl:declare (cl:ignore ignored values-ignored))
                          `(cl:error 'sandbox-impl:disabled-feature
                                     :name ,,name))))))

;;; General

(import-export-symbols
  cl:&allow-other-keys cl:&aux cl:&body cl:&environment cl:&key cl:&optional
  cl:&rest cl:&whole)

;;; The Evaluation and Compilation Dictionary

(import-export-symbols
  cl:lambda cl:quote cl:special-operator-p cl:constantp)

(disabled-features
  compile eval-when load-time-value compiler-macro-function
  define-compiler-macro macro-function macroexpand
  macroexpand-1 define-symbol-macro symbol-macrolet proclaim declaim
  the declare locally)

(sdefun eval (form)
  (cl:values-list (sandbox-impl:translate-form
                   (cl:multiple-value-list
                    (cl:eval (sandbox-impl:translate-form form))))))

(sdefmacro defmacro (name lambda-list cl:&body body)
  (cl:if (cl:get name :sandbox-locked)
         (cl:error 'cl:package-error :package cl:nil)
         `(cl:defmacro ,name ,lambda-list ,@body)))

;;; The Types and Classes Dictionary

(import-export-symbols
  cl:nil cl:boolean cl:function cl:compiled-function cl:generic-function
  cl:standard-generic-function cl:class cl:built-in-class cl:structure-class
  cl:standard-class cl:method cl:standard-method cl:structure-object
  cl:standard-object cl:method-combination cl:t cl:satisfies
  ;; cl:member  Also a function. Imported later.
  cl:not cl:and cl:or cl:values cl:eql cl:coerce cl:subtypep cl:type-of
  cl:typep cl:type-error-datum cl:type-error-expected-type)

(disabled-features
  deftype)

;;; The Data and Control Flow Dictionary

(import-export-symbols
  cl:apply cl:fboundp cl:flet cl:labels cl:macrolet cl:funcall cl:function
  cl:functionp cl:compiled-function-p cl:call-arguments-limit
  cl:lambda-list-keywords cl:lambda-parameters-limit cl:defparameter cl:defvar
  cl:destructuring-bind cl:let cl:let* cl:progv cl:setq cl:psetq cl:block
  cl:go cl:return-from cl:return cl:tagbody
  cl:nil cl:not cl:t cl:eq cl:eql cl:equal cl:equalp
  cl:identity cl:complement cl:constantly cl:every cl:some cl:notevery
  cl:notany cl:and cl:cond cl:if cl:or cl:when cl:unless cl:case cl:ccase
  cl:ecase cl:typecase cl:ctypecase cl:etypecase cl:multiple-value-bind
  cl:multiple-value-list cl:multiple-value-prog1 cl:multiple-value-setq
  cl:values cl:values-list cl:multiple-values-limit cl:nth-value cl:prog
  cl:prog* cl:prog1 cl:prog2 cl:progn cl:setf cl:psetf cl:shiftf cl:rotatef)

(disabled-features
  fdefinition fmakunbound function-lambda-expression
  defconstant define-modify-macro defsetf define-setf-expander
  get-setf-expansion unwind-protect catch throw)

(sdefmacro defun (name lambda-list cl:&body body)
  (cl:if (cl:or (cl:and (cl:symbolp name)
                        (cl:get name :sandbox-locked))
                (cl:and (cl:consp name)
                        (cl:eql (cl:first name) 'cl:setf)
                        (cl:get (cl:second name) :sandbox-locked)))
         (cl:error 'cl:package-error :package cl:nil)
         `(cl:defun ,name ,lambda-list ,@body)))

;;; The Iteration Dictionary

(import-export-symbols
  cl:do cl:do* cl:dotimes cl:dolist)

(disabled-features
  loop)

;;; The Objects Dictionary

(import-export-symbols
  cl:class-of)

(disabled-features
  function-keywords ensure-generic-function allocate-instance
  reinitialize-instance shared-initialize
  update-instance-for-different-class update-instance-for-redefined-class
  change-class slot-boundp slot-exists-p slot-makunbound slot-missing
  slot-unbound slot-value method-qualifiers no-applicable-method
  remove-method make-instance make-instances-obsolete make-load-form
  make-load-form-saving-slots with-accessors with-slots defclass
  defgeneric defmethod find-class next-method-p call-method
  make-method call-next-method compute-applicable-methods
  define-method-combination find-method add-method initialize-instance
  class-name unbound-slot-instance)

;;; The Structures Dictionary

(disabled-features
  defstruct copy-structure)

;;; The Conditions Dictionary

(import-export-symbols
  cl:restart)

(disabled-features
  cell-error-name assert error cerror check-type
  invalid-method-error method-combination-error signal
  simple-condition-format-control simple-condition-format-arguments
  warn invoke-debugger break handler-bind handler-case
  ignore-errors define-condition make-condition compute-restarts
  find-restart invoke-restart invoke-restart-interactively
  restart-bind restart-case restart-name with-condition-restarts
  with-simple-restart abort continue muffle-warning store-value
  use-value)

;;; The Symbols Dictionary

(import-export-symbols
  cl:symbol cl:keyword cl:symbolp cl:keywordp cl:symbol-name cl:boundp)

(disabled-features
  make-symbol copy-symbol gensym gentemp symbol-function
  symbol-package symbol-plist symbol-value get remprop makunbound
  set)

;;; The Packages Dictionary

(disabled-features
  export find-symbol find-package find-all-symbols import
  list-all-packages rename-package shadow shadowing-import
  delete-package make-package with-package-iterator unexport unintern
  in-package unuse-package use-package defpackage do-symbols
  do-external-symbols do-all-symbols intern package-name
  package-nicknames package-shadowing-symbols package-use-list
  package-used-by-list packagep package-error-package)

;;; The Numbers Dictionary

(import-export-symbols
  cl:number cl:complex cl:real cl:float cl:short-float cl:single-float
  cl:double-float cl:long-float cl:rational cl:ratio cl:integer cl:signed-byte
  cl:unsigned-byte cl:mod cl:bit cl:fixnum cl:bignum cl:= cl:/= cl:< cl:>
  cl:<= cl:>= cl:max cl:min cl:minusp cl:plusp cl:zerop cl:floor cl:ffloor
  cl:ceiling cl:fceiling cl:truncate cl:ftruncate cl:round cl:fround cl:sin
  cl:cos cl:tan cl:asin cl:acos cl:atan cl:pi cl:sinh cl:cosh cl:tanh cl:asinh
  cl:acosh cl:atanh cl:1+ cl:1- cl:abs cl:evenp cl:oddp cl:exp cl:expt cl:gcd
  cl:incf cl:decf cl:lcm cl:log cl:mod cl:rem cl:signum cl:sqrt cl:isqrt
  cl:random-state cl:make-random-state cl:random cl:random-state-p
  cl:*random-state* cl:numberp cl:cis cl:complex cl:complexp cl:conjugate
  cl:phase cl:realpart cl:imagpart cl:upgraded-complex-part-type cl:realp
  cl:numerator cl:denominator cl:rational cl:rationalize cl:rationalp cl:ash
  cl:integer-length cl:integerp cl:parse-integer cl:boole cl:boole-1
  cl:boole-2 cl:boole-and cl:boole-andc1 cl:boole-andc2 cl:boole-c1 cl:boole-c2
  cl:boole-clr cl:boole-eqv cl:boole-ior cl:boole-nand cl:boole-nor
  cl:boole-orc1 cl:boole-orc2 cl:boole-set cl:boole-xor cl:logand cl:logandc1
  cl:logandc2 cl:logeqv cl:logior cl:lognand cl:lognor cl:lognot cl:logorc1
  cl:logorc2 cl:logxor cl:logbitp cl:logcount cl:logtest cl:byte cl:byte-size
  cl:byte-position cl:deposit-field cl:dpb cl:ldb cl:ldb-test cl:mask-field
  cl:most-positive-fixnum cl:most-negative-fixnum cl:decode-float
  cl:scale-float cl:float-radix cl:float-sign cl:float-digits
  cl:float-precision cl:integer-decode-float cl:float cl:floatp
  cl:most-positive-short-float cl:least-positive-short-float
  cl:least-positive-normalized-short-float cl:most-positive-double-float
  cl:least-positive-double-float cl:least-positive-normalized-double-float
  cl:most-positive-long-float cl:least-positive-long-float
  cl:least-positive-normalized-long-float cl:most-positive-single-float
  cl:least-positive-single-float cl:least-positive-normalized-single-float
  cl:most-negative-short-float cl:least-negative-short-float
  cl:least-negative-normalized-short-float cl:most-negative-single-float
  cl:least-negative-single-float cl:least-negative-normalized-single-float
  cl:most-negative-double-float cl:least-negative-double-float
  cl:least-negative-normalized-double-float cl:most-negative-long-float
  cl:least-negative-long-float cl:least-negative-normalized-long-float
  cl:short-float-epsilon cl:short-float-negative-epsilon
  cl:single-float-epsilon cl:single-float-negative-epsilon
  cl:double-float-epsilon cl:double-float-negative-epsilon
  cl:long-float-epsilon cl:long-float-negative-epsilon)

(disabled-features
  arithmetic-error-operands arithmetic-error-operation)

;;; The Characters Dictionary

(import-export-symbols
  cl:character cl:base-char cl:standard-char cl:extended-char cl:char=
  cl:char/= cl:char< cl:char> cl:char<= cl:char>= cl:char-equal
  cl:char-not-equal cl:char-lessp cl:char-greaterp cl:char-not-greaterp
  cl:char-not-lessp cl:characterp cl:alpha-char-p cl:alphanumericp
  cl:digit-char cl:digit-char-p cl:graphic-char-p cl:standard-char-p
  cl:char-upcase cl:char-downcase cl:upper-case-p cl:lower-case-p
  cl:both-case-p cl:char-code cl:char-int cl:code-char cl:char-code-limit
  cl:char-name cl:name-char)

;;; The Conses Dictionary

(import-export-symbols
  cl:list cl:null cl:cons cl:atom cl:cons cl:consp cl:atom cl:rplaca cl:rplacd
  cl:car cl:cdr cl:caar cl:cadr cl:cdar cl:cddr cl:caaar cl:caadr cl:cadar
  cl:caddr cl:cdaar cl:cdadr cl:cddar cl:cdddr cl:caaaar cl:caaadr cl:caadar
  cl:caaddr cl:cadaar cl:cadadr cl:caddar cl:cadddr cl:cdaaar cl:cdaadr
  cl:cdadar cl:cdaddr cl:cddaar cl:cddadr cl:cdddar cl:cddddr cl:copy-tree
  cl:sublis cl:nsublis cl:subst cl:subst-if cl:subst-if-not cl:nsubst
  cl:nsubst-if cl:nsubst-if-not cl:tree-equal cl:copy-list cl:list cl:list*
  cl:list-length cl:listp cl:make-list cl:push cl:pop cl:first cl:second
  cl:third cl:fourth cl:fifth cl:sixth cl:seventh cl:eighth cl:ninth cl:tenth
  cl:nth cl:endp cl:null cl:nconc cl:append cl:revappend cl:nreconc cl:butlast
  cl:nbutlast cl:last cl:ldiff cl:tailp cl:nthcdr cl:rest cl:member
  cl:member-if cl:member-if-not cl:mapc cl:mapcar cl:mapcan cl:mapcan cl:mapl
  cl:maplist cl:mapcon cl:acons cl:assoc cl:assoc-if cl:assoc-if-not
  cl:copy-alist cl:pairlis cl:rassoc cl:rassoc-if cl:rassoc-if-not
  cl:get-properties cl:getf cl:remf cl:intersection cl:nintersection cl:adjoin
  cl:pushnew cl:set-difference cl:nset-difference cl:set-exclusive-or
  cl:nset-exclusive-or cl:subsetp cl:union cl:nunion)

;;; The Arrays Dictionary

(import-export-symbols
  cl:array cl:simple-array cl:vector cl:simple-vector cl:bit-vector
  cl:simple-bit-vector cl:make-array cl:adjust-array cl:adjustable-array-p
  cl:aref cl:array-dimension cl:array-dimensions cl:array-element-type
  cl:array-has-fill-pointer-p cl:array-displacement cl:array-in-bounds-p
  cl:array-rank cl:array-row-major-index cl:array-total-size cl:arrayp
  cl:fill-pointer cl:row-major-aref cl:upgraded-array-element-type
  cl:array-dimension-limit cl:array-rank-limit cl:array-total-size-limit
  cl:simple-vector-p cl:svref cl:vector-pop cl:vector-push
  cl:vector-push-extend cl:vectorp cl:bit cl:sbit cl:bit-and cl:bit-andc1
  cl:bit-andc2 cl:bit-eqv cl:bit-ior cl:bit-nand cl:bit-nor cl:bit-not
  cl:bit-orc1 cl:bit-orc2 cl:bit-xor cl:bit-vector-p cl:simple-bit-vector-p)

;;; The Strings Dictionary

(import-export-symbols
  cl:string cl:base-string cl:simple-base-string cl:simple-string-p cl:char
  cl:schar cl:string-upcase cl:string-downcase cl:string-capitalize
  cl:nstring-upcase cl:nstring-downcase cl:nstring-capitalize cl:string-trim
  cl:string-left-trim cl:string-right-trim cl:string= cl:string/= cl:string<
  cl:string> cl:string<= cl:string>= cl:string-equal cl:string-not-equal
  cl:string-lessp cl:string-greaterp cl:string-not-greaterp
  cl:string-not-lessp cl:stringp cl:make-string)

;;; The Sequence Dictionary

(import-export-symbols
  cl:sequence cl:copy-seq cl:elt cl:fill cl:make-sequence cl:subseq cl:map
  cl:map-into cl:reduce cl:count cl:count-if cl:count-if-not cl:length
  cl:reverse cl:nreverse cl:sort cl:stable-sort cl:find cl:find-if
  cl:find-if-not cl:position cl:position-if cl:position-if-not cl:search
  cl:mismatch cl:replace cl:substitute cl:substitute-if cl:substitute-if-not
  cl:nsubstitute cl:nsubstitute-if cl:nsubstitute-if-not cl:concatenate
  cl:merge cl:remove cl:remove-if cl:remove-if-not cl:delete cl:delete-if
  cl:delete-if-not cl:remove-duplicates cl:delete-duplicates)

;;; The Hash Tables Dictionary

(import-export-symbols
  cl:hash-table cl:make-hash-table cl:hash-table-p cl:hash-table-count
  cl:hash-table-rehash-size cl:hash-table-rehash-threshold cl:hash-table-size
  cl:hash-table-test cl:gethash cl:remhash cl:maphash
  cl:with-hash-table-iterator cl:clrhash cl:sxhash)

;;; The Filenames Dictionary

(import-export-symbols
  cl:pathname cl:make-pathname cl:pathnamep cl:pathname-directory
  cl:pathname-name cl:pathname-type cl:pathname-version cl:namestring
  cl:file-namestring cl:directory-namestring cl:parse-namestring
  cl:wild-pathname-p cl:pathname-match-p cl:translate-pathname
  cl:merge-pathnames)

(disabled-features
  pathname-host pathname-device load-logical-pathname-translations
  logical-pathname-translations logical-pathname host-namestring
  enough-namestring translate-logical-pathname)

;;; The Files Dictionary

(disabled-features
  directory probe-file ensure-directories-exist truename file-author
  file-write-date rename-file delete-file file-error-pathname)

;;; The Stream Dictionary

(import-export-symbols
  cl:stream cl:broadcast-stream cl:concatenated-stream cl:echo-stream
  cl:file-stream cl:string-stream cl:synonym-stream cl:two-way-stream
  cl:input-stream-p cl:output-stream-p cl:interactive-stream-p cl:open-stream-p
  cl:stream-element-type cl:streamp cl:peek-char cl:read-char
  cl:read-char-no-hang cl:terpri cl:fresh-line cl:unread-char cl:write-char
  cl:read-line cl:write-string cl:write-line cl:read-sequence cl:write-sequence
  cl:file-position cl:close cl:listen cl:clear-input cl:finish-output
  cl:force-output cl:clear-output cl:with-input-from-string
  cl:with-output-to-string)

(disabled-features
  read-byte write-byte file-length file-string-length open
  stream-external-format with-open-file with-open-stream y-or-n-p
  yes-or-no-p make-synonym-stream synonym-stream-symbol
  broadcast-stream-streams make-broadcast-stream make-two-way-stream
  two-way-stream-input-stream two-way-stream-output-stream
  make-echo-stream concatenated-stream-streams make-concatenated-stream
  make-string-input-stream make-string-output-stream stream-error-stream)

;;; The Printer Dictionary

(import-export-symbols
  cl:write cl:prin1 cl:print cl:pprint cl:princ cl:write-to-string
  cl:prin1-to-string cl:princ-to-string)

(disabled-features
  copy-pprint-dispatch formatter pprint-dispatch
  pprint-exit-if-list-exhausted pprint-fill pprint-linear
  pprint-tabular pprint-indent pprint-logical-block pprint-newline
  pprint-pop pprint-tab print-object print-unreadable-object
  set-ppring-dispatch print-not-readable-object format)

;;; The Reader Dictionary

(disabled-features
  copy-readtable make-dispatch-macro-character read-preserving-whitespace
  read-delimited-list readtable-case readtablep
  set-dispatch-macro-character get-dispatch-macro-character
  set-macro-character get-macro-character set-syntax-from-char
  with-standard-io-syntax)

(sdefun read (cl:&rest args)
  (cl:values-list (sandbox-impl:translate-form
                   (cl:multiple-value-list (cl:apply #'cl:read args)))))

(sdefun read-from-string (cl:&rest args)
  (cl:values-list (sandbox-impl:translate-form
                   (cl:multiple-value-list
                    (cl:apply #'cl:read-from-string args)))))

;;; The System Construction Dictionary

(disabled-features
  compile-file compile-file-pathname load with-compilation-unit
  provide require)

;;; The Environment Dictionary

(import-export-symbols
  cl:decode-universal-time cl:encode-universal-time cl:get-universal-time
  cl:get-decoded-time cl:sleep cl:lisp-implementation-type
  cl:lisp-implementation-version)

(disabled-features
  apropos apropos-list describe describe-object trace untrace
  step time get-internal-real-time get-internal-run-time disassemble
  documentation room ed inspect dribble short-site-name
  long-site-name machine-instance machine-type machine-version
  software-type software-version)

(sdefun user-homedir-pathname (cl:&optional ignored)
  (cl:declare (cl:ignore ignored))
  sandbox-impl:*sandbox-homedir-pathname*)
