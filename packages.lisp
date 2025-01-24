;;;; -*- Mode:Lisp; indent-tabs-mode:nil -*-
;;;; islisp-sys.lisp
;;;; Copyright (c) 2011-2025 Derek Newhall
;;;; SPDX-License-Identifier: CDDL-1.1
;;;;
;;;; This contains all the package definitions for islisp-compat
;;;; and needs to be compiled/loaded first.
(in-package #:common-lisp-user)

(defpackage :islisp-sys
  (:use :common-lisp)
  (:export ;; Class accessors
           #:cl-class-name #:cl-class
           ;; Fix ISLisp-specific things
           #:fix-lambda-list #:fix-global-name #:fix-dynamic-name
           #:dynamic-var-p #:islisp-keyword-p #:identifierp
           #:fix-slot-spec #:fix-method-parameter-profile
           #:validate-format-string #:valid-number-p
           #:validate-function-list #:validate-variable-list
           ;; set-ref support
           #:set-aref1 #:set-aref2 #:set-aref3 #:set-aref4
           #:set-aref5 #:set-aref6 #:set-aref7 #:set-aref8
           #:set-aref9 #:set-aref10 #:set-aref
           ;; Errors
           #:<violation> #:violation
           #:signal-domain-error #:signal-program-error
	   #:signal-control-error #:assert-arity #:assure-identifier
           ;; Exported utility functions
           #:exit #:host-implementation-feature #:islisp-import #:read-new-value
           ;; Backquote expansion/evaluation
           #:bq-completely-process #:bq-process
           ;; Reader
           #:islisp-read #:islisp-read-from-string #:read-form
           #:make-islisp-reader-options #:copy-islisp-reader-options
           #:*default-reader-options*
           #:*default-islisp-character-names*
           #:*default-common-lisp-character-names*
           ;; Reader options slots
           #:reader-options-eos-error-p
           #:reader-options-eos-value
           #:reader-options-neutral-alphabetic-case
           #:reader-options-package
           #:reader-options-feature-conditionals-p
           #:reader-options-features
           #:reader-options-quote-symbol
           #:reader-options-keyword-behavior
           #:reader-options-symbol-colon-behavior
           #:reader-options-allowed-character-names
           ))

(defpackage :islisp
  ;#+sbcl (:lock t)
  (:import-from :common-lisp
		#:t #:nil #:quote #:&rest #:function)
                ;#:lambda)
  (:export      ;; Symbols
                #:&rest #:standard
                ;; Quasiquote
                #:quasiquote #:unquote #:unquote-splicing
                ;; Constants
                #:t #:nil
                #:*pi* #:*most-positive-float* #:*most-negative-float*

                ;; Special forms
                #:defglobal #:defconstant #:defdynamic
                #:defmacro #:defun
                #:defclass #:defgeneric #:defmethod
                #:quote #:progn #:lambda
                #:function #:class #:dynamic
                #:block #:return-from
                #:tagbody #:go
                #:unwind-protect #:ignore-errors
                #:catch #:throw
                #:if #:cond
                #:case #:case-using
                #:and #:or
                #:let #:let* #:dynamic-let
                #:flet #:labels
                #:for #:while
                #:setq #:setf #:set-dynamic
                #:with-open-input-file
                #:with-open-output-file
                #:with-open-io-file
                #:with-standard-input
                #:with-standard-output
                #:with-error-output
                #:with-handler
                #:assure #:the

                ;; Functions
                #:cons #:car #:cdr
                #:list #:create-list
                #:append #:member #:assoc
                #:reverse #:nreverse
                #:mapc #:mapcar #:mapcan
                #:maplist #:mapl #:mapcon
                #:apply #:funcall
                #:not #:eq #:eql #:equal

                #:+ #:* #:- #:quotient #:reciprocal
                #:div #:mod #:rem
                #:= #:/= #:> #:< #:>= #:<=
                #:max #:min #:abs #:exp #:expt #:log
                #:sqrt #:isqrt #:gcd #:lcm
                #:sin #:cos #:tan #:atan #:atan2
                #:sinh #:cosh #:tanh #:atanh
                #:ceiling #:floor #:float
                #:round #:truncate

                #:null #:symbolp #:listp #:consp
                #:functionp #:generic-function-p #:characterp
                #:numberp #:integerp #:floatp
                #:vectorp #:stringp #:basic-array-p
                #:general-vector-p #:basic-vector-p
                #:general-array*-p #:basic-array*-p
                #:streamp #:open-stream-p
                #:input-stream-p #:output-stream-p

                #:char= #:char/= #:char<
                #:char> #:char<= #:char>=
                #:create-string
                #:string= #:string/= #:string<
                #:string> #:string<= #:string>=
                #:char-index #:string-index #:string-append

                #:create-array #:aref #:garef
                #:array-dimensions
                #:create-vector #:vector
                #:elt #:length #:subseq #:map-into

                #:set-car #:set-cdr #:set-property
                #:set-aref #:set-garef #:set-elt
                
                #:create-string-input-stream
                #:create-string-output-stream
                #:get-output-stream-string
                
                #:standard-input #:standard-output #:error-output
                #:open-input-file #:open-output-file #:open-io-file

                #:read-char #:preview-char #:read-line #:read
                #:stream-ready-p
                #:format-char #:format-float #:format-fresh-line
                #:format-integer #:format-object #:format-tab
                #:format
                
                #:read-byte #:write-byte
                #:close #:finish-output
                #:probe-file #:file-length
                #:file-position #:set-file-position
                
                #:get-universal-time #:get-internal-run-time
                #:get-internal-real-time
                #:internal-time-units-per-second

                #:gensym #:property #:remove-property
                #:identity #:parse-number #:convert

                #:create #:class-of
                #:instancep #:subclassp
                #:initialize-object
                
                #:report-condition
                #:signal-condition
                #:error #:cerror
                #:condition-continuable
                #:continue-condition
                
                #:arithmetic-error-operation
                #:arithmetic-error-operands
                #:domain-error-object
                #:domain-error-expected-class
                #:parse-error-string
                #:parse-error-expected-class
                #:simple-error-format-string
                #:simple-error-format-arguments
                #:stream-error-stream
                #:stream-error-stream
                #:undefined-entity-name
                #:undefined-entity-namespace

                ;; Classes
                #:<object> #:<character> #:<stream>
                #:<list> #:<cons> #:<null> #:<symbol>
                #:<number> #:<integer> #:<float>
                #:<function> #:<generic-function>
                #:<standard-generic-function>
                #:<basic-array> #:<basic-array*> #:<general-array*>
                #:<basic-vector> #:<general-vector> #:<string>
                #:<built-in-class> #:<standard-class> #:<standard-object>
                #:<serious-condition> #:<error> #:<storage-exhausted>
                #:<arithmetic-error> #:<division-by-zero>
                #:<floating-point-overflow> #:<floating-point-underflow>
                #:<program-error> #:<domain-error>
                #:<undefined-entity>
                #:<unbound-variable> #:<undefined-function>
                #:<control-error> #:<parse-error> #:<simple-error>
                #:<stream-error> #:<end-of-stream>
                
                ;; Not standard, but needs to be
                #:<sequence> #:<class>
                #:keywordp #:built-in-class-p #:standard-class-p

                ;; Need to export condition so others can use it
                #:<parse-error>
                )
  (:shadow #:defconstant #:defglobal #:defmacro #:defun
           #:defclass #:defgeneric #:defmethod
           #:progn #:lambda #:class
           #:block #:return-from
           #:tagbody #:go
           #:unwind-protect #:ignore-errors
           #:catch #:throw
           #:if #:cond
           #:case
           #:and #:or
           #:let #:let* #:dynamic-let
           #:flet #:labels
           #:setq #:setf
           #:the

           #:class-of
           #:cons #:car #:cdr
           #:list #:append #:member #:assoc
           #:reverse #:nreverse
           #:mapc #:mapcar #:mapcan
           #:maplist #:mapl #:mapcon
           #:apply #:funcall
           #:not #:eq #:eql #:equal

           #:+ #:* #:- #:mod #:rem
           #:= #:/= #:> #:< #:>= #:<=
           #:max #:min #:abs #:exp #:expt #:log
           #:sqrt #:isqrt #:gcd #:lcm
           #:sin #:cos #:tan #:atan #:atan2
           #:sinh #:cosh #:tanh #:atanh
           #:ceiling #:floor #:float
           #:round #:truncate
           
           #:null #:symbolp #:listp #:consp
           #:functionp #:characterp
           #:numberp #:integerp #:floatp
           #:vectorp #:stringp #:basic-array-p
           #:general-vector-p #:basic-vector-p
           #:general-array*-p #:basic-array*-p
           #:streamp #:input-stream-p #:output-stream-p

           #:char= #:char/= #:char<
           #:char> #:char<= #:char>=
           #:string= #:string/= #:string<
           #:string> #:string<= #:string>=

           #:aref #:array-dimensions #:vector
           #:elt #:length #:subseq #:map-into

           #:get-output-stream-string
           #:read-char #:preview-char #:read-line #:read
           #:format

           #:read-byte #:write-byte
           #:close #:finish-output
           #:probe-file #:file-length
           #:file-position

           #:get-universal-time #:get-internal-run-time
           #:get-internal-real-time
           #:internal-time-units-per-second

           #:arithmetic-error-operation
           #:arithmetic-error-operands
           #:domain-error-object
           #:domain-error-expected-class
           #:parse-error-string
           #:parse-error-expected-class
           #:simple-error-format-string
           #:simple-error-format-arguments
           #:stream-error-stream
           #:stream-error-stream
           #:undefined-entity-name
           #:undefined-entity-namespace

           #:gensym #:identity
           #:error #:cerror

           #:keywordp
           ))

(defpackage #:islisp-user
  (:use :islisp)
  ;(:import-from :common-lisp)
                ;#:macroexpand #:macroexpand-1
                ;#:fboundp #:eval #:load)
  (:import-from :islisp-sys
                #:exit #:islisp-import))
