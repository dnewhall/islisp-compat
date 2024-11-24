;;;; -*- Mode:Lisp; indent-tabs-mode:nil -*-
;;;; islisp-functions.lisp
;;;; Copyright (c) 2011-2024 Derek Newhall
;;;; SPDX-License-Identifier: CDDL-1.1
;;;;
;;;; This file contains all the function definitions for ISLisp
;;;; along with the bare minimum of special forms needed to
;;;; define them:
;;;;     defun progn if and or cond
;;;;     let let* function lambda
;;;;     assure the class
;;;;
;;;; All other special forms are in islisp-specials.lisp along with
;;;; the constants and generic functions.
;;;;
;;;; The symbols T, NIL, QUOTE, &REST are imported from CL.
(cl:in-package #:islisp)

;;; These are the bare minimum of special forms to define all the functions

(cl:defmacro defun (name params &rest body)
  (cl:let ((lambda-list (islisp-sys:fix-lambda-list params)))
    ;; Don't use cl:defun so we don't get the default block around
    ;; body with the function name.
    `(progn
       (cl:setf (cl:symbol-function ',name)
              ;; Use various implementation-specific named lambda forms to
              ;; get nicer output.
              #+sbcl
              (sb-int:named-lambda ,name
                                   ,lambda-list
                                   ,@body)
              #+ccl
              (ccl:nfunction ,name (cl:lambda ,lambda-list
                                     (cl:declare (ccl::global-function-name ,name))
                                     ,@body))
              #-(or sbcl ccl)
              (cl:function (cl:lambda ,lambda-list
                             #+clisp (cl:declare (system::in-defun foo))
                             ,@body))
              )
       ',name)))

;;; Conditional and sequencing forms

(cl:defmacro progn (&rest body)
  `(cl:progn ,@body))

(cl:defmacro and (&rest forms)
  `(cl:and ,@forms))

(cl:defmacro or (&rest forms)
  `(cl:or ,@forms))

(cl:defmacro cond (&rest test-forms)
  `(cl:cond ,@test-forms))

(cl:defmacro if (test-form then-form cl:&optional else-form)
  `(cl:if ,test-form
          ,then-form
          ,else-form))

;;; Binding and namespace forms

;; Now imported from CL: to play nice with the implementation's reader.
;(cl:defmacro function (function-name)
;  `(cl:function ,function-name))

;; NOTE: Using this does not allow ((lambda (...) ...) ...) forms!
(cl:defmacro lambda (params &rest body)
  `(cl:lambda ,(islisp-sys:fix-lambda-list params)
    ,@body))

(cl:defmacro let (bindings &rest body-form)
  (if (cl:null bindings)
      `(progn ,@body-form)
      `(cl:let ,(islisp-sys:validate-variable-list bindings)
         ,@body-form)))

(cl:defmacro let* (bindings &rest body-form)
  (if (cl:null bindings)
    `(progn ,@body-form)
    `(cl:let* ,(islisp-sys:validate-variable-list bindings)
       ,@body-form)))

;;; Class access and checking

(cl:defmacro assure (class-name form)
  (if (cl:not (cl:symbolp class-name))
      (islisp-sys:violation `(assure ,class-name ,form) "Class name is ~A and not a symbol" class-name))
  (cl:cond ((cl:eq class-name '<object>)
         form)
        ;; There's no CL class equivalent to <basic-array*>
        ((cl:eq class-name '<basic-array*>)
         `(cl:typep ,form '(cl:and (cl:array cl:* cl:*)
                                   (cl:not (cl:vector cl:*)))))
        (t
         (cl:let ((value (cl:gensym)))
           `(let ((,value ,form))
              (if (instancep ,value (class ,class-name))
                  ,value
                  (islisp-sys:signal-domain-error ,value (class ,class-name))))))))

(cl:defmacro the (class-name form)
  ;; There's no CL class equivalent to <basic-array*>
  (if (cl:eq class-name '<basic-array*>)
      `(cl:the (and (cl:array cl:* cl:*)
                    (not (cl:vector cl:*)))
               ,form)
      `(cl:the ,(islisp-sys:cl-class-name class-name) ,form)))

(cl:defmacro class (class-name)
  `(cl:find-class ',(islisp-sys:cl-class-name class-name)))

;;; Symbol functions

(defun gensym ()
  (cl:gensym))

;; Not standard, but needs to be in it.
(defun keywordp (obj)
  (islisp-sys:islisp-keyword-p obj))

;;; Equality functions

(defun eq (obj1 obj2)
  (cl:eq obj1 obj2))

(defun eql (obj1 obj2)
  (cl:eql obj1 obj2))

(defun equal (obj1 obj2)
  (cl:equal obj1 obj2))

(defun not (obj)
  (cl:not obj))

;;; Type predicates

(defun integerp (obj)
  (cl:integerp obj))

(defun floatp (obj)
  (cl:floatp obj))

(defun numberp (obj)
  ;(numberp obj))
  ;; NOTE: This will fail on ratios, by design.
  (or (integerp obj)
      (floatp obj)))

(defun null (obj)
  (cl:null obj))

(defun consp (obj)
  (cl:consp obj))

(defun listp (obj)
  (cl:listp obj))

(defun symbolp (obj)
  (cl:symbolp obj))

(defun characterp (obj)
  (cl:characterp obj))

(defun stringp (obj)
  (cl:stringp obj))

(defun general-vector-p (obj)
  (and (not (stringp obj))
       (cl:vectorp obj)))

(defun basic-vector-p (obj)
  (or (stringp obj)
      (general-vector-p obj)))

(defun general-array*-p (obj)
  (and (cl:arrayp obj)
       (not (basic-vector-p obj))))

(defun basic-array*-p (obj)
  (general-array*-p obj))

(defun basic-array-p (obj)
  (or (basic-vector-p obj)
      (basic-array*-p obj)))

(defun functionp (obj)
  (cl:functionp obj))

(defun generic-function-p (obj)
   (cl:typep obj 'cl:generic-function))

(defun streamp (obj)
  (cl:streamp obj))

(defun instancep (obj class)
  (if (cl:typep class 'cl:class)
      ;; Treat conditions as <built-in-class>es
      (if (cl:eql class (cl:find-class 'cl:built-in-class))
          (or (cl:typep obj 'cl:built-in-class)
              (cl:typep obj 'cl:serious-condition)
              ;; Handle implementation-specific classes
              ;; (i.e. SEQUENCE in SBCL)
              (and (cl:typep obj 'cl:class)
                   (not (cl:typep obj 'cl:standard-class))))
          ;; Standard CL type check
          (cl:typep obj class))
      (islisp-sys:signal-domain-error class (cl:find-class 'cl:class))))

;;; Cons handling functions

(defun cons (obj1 obj2)
  (cl:cons obj1 obj2))

(defun car (cons)
  (cl:car (assure <cons> cons)))

(defun cdr (cons)
  (cl:cdr (assure <cons> cons)))

;;; Function application functions

(defun apply (function &rest lists)
  (cl:apply #'cl:apply (assure <function> function) lists))

(defun funcall (function &rest objs)
  (cl:apply (assure <function> function) objs))

;;; Numeric comparisons

(defun = (x1 x2)
  (cl:= (assure <number> x1) (assure <number> x2)))

(defun /= (x1 x2)
  (cl:/= (assure <number> x1) (assure <number> x2)))

(defun >= (x1 x2)
  (cl:>= (assure <number> x1) (assure <number> x2)))

(defun <= (x1 x2)
  (cl:<= (assure <number> x1) (assure <number> x2)))

(defun > (x1 x2)
  (cl:> (assure <number> x1) (assure <number> x2)))

(defun < (x1 x2)
  (cl:< (assure <number> x1) (assure <number> x2)))

;;; Arithmetic operators

(defun + (&rest nums)
  (cl:dolist (x nums)
    (assure <number> x))
  (apply #'cl:+ nums))

(defun * (&rest nums)
  (cl:dolist (x nums)
    (assure <number> x))
  (apply #'cl:* nums))

(defun - (x &rest nums)
  (if (null nums)
      (cl:- (assure <number> x))
      (progn
        (cl:dolist (y nums)
          (assure <number> y))
        (apply #'cl:- (cons x nums)))))

(defun quotient (dividend &rest divisors)
  (assure <number> dividend)
  (cl:dolist (divisor divisors)
    (assure <number> divisor)
    (if (= divisor 0)
        (cl:error (cl:make-condition 'cl:division-by-zero))))
  (cl:float (apply #'cl:/ (cons dividend divisors))))

(defun reciprocal (x)
  (if (= (assure <number> x) 0)
      (cl:error (cl:make-condition 'cl:division-by-zero))
      (cl:float (cl:/ x))))

(defun div (z1 z2)
  (cl:values (cl::truncate (cl:/ (assure <integer> z1)
                                 (assure <integer> z2)))))

(defun mod (z1 z2)
  (cl:mod (assure <integer> z1)
          (assure <integer> z2)))

;;; Setters

(defun set-car (obj cons)
  (cl:setf (cl:car (assure <cons> cons)) obj))

(defun set-cdr (obj cons)
  (cl:setf (cl:cdr (assure <cons> cons)) obj))

(defun set-aref (obj array &rest indeces)
  (islisp-sys:set-aref obj (assure <basic-array> array) indeces))

(defun set-garef (obj array &rest indeces)
  (islisp-sys:set-aref obj (assure <general-array> array) indeces))

(defun set-elt (obj sequence z)
  (cl:setf (cl:elt sequence z) obj))

(defun set-property (obj symbol property-name)
  (cl:setf (cl:get (assure <symbol> symbol) (assure <symbol> property-name)) obj))

;;; Creation functions

(defun create-list (i &rest initial-value)
  (assure <integer> i)
  (islisp-sys:assert-arity 'create-list initial-value 0 1)
  (cl:make-list i :initial-element (if initial-value
                                       (car initial-value)
                                       nil)))

(defun create-array (dimensions &rest initial-value)
  (assure <list> dimensions)
  (islisp-sys:assert-arity 'create-array initial-value 0 1)
  (cl:make-array dimensions :initial-element (if initial-value
                                                 (car initial-value)
                                                 nil)))

(defun create-string (i &rest initial-value)
  (assure <integer> i)
  (islisp-sys:assert-arity 'create-array initial-value 0 1)
  ;; Both OpenLisp and TISL use #\Space as the default for strings
  (cl:make-string i :initial-element (if initial-value
                                         (car initial-value)
                                         #\Space)))

(defun create-vector (i &rest initial-value)
  (assure <integer> i)
  (islisp-sys:assert-arity 'create-vector initial-value 0 1)
  (cl:make-array i :initial-element (if initial-value
                                        (car initial-value)
                                        nil)))

(defun create-string-input-stream (string)
  (cl:make-string-input-stream string))

(defun create-string-output-stream ()
  (cl:make-string-output-stream))

;;; List functions

(defun list (&rest objs)
  objs)

(defun append (&rest lists)
  (if (null lists)
      nil
      (progn
        (cl:dolist (sublist lists)
          (assure <list> sublist))
        (apply #'cl:append lists))))

(defun member (obj list)
  (cl:member obj (assure <list> list)))

(defun reverse (list)
  (cl:reverse (assure <list> list)))

(defun nreverse (list)
  (cl:nreverse (assure <list> list)))

(defun mapcar (function &rest lists)
  (assure <function> function)
  (islisp-sys:assert-arity 'mapcar lists 1 nil)
  (cl:dolist (list lists)
    (assure <list> list))
  (apply #'cl:mapcar function lists))

(defun mapc (function &rest lists)
  (assure <function> function)
  (islisp-sys:assert-arity 'mapc lists 1 nil)
  (cl:dolist (list lists)
    (assure <list> list))
  (apply #'cl:mapc function lists))

(defun mapcan (function &rest lists)
  (assure <function> function)
  (islisp-sys:assert-arity 'mapcan lists 1 nil)
  (cl:dolist (list lists)
    (assure <list> list))
  (apply #'cl:mapcan function lists))

(defun maplist (function &rest lists)
  (assure <function> function)
  (islisp-sys:assert-arity 'maplist lists 1 nil)
  (cl:dolist (list lists)
    (assure <list> list))
  (apply #'cl:maplist function lists))

(defun mapl (function &rest lists)
  (assure <function> function)
  (islisp-sys:assert-arity 'mapl lists 1 nil)
  (cl:dolist (list lists)
    (assure <list> list))
  (apply #'cl:mapl function lists))

(defun mapcon (function &rest lists)
  (assure <function> function)
  (islisp-sys:assert-arity 'mapcon lists 1 nil)
  (cl:dolist (list lists)
    (assure <list> list))
  (apply #'cl:mapcon function lists))

(defun assoc (obj association-list)
  (cl:dolist (cons (assure <list> association-list))
    (assure <cons> cons))
  (cl:assoc obj association-list))

;;; Array functions

(defun aref (basic-array &rest indeces)
  (assure <basic-array> basic-array)
  (apply #'cl:aref (cons basic-array indeces)))

(defun garef (general-array &rest indeces)
  (assure <general-array*> general-array)
  (apply #'cl:aref (cons general-array indeces)))

(defun array-dimensions (basic-array)
  (cl:array-dimensions (assure <basic-array> basic-array)))

;;; Vector/string functions

(defun vector (&rest objs)
  (cl:apply #'cl:vector objs))

(defun string= (string1 string2)
  (cl:string= (assure <string> string1) (assure <string> string2)))

(defun string/= (string1 string2)
  (cl:string/= (assure <string> string1) (assure <string> string2)))

(defun string< (string1 string2)
    (cl:string< (assure <string> string1) (assure <string> string2)))

(defun string> (string1 string2)
  (cl:string> (assure <string> string1) (assure <string> string2)))

(defun string<= (string1 string2)
  (cl:string<= (assure <string> string1) (assure <string> string2)))

(defun string>= (string1 string2)
  (cl:string>= (assure <string> string1) (assure <string> string2)))

(defun char-index (char string &rest start-position)
  (assure <character> char)
  (assure <string> string)
  (islisp-sys:assert-arity 'char-index start-position 0 1)
  (if (null start-position)
      (cl:position char string)
      (cl:position char string :start (assure <integer> (car start-position)))))

(defun string-index (substring string &rest start-position)
  (assure <string> substring)
  (assure <string> string)
  (islisp-sys:assert-arity 'string-index start-position 0 1)
  (let ((length (cl:length string))
        (sublength (cl:length substring)))
    (cl:labels ((find-string (i)
                  (cond ((> (+ i sublength) length)
                         nil)
                        ((string= substring
                                  (cl:subseq string i (+ i sublength)))
                         i)
                        (t (find-string (+ i 1))))))
      (if start-position
          (find-string (assure <integer> (car start-position)))
          (find-string 0)))))

(defun string-append (&rest strings)
  (cl:dolist (string strings)
    (assure <string> string))
  (apply #'cl:concatenate (cons 'cl:string strings)))

;;; Sequence functions
;; NOTE: ISLisp does not have a defined <sequence> class, so
;; the type checks are non-standard.

(defun length (sequence)
  (cl:length (assure <sequence> sequence)))

(defun elt (sequence z)
  (cl:elt (assure <sequence> sequence) (assure <integer> z)))

(defun subseq (sequence z1 z2)
  (cl:subseq (assure <sequence> sequence) (assure <integer> z1) (assure <integer> z2)))

(defun map-into (destination function &rest sequences)
  (assure <sequence> destination)
  (cl:dolist (sequence sequences)
    (assure <sequence> sequence))
  (cl:apply #'cl:map-into destination (assure <function> function) sequences))

;;; Characters

(defun char= (char1 char2)
  (cl:char= (assure <character> char1) (assure <character> char2)))

(defun char/= (char1 char2)
  (cl:char/= (assure <character> char1) (assure <character> char2)))

(defun char< (char1 char2)
  (cl:char< (assure <character> char1) (assure <character> char2)))

(defun char> (char1 char2)
  (cl:char> (assure <character> char1) (assure <character> char2)))

(defun char<= (char1 char2)
  (cl:char<= (assure <character> char1) (assure <character> char2)))

(defun char>= (char1 char2)
  (cl:char>= (assure <character> char1) (assure <character> char2)))

;;; Mathematical functions

(defun float (x)
  (cl:float (assure <number> x)))

(defun floor (x)
  (cl:values (cl:floor (assure <number> x))))

(defun ceiling (x)
  (cl:values (cl:ceiling (assure <number> x))))

(defun round (x)
  (cl:values (cl:round (assure <number> x))))

(defun truncate (x)
  (cl:values (cl:truncate (assure <number> x))))

(defun max (&rest nums)
  (islisp-sys:assert-arity 'max nums 1 nil)
  (cl:dolist (x nums)
    (assure <number> x))
  (cl:reduce #'cl:max nums))

(defun min (&rest nums)
  (islisp-sys:assert-arity 'min nums 1 nil)
  (cl:dolist (x nums)
    (assure <number> x))
  (cl:reduce #'cl:min nums))

(defun abs (x)
  (cl:abs (assure <number> x)))

(defun exp (x)
  (cl:exp (assure <number> x)))

(defun log (x)
  (cl:log (assure <number> x)))

(defun expt (x y)
  (cl:expt (assure <number> x) (assure <number> y)))

(defun sqrt (x)
  (cl:sqrt (assure <number> x)))

(defun isqrt (z)
  (cl:isqrt (assure <integer> z)))

(defun sin (x)
  (cl:sin (assure <number> x)))

(defun cos (x)
  (cl:cos (assure <number> x)))

(defun tan (x)
  (cl:tan (assure <number> x)))

(defun atan (x)
  (cl:atan (assure <number> x)))

(defun atan2 (x1 x2)
  (cl:atan (assure <number> x1) (assure <number> x2)))

(defun sinh (x)
  (cl:sinh (assure <number> x)))

(defun cosh (x)
  (cl:cosh (assure <number> x)))

(defun tanh (x)
  (cl:tanh (assure <number> x)))

(defun atanh (x)
  (cl:atanh (assure <number> x)))

(defun gcd (z1 z2)
  (cl:gcd (assure <integer> z1) (assure <integer> z2)))

(defun lcm (z1 z2)
  (cl:lcm (assure <integer> z1) (assure <integer> z2)))

;;; Standard I/O

(defun standard-input ()
  cl:*standard-input*)

(defun standard-output ()
  cl:*standard-output*)

(defun error-output ()
  cl:*error-output*)

;;; File I/O functions

(defun open-input-file (filename &rest element-class)
  (assure <string> filename)
  (islisp-sys:assert-arity 'open-input-file element-class 0 1)
  (if (or (null element-class)
          (eql (car element-class) (class <character>)))
      (cl:open filename :direction :input
                        :element-type 'cl:character)
      (if (eql (car element-class) 8)
          (cl:open filename :direction :input
                            :element-type '(cl:unsigned-byte 8))
          ;; NOTE: spec is technically silent on this
          ;; FIXME: domain-error takes an expected class, but <character> and 8 aren't the same
          ;; TISL uses <integer>, OpenLisp signals a <stream-error> instead
          (islisp-sys:signal-domain-error element-class (class <integer>)))))

;; NOTE: the standard doesn't say what to do if the file exists, so we :supersede
(defun open-output-file (filename &rest element-class)
  (assure <string> filename)
  (islisp-sys:assert-arity 'open-output-file element-class 0 1)
  (if (or (null element-class)
          (eql (car element-class) (class <character>)))
      (cl:open filename :direction :output
                        :element-type 'cl:character
                        :if-exists :supersede)
      (if (eql (car element-class) 8)
          (cl:open filename :direction :output
                            :element-type '(cl:unsigned-byte 8)
                            :if-exists :supersede)
          ;; NOTE: spec is technically silent on this
          ;; FIXME: domain-error takes an expected class, but <character> and 8 aren't the same
          ;; TISL uses <integer>, OpenLisp signals a <stream-error> instead
          (islisp-sys:signal-domain-error element-class (class <integer>)))))

;; NOTE: the standard doesn't say what to do if the file exists, so we :supersede
(defun open-io-file (filename &rest element-class)
  (assure <string> filename)
  (islisp-sys:assert-arity 'open-io-file element-class 0 1)
  (if (or (null element-class)
          (eql (car element-class) (class <character>)))
      (cl:open filename :direction :io
                        :element-type 'cl:character
                        :if-exists :supersede)
      (if (eql (car element-class) 8)
          (cl:open filename :direction :io
                            :element-type '(cl:unsigned-byte 8)
                            :if-exists :supersede)
          ;; NOTE: spec is technically silent on this
          ;; FIXME: domain-error takes an expected class, but <character> and 8 aren't the same
          ;; TISL uses <integer>, OpenLisp signals a <stream-error> instead
          (islisp-sys:signal-domain-error element-class (class <integer>)))))

;;; Stream functions

(defun output-stream-p (obj)
  (and (streamp obj)
       (cl:output-stream-p obj)))

(defun input-stream-p (obj)
  (and (streamp obj)
       (cl:input-stream-p obj)))

;; Note: result is implementation defined
(defun close (stream)
  (cl:close (assure <stream> stream)))

(defun finish-output (stream)
  ;; NOTE: there is not a specific type for an output-stream
  (if (not (output-stream-p stream))
      (islisp-sys:signal-domain-error stream (class <stream>))
      (cl:finish-output stream)))

(defun get-output-stream-string (stream)
  ;; There's no standard way to check for a string stream in ISLISP
  ;; TISL uses a <string-stream> class
  (if (not (cl:typep stream 'cl:string-stream))
      (islisp-sys:signal-domain-error stream (class <stream>))
      (cl:get-output-stream-string stream)))

;;; Input functions

(defun read-char (&rest arguments)
  (islisp-sys:assert-arity 'read-char arguments 0 3)
  (cl:destructuring-bind (cl:&optional (input-stream (standard-input))
                                       (eos-error-p t)
                                       (eos-value nil))
                         arguments
  ;; NOTE: there is not a specific type for an input-stream
  (if (not (input-stream-p input-stream))
      (islisp-sys:signal-domain-error input-stream (class <stream>))
      (cl:read-char input-stream eos-error-p eos-value))))

(defun preview-char (&rest arguments)
  (islisp-sys:assert-arity 'preview-char arguments 0 3)
  (cl:destructuring-bind (cl:&optional (input-stream (standard-input))
                                       (eos-error-p t)
                                       (eos-value nil))
                         arguments
  ;; NOTE: there is not a specific type for an input-stream
  (if (not (input-stream-p input-stream))
      (islisp-sys:signal-domain-error input-stream (class <stream>))
      (cl:peek-char nil input-stream eos-error-p eos-value))))

(defun read-line (&rest arguments)
  (islisp-sys:assert-arity 'read-line arguments 0 3)
  (cl:destructuring-bind (cl:&optional (input-stream (standard-input))
                                       (eos-error-p t)
                                       (eos-value nil))
                         arguments
  ;; NOTE: there is not a specific type for an input-stream
  (if (not (input-stream-p input-stream))
      (islisp-sys:signal-domain-error input-stream (class <stream>))
      (cl:read-line input-stream eos-error-p eos-value))))

(defun read (&rest arguments)
  (islisp-sys:assert-arity 'read arguments 0 3)
  (cl:destructuring-bind (cl:&optional (input-stream (standard-input))
                                       (eos-error-p t)
                                       (eos-value nil))
                         arguments
  ;; NOTE: there is not a specific type for an input-stream
  (if (not (input-stream-p input-stream))
      (islisp-sys:signal-domain-error input-stream (class <stream>))
      (islisp-sys:islisp-read input-stream eos-error-p eos-value))))

(defun stream-ready-p (input-stream)
  (and (input-stream-p input-stream)
       (preview-char input-stream nil nil)))

;;; Output functions

(defun format-char (output-stream char)
  (assure <character> char)
  ;; NOTE: There is not a specific class for an output-stream
  (if (not (output-stream-p output-stream))
      (islisp-sys:signal-domain-error output-stream (class <stream>))
      (cl:write-char char output-stream)))

(defun format-float (output-stream float)
  (assure <float> float)
  ;; NOTE: There is not a specific class for an output-stream
  (if (not (output-stream-p output-stream))
      (islisp-sys:signal-domain-error output-stream (class <stream>))
      (cl:princ float output-stream)))

(defun format-fresh-line (output-stream)
  ;; NOTE: There is not a specific class for an output-stream
  (if (not (output-stream-p output-stream))
      (islisp-sys:signal-domain-error output-stream (class <stream>))
      (cl:format output-stream "~&")))

(defun format-integer (output-stream integer radix)
  (assure <integer> integer)
  ;; NOTE: There is not a specific class for an output-stream
  (if (not (output-stream-p output-stream))
      (islisp-sys:signal-domain-error output-stream (class <stream>))
      (cl:format output-stream
                 (cl:format nil "~~~DR" radix)
                 integer)))

(defun format-object (output-stream object escape-p)
  ;; NOTE: There is not a specific class for an output-stream
  (if (not (output-stream-p output-stream))
      (islisp-sys:signal-domain-error output-stream (class <stream>))
      ;; TODO: pretty print '(quote) and `(quasiquote)
      (if escape-p
          (cl:prin1 object output-stream)
          (cl:princ object output-stream))))

(defun format-tab (output-stream column)
  ;; NOTE: There is not a specific class for an output-stream
  (if (not (output-stream-p output-stream))
      (islisp-sys:signal-domain-error output-stream (class <stream>))
      (cl:format output-stream
                 (cl:format nil "~~~DT" column))))

(defun format (output-stream format-string &rest objs)
  (assure <stream> output-stream)
  (assure <string> format-string)
  ;; NOTE: There is not a specific class for an output-stream
  (if (not (output-stream-p output-stream))
      (islisp-sys:signal-domain-error output-stream (class <stream>)))
  ;; The spec doesn't say what to do if the format string is bad
  ;; TISL and OpenLisp signal a program-error, though, so that's what we're doing
  ;; If fmt is a symbol, that's the reason it failed
  (let ((fmt (islisp-sys:validate-format-string format-string objs)))
    (if (stringp fmt)
        (apply #'cl:format output-stream fmt objs)
        (islisp-sys:signal-program-error 'format fmt))))

;;; Binary I/O

(defun read-byte (input-stream &rest arguments)
  (islisp-sys:assert-arity 'read-byte arguments 1 2)
  (cl:destructuring-bind (cl:&optional (eos-error-p t)
                                       (eos-value nil))
                        arguments
    ;; NOTE: There is not a specific class for an input-stream
    (if (not (input-stream-p input-stream))
        (islisp-sys:signal-domain-error input-stream (class <stream>))
        (cl:read-byte (assure <stream> input-stream) eos-error-p eos-value))))

(defun write-byte (output-stream z)
  ;; NOTE: There is not a specific class for an output-stream
  (if (not (output-stream-p output-stream))
      (islisp-sys:signal-domain-error output-stream (class <stream>))
      ;; There's no way to check bytes, either
      (if (or (< z 0)
              (> z 255))
          (islisp-sys:signal-domain-error z (class <integer>))
          (cl:write-byte output-stream z))))

;;; File handling

(defun probe-file (filename)
  (if (cl:probe-file (assure <string> filename))
      t
      nil))

(defun file-position (stream)
  (cl:file-position (assure <stream> stream)))

(defun set-file-position (stream z)
  (cl:file-position (assure <stream> stream) (assure <integer> z))
  (file-position stream))

(defun file-length (filename element-class)
  (let ((element-type (cond ((eql element-class 8)
                             '(cl:unsigned-byte 8))
                            ((eql element-class (class <character>))
                             'cl:character)
                            (t ; Hope the implementation handles it...
                             element-class))))
    (cl:with-open-file (stream filename :direction :input
                                        :element-type element-type)
      (cl:file-length stream))))

;;; Class enquiry

;; Needs to be defined here so the type checkers know it exists
(cl:define-condition <parse-error> (cl:parse-error)
  ((string :initarg :string
           :reader %parse-error-string)
   (expected-class :initarg :expected-class
                   :reader %parse-error-expected-class)))

(defun class-of (obj)
  ;; In theory, these three classes should always have a more
  ;; specific subclass and never be returned by this functions:
  ;;   <list>  <number>  <basic-array>
  (cond ((null obj) (class <null>))
        ((symbolp obj) (class <symbol>))
        ((consp obj) (class <cons>))
        ((integerp obj) (class <integer>))
        ((floatp obj) (class <float>))
        ((streamp obj) (class <stream>))
        ((characterp obj) (class <character>))
        ((stringp obj) (class <string>))
        ((general-vector-p obj) (class <general-vector>))
        ((basic-vector-p obj) (class <basic-vector>))
        ((general-array*-p obj) (class <general-array*>))
        ((basic-array*-p obj) (class <basic-array*>))
        ((generic-function-p obj) (class <generic-function>))
        ((cl:typep obj 'cl:standard-object) (class <standard-object>))
        ((cl:typep obj 'cl:built-in-class) (class <built-in-class>))
        ((cl:typep obj 'cl:standard-class) (class <standard-class>))
        ;; Conditions and errors
        ;; NOTE: Make sure the tests are in reverse order of genericity
        ((cl:typep obj 'cl:end-of-file) (class <end-of-stream>))
        ((cl:typep obj 'cl:stream-error) (class <stream-error>))
        ((cl:typep obj 'cl:simple-error) (class <simple-error>))
        ((cl:typep obj 'cl:division-by-zero) (class <division-by-zero>))
        ((cl:typep obj 'cl:floating-point-overflow) (class <floating-point-overflow>))
        ((cl:typep obj 'cl:floating-point-underflow) (class <floating-point-underflow>))
        ((cl:typep obj 'cl:control-error) (class <control-error>))
        ;; Special case for the two parse-errors
        ((cl:typep obj 'cl:parse-error) (cl:find-class 'cl:parse-error))
        ((cl:typep obj '<parse-error>) (class <parse-error>))
        ((cl:typep obj 'cl:type-error) (class <domain-error>))
        ((cl:typep obj 'cl:unbound-variable) (class <unbound-variable>))
        ((cl:typep obj 'cl:undefined-function) (class <undefined-function>))
        ((cl:typep obj 'cl:cell-error) (class <undefined-entity>))
        ((cl:typep obj 'cl:program-error) (class <program-error>))
        ((cl:typep obj 'cl:error) (class <error>))
        ((cl:typep obj 'cl:serious-condition) (class <serious-condition>))
        ;; All other possible values
        (t (cl:class-of obj))))

(defun subclassp (class1 class2)
  (cl:values (cl:subtypep (assure <class> class1)
                          (assure <class> class2))))

;;; Error handling

(defun signal-condition (condition continuable)
  (if continuable
      (progn
        (assure <string> continuable)
        (cl:restart-case
            ;(cl:cerror (assure <string> continuable) condition)
            (cl:error condition)
          (cl:use-value (value)
            :interactive islisp-sys:read-new-value
            :report (cl:lambda (stream)
                      (cl:write-string continuable stream))
            value)))
      (cl:error condition)))

(defun error (error-string &rest objs)
  (assure <string> error-string)
  (signal-condition (cl:make-condition 'cl:simple-error
                                       :format-control error-string
                                       :format-arguments objs)
                    nil))
  ;(apply #'cl:error (assure <string> error-string) objs))

(defun cerror (continue-string error-string &rest objs)
  (assure <string> continue-string)
  (assure <string> error-string)
  (let ((message (apply #'cl:format nil continue-string objs)))
    (signal-condition (cl:make-condition 'cl:simple-error
                                         :format-control error-string
                                         :format-arguments objs)
                      message)))
  ;(apply #'cl:cerror (assure <string> continue-string)
  ;                   (assure <string> error-string)
  ;                   objs))

(defun condition-continuable (condition)
  (assure <serious-condition> condition)
  ;; A condition is continuable if the most recent restart
  ;; is CONTINUE or USE-VALUE.
  (let* ((restarts (cl:compute-restarts condition))
         (restart (car restarts)))
    (if (or (eq (cl:restart-name restart) 'cl:use-value)
            (eq (cl:restart-name restart) 'cl:continue))
        (cl:format nil "~A" restart)
        nil)))

(defun continue-condition (condition &rest value)
  (assure <serious-condition> condition)
  (let ((value (if value
                   (progn
                     (islisp-sys:assert-arity 'continue-condition value 1 1)
                     (car value))
                   nil)))
    (if (condition-continuable condition)
        (let* ((restarts (cl:compute-restarts condition))
               (restart (car restarts)))
          (cl:case (cl:restart-name restart)
            ((cl:use-value) (cl:use-value value condition))
            ((cl:continue) (cl:continue condition))))
        ;; TISL and OpenLisp signal a <control-error> if
        ;; the condition is not continuable.
        (islisp-sys:signal-control-error))))

;;; Condition class accessors

(defun arithmetic-error-operation (arithmetic-error)
  (cl:arithmetic-error-operation (assure <arithmetic-error> arithmetic-error)))

(defun arithmetic-error-operands (arithmetic-error)
  (cl:arithmetic-error-operands (assure <arithmetic-error> arithmetic-error)))

(defun domain-error-object (domain-error)
  (cl:type-error-datum (assure <domain-error> domain-error)))

(defun domain-error-expected-class (domain-error)
  (cl:type-error-expected-type (assure <domain-error> domain-error)))

;; NOTE: parse-error-string and parse-error-expected-class have
;; no direct CL equivalents that I can find.
(defun parse-error-string (parse-error)
  (%parse-error-string (assure <parse-error> parse-error)))

(defun parse-error-expected-class (parse-error)
  (%parse-error-expected-class (assure <parse-error> parse-error)))

(defun simple-error-format-string (simple-error)
  (cl:simple-condition-format-control (assure <simple-error> simple-error)))

(defun simple-error-format-arguments (simple-error)
  (cl:simple-condition-format-arguments (assure <simple-error> simple-error)))

(defun stream-error-stream (stream-error)
  (cl:stream-error-stream (assure <stream-error> stream-error)))

(defun undefined-entity-name (undefined-entity)
  (cl:cell-error-name (assure <undefined-entity> undefined-entity)))

(defun undefined-entity-namespace (undefined-entity)
  (cl:typecase (assure <undefined-entity> undefined-entity)
    (cl:unbound-variable
     (if (islisp-sys:dynamic-var-p (cl:cell-error-name undefined-entity))
         'dynamic-variable
         'variable))
    (cl:undefined-function
     'function)
    (t
     (if (cl:find-class (cl:cell-error-name undefined-entity))
           'class
           'unknown))))

;;; Misc.

(defun property (symbol property-name &rest obj)
  (islisp-sys:assert-arity 'property obj 0 1)
  (cl:get symbol property-name (if obj (car obj) nil)))

(defun remove-property (symbol property-name)
  (cl:remprop (assure <symbol> symbol) (assure <symbol> property-name)))

(defun identity (obj)
  obj)

(defun parse-number (string)
  (assure <string> string)
  (if (islisp-sys:valid-number-p string)
      (cl:values (cl:read-from-string string))
      (cl:error (cl:make-condition '<parse-error>
                                   :string string
                                   :expected-class (class <number>)))))
