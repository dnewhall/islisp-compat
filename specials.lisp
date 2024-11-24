;;;; -*- Mode:Lisp; indent-tabs-mode:nil -*-
;;;; islisp-specials.lisp
;;;; Copyright (c) 2011-2024 Derek Newhall
;;;; SPDX-License-Identifier: CDDL-1.1
;;;;
;;;; This file contains all the special forms for ISLisp
;;;; along with the constant and generic function definitions.
;;;; It requires the functions defined in islisp-functions.lisp.
;;;;
;;;; The only special forms not defined here are:
;;;;     defun progn if and or cond
;;;;     let let* function lambda
;;;;     assure the class
;;;; Those special forms are in islisp-functions.lisp.
;;;;
;;;; The symbols T, NIL, QUOTE, &REST are imported from CL.
(cl:in-package #:islisp)

;;; Toplevel defining forms

(cl:defmacro defmacro (name params &rest body)
  `(cl:defmacro ,name ,(islisp-sys:fix-lambda-list params)
     ,@body))

(defmacro defconstant (name form)
  ;; Muffle the style-warnings about *...* in SBCL
  #+sbcl
  `(cl:handler-bind ((cl:style-warning (cl:lambda (condition)
                                         (cl:declare (cl:ignore condition))
                                         (cl:muffle-warning))))
    (cl:defconstant ,name ,form)
    ',name)
  #-sbcl
  `(progn
     (cl:defconstant ,name ,form)
     ',name))

(defmacro defglobal (name form)
  (islisp-sys:assure-identifier name `(defglobal ,name ,form))
  (let ((global-name (cl:gensym (cl:symbol-name name))))
    `(progn
       ;; Need to use a symbol macro to allow lexical binding to work.
       ;; SBCL and others don't allow lexical globals to be shadowed,
       ;; so we don't use their SB-EXT:DEFGLOBAL, CCL:DEFGLOGAL, etc.
       (cl:defparameter ,global-name ,form)
       (cl:define-symbol-macro ,name ,global-name)
       ',name)))

(defmacro defdynamic (name form)
  (let ((dynamic-name (islisp-sys:fix-dynamic-name name)))
    `(progn
       (cl:defvar ,dynamic-name ,form)
       ',name)))

(defmacro defclass (class-name sc-names slot-specs &rest class-opts)
  (assure <list> sc-names)
  (assure <list> slot-specs)
  (cl:destructuring-bind (cl::&key abstractp metaclass)
                         (cl:if class-opts
                             (cl:car class-opts)
                             nil)
    (cl:declare (cl:ignorable abstractp))
    (let ((boundp-forms '())
             (slots '()))
      ;; Fix slot-opts
      (cl:dolist (slot-spec slot-specs)
        (cl:destructuring-bind (slot . defs)
                               (islisp-sys:fix-slot-spec slot-spec)
          (cl:push slot slots)
          (cl:setq boundp-forms (cl:append boundp-forms defs))))
      `(progn
         ,(cl:if metaclass
                `(cl:defclass ,class-name ,sc-names
                   ,slots
                   (:metaclass ,metaclass))
                `(cl:defclass ,class-name ,sc-names
                   ,slots))
         ,(if abstractp
              `(cl:defmethod cl:make-instance ((class (cl:eql (class ,class-name))) cl:&rest initargs cl:&key cl:&allow-other-keys)
                 (cl:declare (cl:ignore initargs))
                 (islisp-sys:signal-program-error 'create 'class-abstract)))
         ,@boundp-forms
         ',class-name))))

;;; Quasiquote

(cl:defmacro quasiquote (form)
  (islisp-sys:bq-completely-process form))

(cl:defmacro unquote (form)
  (islisp-sys:violation (cl:list 'unquote form) "Unquote not inside of quasiquote"))

(cl:defmacro unquote-splicing (form)
  (islisp-sys:violation (cl:list 'unquote-splicing form) "Unquote-splicing not inside of quasiquote"))

;;; Dynamic variables

(defmacro dynamic (var)
  (assure <symbol> var)
  (islisp-sys:fix-dynamic-name var))

(defmacro dynamic-let (bindings &rest body)
  (islisp-sys:validate-variable-list bindings)
  (let ((vars (cl:mapcar (lambda (binding)
                           (islisp-sys:fix-dynamic-name (car binding)))
                      bindings)))
    `(let ,(cl:mapcar (lambda (binding)
                        `(,(islisp-sys:fix-dynamic-name (car binding)) ,@(cdr binding)))
                      bindings)
       (cl:declare (cl:special ,@vars))
       ,@body)))

;;; Function special forms

(defmacro labels (fspecs &rest body-forms)
  `(cl:labels ,(islisp-sys:validate-function-list fspecs)
    ,@body-forms))

(defmacro flet (fspecs &rest body-forms)
  `(cl:flet ,(islisp-sys:validate-function-list fspecs)
    ,@body-forms))

;; Control special forms

(defmacro case (keyform &rest clauses)
  (cl:mapl (cl:lambda (clauselist)
             (let* ((clause (cl:car clauselist))
                    (keys (cl:car clause)))
             ;; Last clause can start with T.
             (if (null (cdr clauselist))
                 (if (not (or (listp keys)
                              (eq keys t)))
                     (islisp-sys:violation keys
                                           "Final ~A clause must start with a list of keys or ~A, not ~A."
                                           'case 't keys))
                 (if (not (listp keys))
                     (islisp-sys:violation keys
                                           "~A clause must start with a list of keys, not ~A."
                                           'case keys)))))
           clauses)
  `(cl:case ,keyform ,@clauses))

(defmacro case-using (predform keyform &rest clauses)
  (let ((fn (cl:gensym))
        (arg (cl:gensym))
        (key-form (cl:gensym)))
    `(let* ((,key-form ,keyform)
            (,fn (lambda (,arg)
                   (cl:funcall ,predform ,key-form ,arg))))
       (cond ,@(cl:maplist (lambda (clauselist)
                             (let* ((clause (cl:car clauselist))
                                    (keys (cl:car clause))
                                    (forms (cl:cdr clause)))
                               ;; Last clause can start with T.
                               (if (cl:null (cl:cdr clause))
                                   (cond ((listp keys)
                                          `((cl:member-if ,fn ',keys)
                                            ,@forms))
                                         ((cl:eq keys t)
                                          `(t ,@forms))
                                         (t
                                          (islisp-sys:violation keys "Final ~A clause must start with a list of keys or ~A, not ~A."
                                                                'case 't keys)))
                                   `((cl:member-if ,fn ',(assure <list> keys))
                                     ,@forms))))
                           clauses)))))

;;; Non-local exits

(defmacro block (name &rest forms)
  `(cl:block ,(assure <symbol> name)
    ,@forms))

(defmacro return-from (name result-form)
  `(cl:return-from ,(assure <symbol> name) ,result-form))

(defmacro catch (tag-form &rest forms)
  `(cl:catch ,tag-form
    ,@forms))

(defmacro throw (tag-form result-form)
  `(cl:throw ,tag-form ,result-form))

(defmacro tagbody (&rest forms)
  `(cl:tagbody ,@forms))

(defmacro go (tag)
  `(cl:go ,tag))

(defmacro unwind-protect (form &rest cleanup-forms)
  `(cl:unwind-protect ,form
     ,@cleanup-forms))

;;; Iteration control forms

(defmacro while (test-form &rest body-forms)
  (let ((begin (cl:gensym))
        (end (cl:gensym)))
    `(tagbody
        ,begin
        (if (not ,test-form)
            (go ,end))
        (progn ,@body-forms)
        (go ,begin)
        ,end
        nil)))

(defmacro for (iteration-specs end-test-result-forms &rest body)
  (let ((block (gensym))
        (begin (gensym))
        (end (gensym))
        (inits (cl:mapcar (lambda (spec)
                            (assure <cons> spec)
                            (case (cl:length spec)
                              ((2 3) `(,(car spec) ,(car (cdr spec))))
                              (t (islisp-sys:violation 'for
                                                       "Iteration specifier must have 2 or 3 items, not ~D"
                                                       (cl:length spec)))))
                          iteration-specs))
        (updates (cl:mapcar (lambda (spec)
                              (if (cl:= (cl:length spec) 3)
                                  `(setq ,(cl:car spec)
                                         ,(cl:car (cl:cdr (cl:cdr spec))))
                                  nil))
                            iteration-specs))
        (end-test (if (cl:null end-test-result-forms)
                      (islisp-sys:violation 'for
                                            "Missing end test")
                      (cl:car end-test-result-forms)))
        (result-forms (if (cl:null (cl:cdr end-test-result-forms))
                          '(nil)
                          (cl:cdr end-test-result-forms))))
    ;; Handle the four different usages...
    ;; 1) Standard usage: has both variable forms and an ending test
    (cond ((and inits end-test)
           `(let ,inits
              (block ,block
                (tagbody
                 ,begin
                 (if ,end-test
                     (go ,end))
                 (progn ,@body)
                 (progn ,@updates)
                 (go ,begin)
                 ,end
                 (return-from ,block (progn ,@result-forms))))))
          ;; 2) Until loop
          (end-test
           `(block ,block
             (tagbody
               ,begin
               (if ,end-test
                   (go ,end))
               (progn ,@body)
               (go ,begin)
               ,end
               (return-from ,block (progn ,@result-forms)))))
          ;; 3) Infinite loop with update forms
          (inits
           `(let ,inits
              (tagbody
                 ,begin
                 (progn ,@body)
                 (progn ,@updates)
                 (go ,begin))))
          ;; 4) Infinite loop
          (t
           `(tagbody
               ,begin
               (progn ,@body)
               (go ,begin))))))

;;; Setter special forms

(defmacro setq (var form)
  `(cl:setq ,var ,form))

(defmacro setf (place form)
  (if (cl:symbolp place)
      `(setq ,place ,form)
      (if (cl:listp place)
          (flet ((assure-length (length)
                  (if (cl:/= (cl:length place) length)
                      (islisp-sys:violation `(setf ,place, form)
                                            "~A form should have length of ~D"
                                            (cons 'setf (cl:car place)) length))))
            (case (cl:car place)
              ((car)
                (assure-length 2)
                `(set-car ,form ,(cl:car (cl:cdr place))))
              ((cdr)
                (assure-length 2)
                `(set-cdr ,form ,(cl:car (cl:cdr place))))
              ((dynamic)
                (assure-length 2)
                `(set-dynamic ,form ,(cl:car (cl:cdr place))))
              ((aref)
               ;; FIXME :check length
                `(set-aref ,form
                           ,(cl:car (cl:cdr place))
                           ,@(cl:cdr (cl:cdr place))))
              ((garef)
               ;; FIXME :check length
                `(set-garef ,form
                            ,(cl:car (cl:cdr place))
                            ,@(cl:cdr (cl:cdr place))))
              ((elt)
                (assure-length 3)
                `(set-elt ,form
                          ,(cl:car (cl:cdr place))
                          ,(cl:car (cl:cdr (cl:cdr place)))))
              ((property)
                (assure-length 3)
                `(set-property ,form
                               ,(cl:car (cl:cdr place))
                               ,(cl:car (cl:cdr (cl:cdr place)))))
              (t
                `(cl:setf ,place ,form))))
          (islisp-sys:violation `(setf ,place ,form)
                                "First argument to ~A must be a symbol or list, not ~A."
                 (cl:cons 'setf place) place))))

(defmacro set-dynamic (form var)
  `(cl:setq ,(islisp-sys:fix-dynamic-name (assure <symbol> var)) ,form))

;;; Standard I/O special forms

(defmacro with-standard-input (stream-form &rest forms)
  `(let ((cl:*standard-input* ,stream-form))
    ,@forms))

(defmacro with-standard-output (stream-form &rest forms)
  `(let ((cl:*standard-output* ,stream-form))
    ,@forms))

(defmacro with-error-output (stream-form &rest forms)
  `(let ((cl:*error-output* ,stream-form))
    ,@forms))

;;; File I/O special forms

(defmacro with-open-input-file (stream-spec &rest forms)
  (islisp-sys:assert-arity 'with-open-input-file stream-spec 2 3)
  (let ((name (islisp-sys:assure-identifier (cl:car stream-spec)
                                            stream-spec))
        (filename (cl:car (cl:cdr stream-spec)))
        (element-class (if (cl:null (cl:cdr (cl:cdr stream-spec)))
                           (class <character>)
                           (cl:car (cl:cdr (cl:cdr stream-spec))))))
    `(cl:with-open-stream (,name (open-input-file ,filename
                                                  ,element-class))
       ,@forms)))

(defmacro with-open-output-file (stream-spec &rest forms)
  (islisp-sys:assert-arity 'with-open-output-file stream-spec 2 3)
  (let ((name (islisp-sys:assure-identifier (cl:car stream-spec)
                                            stream-spec))
        (filename (cl:car (cl:cdr stream-spec)))
        (element-class (if (cl:null (cl:cdr (cl:cdr stream-spec)))
                           (class <character>)
                           (cl:car (cl:cdr (cl:cdr stream-spec))))))
    `(cl:with-open-stream (,name (open-output-file ,filename
                                                   ,element-class))
      ,@forms)))

(defmacro with-open-io-file (stream-spec &rest forms)
  (islisp-sys:assert-arity 'with-open-io-file stream-spec 2 3)
  (let ((name (islisp-sys:assure-identifier (cl:car stream-spec)
                                            stream-spec))
        (filename (cl:car (cl:cdr stream-spec)))
        (element-class (if (cl:null (cl:cdr (cl:cdr stream-spec)))
                           (class <character>)
                           (cl:car (cl:cdr (cl:cdr stream-spec))))))
    `(cl:with-open-stream (,name (open-io-file ,filename
                                               ,element-class))
      ,@forms)))

;;; Method definition

(defmacro defgeneric (func-spec lambda-list &rest options)
  (cl:assert (cl:or (islisp-sys:identifierp func-spec)
                    (cl:and (cl:consp func-spec)
                            (cl:eq (cl:car func-spec) 'setf))))
  (let ((params (islisp-sys:fix-lambda-list lambda-list))
        (methods nil)
        (method-combination nil)
        (method-combination-set-p nil)
        (generic-function-class nil))
    ;; Handle options
    (cl:dolist (option options)
      (cl:assert (cl:consp option))
      (cl:ecase (cl:car option)
        ((:method)
         (islisp-sys:assert-arity 'defgeneric option 1 nil)
         (cl:push `(defmethod ,func-spec ,@(cl:cdr option)) methods))
        ((:method-combination)
         (islisp-sys:assert-arity 'defgeneric option 2 2)
         (if method-combination-set-p
             (islisp-sys:violation option "Defgeneric option :method-combination seen multiple times.")
             (progn
               (setq method-combination (cl:car (cl:cdr option)))
               ;; Convert method combination to CL one
               (if (eq method-combination 'standard)
                   'cl:standard
                   method-combination))))
        ((:generic-function-class)
         (islisp-sys:assert-arity 'defgeneric option 2 2)
         (if generic-function-class
             (islisp-sys:violation option "Defgeneric option :generic-function-class seen multiple times.")
             (setq generic-function-class (cl:car (cl:cdr option)))))))
    ;; Output
    `(progn
       ,(if options
            (cl:append `(cl:defgeneric ,func-spec ,params)
                       (if method-combination
                           `((:method-combination ,method-combination)))
                       (if generic-function-class
                           `(:generic-function-name ,(islisp-sys:cl-class-name generic-function-class))))
            `(cl:defgeneric ,func-spec ,params))
       ,@(if methods
             (cl:nreverse methods))
       ',func-spec)))

(defmacro defmethod (func-spec &rest method-forms)
  (cl:assert (cl:or (islisp-sys:identifierp func-spec)
                    (cl:and (cl:consp func-spec)
                            (cl:eq (cl:car func-spec) 'setf))))
  (let ((method-qualifier nil)
        (parameter-profile nil)
        (forms nil))
    ;; Test for method-qualifier
    (if method-forms
        (if (cl:consp (cl:car method-forms))
            (progn
              (setq parameter-profile (cl:car method-forms))
              (setq forms (cl:cdr method-forms)))
            (progn
              (setq method-qualifier (cl:car method-forms))
              (setq parameter-profile (cl:car (cl:cdr method-forms)))
              (setq forms (cl:cdr (cl:cdr method-forms))))))
    ;; Fix parameters
    (setq parameter-profile (islisp-sys:fix-method-parameter-profile parameter-profile))
    (if method-qualifier
      `(cl:defmethod ,func-spec ,method-qualifier ,parameter-profile
        ,@forms)
      `(cl:defmethod ,func-spec ,parameter-profile
        ,@forms))))

;;; Object creation

(defgeneric initialize-object (instance initialization-arguments))

(defmethod initialize-object ((instance <standard-object>) initialization-arguments)
  (apply #'cl:initialize-instance instance initialization-arguments))

(defgeneric create (class &rest init-args))

(defmethod create ((class <standard-class>) &rest init-args)
  (cond ((cl:typep class (class <standard-class>))
         (let ((object (apply #'cl:make-instance class init-args)))
           (initialize-object object init-args)))
        (t
         (islisp-sys:signal-domain-error class (class <standard-class>)))))

;;; Error handling

(defmacro ignore-errors (&rest forms)
  `(cl:ignore-errors ,@forms))

(defmacro with-handler (handler &rest forms)
  (let ((handler-gs (cl:gensym)))
    `(let ((,handler-gs (assure <function> ,handler)))
       (cl:handler-bind ((cl:serious-condition
                          (lambda (condition)
                            (cl:funcall ,handler-gs condition)
                            (islisp-sys:signal-control-error))))
          ,@forms))))

(defgeneric report-condition (condition stream))

(defmethod report-condition ((condition <serious-condition>) stream)
  (format-object stream condition nil))

;;; Convert

(defmacro convert (obj class-name)
  ;; If obj is a constant quoted form, redo with the quoted form.
  ;; Only lists and symbols don't evaluate to themselves
  (if (and (consp obj)
           (eq (car obj) 'quote)
           (consp (cdr obj))
           (not (listp (car (cdr obj))))
           (not (symbolp (car (cdr obj)))))
      (setq obj (car (cdr obj))))
  (let ((obj-gs (gensym))
        ;; Need to capture these so they don't get shadowed locally.
        (characterp #'characterp)
        (stringp #'stringp)
        (general-vector-p #'general-vector-p)
        (listp #'listp)
        (integerp #'integerp)
        (floatp #'floatp)
        (symbolp #'symbolp))
    (cl:ecase class-name
      ((<character>)
       (cond ((characterp obj) obj)
             ((integerp obj) (cl:code-char obj))
             (t `(let ((,obj-gs ,obj))
                   (cond ((funcall ,characterp ,obj-gs) ,obj-gs)
                         ((funcall ,integerp ,obj-gs) (cl:code-char ,obj-gs))
                         (t (islisp-sys:signal-domain-error ,obj-gs (class ,class-name))))))))

      ((<list>)
       (cond ((null obj) obj)
             ((stringp obj) `(cl:coerce ,obj 'cl:list))
             ((general-vector-p obj) `(cl:coerce ,obj 'cl:list))
             ;; Special check for a quoted list
             ((and (consp obj)
                   (eq (car obj) 'quote)
                   (consp (cdr obj))
                   (listp (car (cdr obj)))) `,(car (cdr obj)))
              (t `(let ((,obj-gs ,obj))
                    (cond ((funcall ,listp ,obj-gs) ,obj-gs)
                          ((funcall ,stringp ,obj-gs) (cl:coerce ,obj-gs 'cl:string))
                          ((funcall ,general-vector-p ,obj-gs) (cl:coerce ,obj-gs 'cl:vector))
                          (t (islisp-sys:signal-domain-error ,obj-gs (class ,class-name))))))))

      ((<general-vector>)
       (cond ((general-vector-p obj) obj)
             ((stringp obj) (cl:coerce obj 'cl:vector))
             ;; Special check for a quoted list
             ((and (consp obj)
                   (eq (car obj) 'quote)
                   (consp (cdr obj))
                   (listp (car (cdr obj)))) (cl:coerce (car (cdr obj)) 'cl:vector))
             (t `(let ((,obj-gs ,obj))
                   (cond ((funcall ,general-vector-p ,obj-gs) ,obj-gs)
                         ((funcall ,stringp ,obj-gs) (cl:coerce ,obj-gs 'cl:vector))
                         ((funcall ,listp ,obj-gs) (cl:coerce ,obj-gs 'cl:vector))
                         (t (islisp-sys:signal-domain-error ,obj-gs (class ,class-name))))))))

      ((<symbol>) ;; FIXME: What if in another package?
       (cond ((symbolp obj) `,obj)
             ((stringp obj) `',(cl:intern obj))
             ((characterp obj) `',(cl:intern (cl:string obj)))
             (t `(let ((,obj-gs ,obj))
                   (cond ((funcall ,symbolp ,obj-gs) ,obj-gs)
                         ((funcall ,stringp ,obj-gs) (cl:intern ,obj-gs))
                         ((funcall ,characterp ,obj-gs) (cl:intern (cl:string ,obj-gs)))
                         (t (islisp-sys:signal-domain-error ,obj-gs (class ,class-name))))))))

      ((<integer>)
       (cond ((integerp obj) obj)
             ((stringp obj) (parse-number obj))
             ((characterp obj) (cl:char-code obj))
             (t `(let ((,obj-gs ,obj))
                   (cond ((funcall ,integerp ,obj-gs) ,obj-gs)
                         ((funcall ,stringp ,obj-gs) (parse-number ,obj-gs))
                         ((funcall ,characterp ,obj-gs) (cl:char-code ,obj-gs))
                         (t (islisp-sys:signal-domain-error ,obj-gs (class ,class-name))))))))

      ((<float>)
       (cond ((floatp obj) obj)
             ((stringp obj) (parse-number obj))
             ((integerp obj) (cl:float obj))
             ((cl:rationalp obj) (cl:float obj)) ; Helper extension
             (t `(let ((,obj-gs ,obj))
                   (cond ((funcall ,floatp ,obj-gs) ,obj-gs)
                         ((funcall ,stringp ,obj-gs) (parse-number ,obj-gs))
                         ((funcall ,integerp ,obj-gs) (cl:float ,obj-gs))
                         ((cl:rationalp ,obj-gs) (cl:float ,obj-gs)) ; Helper extension
                         (t (islisp-sys:signal-domain-error ,obj-gs (class ,class-name))))))))

      ((<string>)
       (cond ((stringp obj) obj)
             ((integerp obj) (cl:format nil "~D" obj))
             ((floatp obj) (cl:format nil "~G" obj))
             ;; Special check for a quoted symbol
             ((and (consp obj)
                   (eq (car obj) 'quote)
                   (consp (cdr obj))
                   (symbolp (car (cdr obj))) (cl:symbol-name (car (cdr obj)))))
             (t `(let ((,obj-gs ,obj))
                   (cond ((funcall ,stringp ,obj-gs) ,obj-gs)
                         ((funcall ,integerp ,obj-gs) (cl:format nil "~D" ,obj-gs))
                         ((funcall ,floatp ,obj-gs) (cl:format nil "~G" ,obj-gs))
                         ((funcall ,symbolp ,obj-gs) (cl:symbol-name ,obj-gs))
                         (t (islisp-sys:signal-domain-error ,obj-gs (class ,class-name)))))))) )))

;;; Constants

(defconstant *pi* cl:pi)
(defconstant *most-positive-float* cl:most-positive-double-float)
(defconstant *most-negative-float* cl:most-negative-double-float)
