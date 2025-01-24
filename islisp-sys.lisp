;;;; -*- Mode:Lisp; indent-tabs-mode:nil -*-
;;;; islisp-sys.lisp
;;;; Copyright (c) 2011-2025 Derek Newhall
;;;; SPDX-License-Identifier: CDDL-1.1
;;;;
;;;; This contains all the implementation details required by
;;;; functions.lisp and specials.lisp.
(in-package #:islisp-sys)

;;; Set up useful errors

;; For syntax errors
(define-condition <violation> (serious-condition)
  ((form :initarg form
         :accessor violation-form)
   (description :initarg :description
                :accessor violation-description)))

(defmacro violation (form format-string &rest arguments)
  "Helper macro for signaling a syntax violation."
  `(let ((description (apply #'format nil ,format-string (list ,@arguments))))
     (error (make-condition '<violation>
                            :form ,form
                            :description description))))

(defun signal-domain-error (object expected-class)
  (let ((err (make-condition 'type-error
                             :datum object
                             :expected-type expected-class)))
    (error err)))

(defun signal-control-error ()
  (error (make-instance 'control-error)))

(defun signal-program-error (fname error-id)
  (declare (ignore fname error-id))
  (error (make-instance 'program-error)))
  ;; Per spec, program-error takes no parameters, but TISL has these slots
  #|`(error (make-instance <program-error>
                           'function fname
                           'error-id error-id))|#

(defun signal-arity-error (fname len1 len2)
  (declare (ignore len1 len2))
  (signal-program-error fname 'arity-error))

(defun assert-arity (fname args minlength &optional maxlength)
  "Signals an error if the length of args doesn't fit within
the bounds of minlegnth and maxlength."
  (unless (and (>= (length args) minlength)
               (if maxlength
                   (<= (length args) maxlength)
                   t))
      (signal-arity-error fname (length args) minlength)))

(defun islisp-keyword-p (obj)
  "Predicate for whether the object is an ISLisp keyword."
  ;; Correspond to :cl, :symbol, and :keyword-object in the reader
  (or (keywordp obj)
      (and (symbolp obj)
           (> (length (symbol-name obj)) 0)
           (char= (char (symbol-name obj) 0) #\:))))
      ;(typep obj 'islisp-sys:<keyword>)))

(defun identifierp (name)
  "Returns true if name is a valid identifier (a symbol and not reserved)."
  (and (symbolp name)
       (char/= (char (symbol-name name) 0) #\&)
       (not (islisp-keyword-p name))))

(defun assure-identifier (name form)
  "Signals a violation if NAME is not a valid identifier."
  (if (identifierp name)
      name
      (islisp-sys:violation form
                            "~A cannot be used as an identifier."
                            name)))


;;; Set up built-in classes

(defun cl-class-name (class-name)
  "Returns the CL class name for the ISLisp class name."
  (case class-name
    (islisp:<object> 'cl:t)
    (islisp:<basic-array> 'cl:array)
    (islisp:<basic-array*> 'cl:simple-array) ; ???
    (islisp:<general-array*> 'cl:simple-array)
    (islisp:<basic-vector> 'cl:vector)
    (islisp:<general-vector> 'cl:simple-vector)
    (islisp:<string> 'cl:string)
    (islisp:<character> 'cl:character)
    (islisp:<function> 'cl:function)
    (islisp:<generic-function> 'cl:generic-function)
    (islisp:<standard-generic-function> 'cl:standard-generic-function)
    (islisp:<list> 'cl:list)
    (islisp:<cons> 'cl:cons)
    (islisp:<null> 'cl:null)
    (islisp:<symbol> 'cl:symbol)
    (islisp:<number> 'cl:number)
    (islisp:<float> 'cl:float)
    (islisp:<integer> 'cl:integer)
    (islisp:<serious-condition> 'cl:serious-condition)
    (islisp:<error> 'cl:error)
    (islisp:<arithmetic-error> 'cl:arithmetic-error)
    (islisp:<division-by-zero> 'cl:division-by-zero)
    (islisp:<floating-point-overflow> 'cl:floating-point-overflow)
    (islisp:<floating-point-underflow> 'cl:floating-point-underflow)
    (islisp:<control-error> 'cl:control-error)
    (islisp:<parse-error> 'islisp:<parse-error>)
    (islisp:<program-error> 'cl:program-error)
    (islisp:<domain-error> 'cl:type-error)
    (islisp:<undefined-entity> 'cl:cell-error)
    (islisp:<unbound-variable> 'cl:unbound-variable)
    (islisp:<undefined-function> 'cl:undefined-function)
    (islisp:<simple-error> 'cl:simple-error)
    (islisp:<stream-error> 'cl:stream-error)
    (islisp:<end-of-stream> 'cl:end-of-file)
    (islisp:<built-in-class> 'cl:built-in-class)
    (islisp:<standard-class> 'cl:standard-class)
    (islisp:<class> 'cl:class) ;; NOT STANDARD
    (islisp:<standard-object> 'cl:standard-object)
    (islisp:<stream> 'cl:stream)
    (islisp:<sequence> 'cl:sequence) ;; NOT STANDARD
    (islisp:<storage-exhausted> 'cl:storage-condition)
    (t class-name)))

(defun cl-class (class-name)
  "Returns the CL class for the ISLisp class name."
  (find-class (cl-class-name class-name)))


;;; Fix various ISLisp-specific things

(defun fix-global-name (name)
  (gensym (symbol-name name)))

(defconstant +dynamic-prefix+
  '%%dynamic-)

(defun fix-dynamic-name (name &optional (package (find-package '#:islisp-user)))
  "Takes a symbol and returns a mangled name for use in ISLisp."
  (let ((dynamic-name (concatenate 'string (symbol-name +dynamic-prefix+)
                                           (symbol-name name))))
    (intern dynamic-name package)))

(defun dynamic-var-p (sym)
  "Predicate for whether a symbol is an ISLisp dynamic variable."
  ;; This check used to be more complicated...
  (let ((name (symbol-name sym))
        (dynamic (symbol-name +dynamic-prefix+)))
    (and (> (length name) (length dynamic))
         (string= name dynamic :start1 0 :end1 (length dynamic))
         (boundp sym))))

(defun param-eq (param symbol)
  "Compares two lambda list parameter names.
Does a string comparison instead of EQ to get around package issues."
  (string-equal (symbol-name param)
                (symbol-name symbol)))

(defun fix-lambda-list (lambda-list)
  "Takes an ISLisp lambda list and returns one usable by CL."
  (mapcar (lambda (param)
            (cond ((or (eq param 'cl::&rest)
                       (eq param 'islisp::&rest)
                       (eq param :rest))
                    'cl::&rest)
                  ((or (param-eq param '&optional)
                       (param-eq param '&body)
                       (param-eq param '&key)
                       (param-eq param '&aux)
                       (param-eq param '&whole))
                    (cl:warn "Suspicious parameter in lambda list: ~A"
                             param)
                    param)
                  ((keywordp param)
                    (cl:warn "Keyword parameters are implementation dependent: ~A"
                             param)
                    param)
                  (t param)))
          lambda-list))

(defun fix-method-parameter-profile (parameter-profile)
  "Takes a list of ISLisp method parameters and returns
one usable for CL's DEFMETHOD."
  (mapcar #'(lambda (param)
              ;; TODO: assure-arity 2
              (if (consp param)
                  (list (first param) (cl-class-name (second param)))
                  param))
          parameter-profile))

(defun fix-slot-spec (slot-spec)
  "Takes a slot specification and returns a cons of
of (valid-cl-slot-opts . other-defining-forms)
for use by DEFCLASS."
  (if (identifierp slot-spec)
      (cons slot-spec nil)
      (let ((opts '())
            (defs '()))
        ;;(assert (consp spec))
        ;; FIXME: signal-domain-error
        (let ((slot-name (car slot-spec))
              (slot-opts (cdr slot-spec))
              (opt nil)) ; flips between the key and value
          (dolist (slot-opt slot-opts)
            (if (null opt)
                (setq opt slot-opt)
                (progn
                  (ecase opt
                    ;; Standard CL ones - return as is
                    ((:reader :writer :accessor :initarg :initform)
                     (push opt opts)
                     (push slot-opt opts)
                     (setq opt nil))
                    ;; Create new method for :boundp
                    ((:boundp)
                     (setq opt nil)
                     (assert (identifierp slot-opt))
                     (push `(defmethod ,slot-opt (instance)
                              (cl:slot-boundp instance ',slot-name))
                           defs))))))
          ;; Return (valid-cl-slot-opts . other-defining-forms)
	  (cons (cons slot-name (nreverse opts))
                (nreverse defs))))))

(defun quote-symbol-p (sym)
"Predicate used for testing if a form is quoted.
Depending on the reader, QUOTE could be from CL or ISLISP."
  (or (eq sym 'cl:quote)
      (eq sym 'islisp:quote)))

(defvar *format-directives*
  "~ABCDGORSTX%&"
  "The format directives allowed by ISLisp")

(defun validate-format-string (string objs)
  "When given a format control string and its arguments,
returns the string if it is a valid control string for ISLisp;
otherwise, returns a symbol denoting why it is not valid:
  :arity-error, :unknown-format-control, :bad-numeric-format-control"
  (let ((tildep nil)
        (numberp nil)
        (num-obj-dirs 0))
    (dotimes (i (length string) (if (>= (length objs) num-obj-dirs)
                                    string
                                    'arity-error))
      (let ((char (char-upcase (char string i))))
        (if (not tildep)
            ;; Check if tilde
            (if (char= char #\~)
                (setq tildep t))
            ;; If we have a ~...
            (if (digit-char-p char)
                ;; If followed by a number
                (setq numberp t)
                (progn
                  ;; See if the format directive is good
                  (if (not (position char *format-directives*))
                      (return :unknown-format-control)
                      ;; If we have a number, ensure it's only ~nR or ~nT
                      (if numberp
                          (unless (or (char= char #\R)
                                      (char= char #\T))
                            (return :bad-numeric-format-control))))
                  ;; Count the number of directives seen that use arguments
                  ;; ~nT, ~%, ~&, and ~~ don't consume an object
                  (unless (position char "~T%&")
                    (incf num-obj-dirs))
                  (setq tildep nil)
                  (setq numberp nil))))))))

(defun valid-number-p (string)
  "Predicate for whether a string represents a valid ISLisp number.
NOTE: does not trim any whitespace."
  ;; integers: [+|-] d*
  ;; floats: [+|-] d* [. d*] [e|E [+|-] d*]
  (if (or (zerop (length string))
          (not (find (char string 0) "+-0123456789")))
         nil
         (do ((decimal-seen-p nil)
              (e-seen-p nil)
              (i (if (find (char string 0) "+-")
                     1
                     0)
                 (1+ i)))
             ((>= i (length string))
              ;; Final check for trailing E
              (if (digit-char-p (char string (1- (length string))))
                  t
                  nil))
           
           (let ((char (char string i)))
             (cond ((digit-char-p char)) ; Do nothing on digits
                   ((char= char #\.) ; Decimal
                    (if decimal-seen-p
                        (return nil)
                        (setq decimal-seen-p t)))
                   ((or (char= char #\e) ; E
                        (char= char #\E))
                    (if e-seen-p
                        (return nil)
                        (progn
                          (setq e-seen-p t)
                          ;; Skip any + or - following the E
                          (if (and (> (length string) (+ i 2))
                                   (find (char string (1+ i)) "-+"))
                              (incf i)))))
                   (t ; Anything else
                    (return nil)))))))

(defun validate-variable-list (bindings)
  "Validates the variable list for use in ISLisp's LET or LET*."
  ;; Make sure we have a list
  (when (not (consp bindings))
    (violation bindings
               "~S is not a valid variable list."
               bindings))
  ;; Check every definition
  (dolist (binding bindings)
    (cond ((not (consp binding))
           (violation binding
                      "~S is not a valid variable list pair."
                      binding))
          ((/= (length binding) 2)
           (violation binding
                      "Bad number of elements in variable list pair ~S."
                      binding))
          ((islisp-keyword-p (car binding))
           (violation binding
                      "Keyword ~A cannot be used as a variable name."
                      (car binding)))))
  bindings)

(defun validate-function-list (bindings)
  "Validates the variable list for use in ISLisp's FLET or LABELS."
  ;; Make sure we have a list
  (when (not (consp bindings))
    (violation bindings
               "~S is not a valid variable list."
               bindings))
  ;; Check every definition
  (dolist (binding bindings)
    (cond ((not (consp binding))
           (violation binding
                      "~S is not a valid function list defintion."
                      binding))
          ((< (length binding) 2)
           (violation binding
                      "Bad number of elements in function list definition ~S."
                      binding))
          ((islisp-keyword-p (car binding))
           (violation binding
                      "Keyword ~A cannot be used as a function name."
                      (car binding)))))
  ;; Make sure no duplicate names
  (let ((fnames (mapcar #'car bindings)))
    (dolist (fname fnames)
      (if (> (count fname fnames) 1)
          (violation bindings
                     "Function definition for ~A duplicated."
                     fname))))
  bindings)


;;; REPL helper

(defun read-new-value ()
  "Used as an :interactive argument for SIGNAL-CONDITION."
   (format t "Enter a new value: ")
   (eval (islisp-read)))


;;; Array accessors

(defun set-aref (obj array indeces)
  "Function version of (setf (aref ,array ,@indeces) ,obj)."
  ;; I've been a professional Common Lisp programmer for 20 years, and I
  ;; just now learned about ROW-MAJOR-AREF...
  (setf (row-major-aref array (apply #'array-row-major-index array indeces))
        obj))


;;; Extension utility functions

(defun host-implementation-feature ()
  #+sbcl :sbcl
  #+ccl :ccl
  #+cmu :cmu
  #+clisp :clisp
  #+lispworks :lispworks
  #+allegro :allegro
  #+abcl :abcl
  #+ecl :ecl
  #+genera :genera
  #+corman :cormanlisp
  #+mezzano :mezzano
  #+mocl :mocl
  #+mcl :mcl
  #+openmcl :openmcl
  #+mkcl :mkcl
  #+clasp :clasp
  #+scl :scl
  #+xcl :xcl
  )

(defun exit (&optional status)
  (declare (ignorable status))
  (when (null status)
    (setq status 0))
  #+sbcl (sb-ext:exit :code status)
  #+clisp (ext:exit status)
  #+ccl (ccl:quit status)
  #+cmu (ext:quit status)
  #+allegro (excl:exit status)
  #+lispworks (lispworks:quit :status status)
  )

(defvar *default-helper-functions*
  '(exit import))

(defvar *standard-helper-functions*
  '(load eval macroexpand macroexpand-1 fboundp apropos))

(defvar *imported-symbols*
  '())

(defun import (&rest symbols)
  "Imports functions from one package into ISLISP-USER. Takes any number of
symbols or lists of symbols (which it flattens).
Understands :standard-helpers to mean the list of symbols in
*standard-helper-functions*.
Passing :clear as the first argument clears (uninterns) all the imported
symbols. Any symbols passed after the :clear are then imported.
Passing :clear-all clears (uninterns) all the imported symbols, plus the
functions exit and import itself (use islisp-sys:import to add them back).
Example: (islisp-sys:import 'cl:load 'cl:eval)"
  (when symbols
    (let ((symbol-list '())
          (islisp-package (find-package :islisp))
          (package (find-package :islisp-user)))
      ;; Check for :clear and :clear-all.
      (when (or (eq (car symbols) :clear)
                (eq (car symbols) :clear-all))
        (dolist (symbol (if (eq (car symbols) :clear-all)
                            (append *default-helper-functions*
                                    *imported-symbols*)
                            *imported-symbols*))
          (unintern symbol package))
        (setq *imported-symbols* '())
        (setq symbols (cdr symbols)))
      ;; Flatten any sublists in symbols.
      (labels ((flatten (sym)
                 (cond ((consp sym)
                        (flatten (car sym))
                        (flatten (cdr sym)))
                       ((keywordp sym)
                        (ecase sym
                          (:standard-helpers
                           (flatten *default-helper-functions*)
                           (flatten *standard-helper-functions*))))
                       (t
                        (push sym symbol-list)))))
        (flatten symbols)
        (setq symbol-list (nreverse symbol-list))
        ;; Import the actual symbols.
        (dolist (symbol symbol-list)
          (when symbol
            ;; If from the :islisp or :islisp-user packages (but not
            ;; :islisp-sys!), import from :common-lisp-user instead.
            (let* ((symbol-package (symbol-package symbol))
                   (resolved-symbol (if (or (eq symbol-package package)
                                            (eq symbol-package islisp-package))
                                        (find-symbol (symbol-name symbol) :cl-user)
                                        symbol))
                   (islisp-symbol (intern (symbol-name resolved-symbol) package)))
              ;; Copy function definition.
              (setf (symbol-function islisp-symbol)
                    (symbol-function resolved-symbol))
              ;; Add to imported symbols list.
              (push symbol *imported-symbols*))))))))
