;;;; -*- Mode:Lisp; indent-tabs-mode:nil -*-
;;;; reader.lisp
;;;; Copyright (c) 2011-2025 Derek Newhall
;;;; SPDX-License-Identifier: CDDL-1.1
;;;;
;;;; A complete ISLisp reader.
(in-package #:islisp-sys)

#|

Base forms allowed:
Lists: (...)
Conses: (a . b)
Symbols: foo
Integers: 123, +1, -2
Floats: 3.14, 123e123
Strings: "..."

Dispatch forms allowed:
Quote: #'
Characters: #\?, #\space
Vectors: #(...)
Hex integers: #xDEADBEEF
Octal integers: #o777
Binary integers: #b010010

Conditionally allowed:
Keywords: :rest
Feature conditions: #+islisp

Not allowed:
#.
#n#
#n=...
#*
#:
#<
#c(r i)
#p"..."
#nR
#s(...)

|#

(defvar *default-islisp-character-names*
  (list
   (cons "space" #\Space)
   (cons "newline" #\Newline)
   )
  "Default allowed character names in the ISLisp standard. An alist of the form (string . character).")

(defvar *default-common-lisp-character-names*
  (list
   (cons "space" #\Space)
   (cons "newline" #\Newline)
   (cons "tab" #\Tab)
   (cons "return" #\Return)
   (cons "rubout" #\Rubout)
   (cons "page" #\Page)
   (cons "backspace" #\Backspace)
   (cons "linefeed" #\Linefeed)
   )
  "Default allowed character names in the Common Lisp standard. An alist of the form (string . character).")

(defvar *default-features*
  (list :islisp
        :islisp-compat
        (host-implementation-feature)))

;; Abandoned experiment in representing keywords as another class
;(defclass <keyword> ()
;  ((name :initarg :name
;         :type string
;         :accessor keyword-name)))

(defstruct (islisp-reader-options (:conc-name reader-options-))
  "Structure to hold all the options used by the reader."
  (eos-error-p t)
  (eos-value nil)
  (neutral-alphabetic-case :upcase :type keyword) ; one of: upcase, :downcase
  (package (find-package "ISLISP-USER") :type package)
  (feature-conditionals-p t)
  (features *default-features* :type list)
  (quote-symbol 'islisp:quote :type symbol)
  (keyword-behavior :cl :type keyword) ; one of: :cl, :symbol
  (symbol-colon-behavior :cl :type keyword) ; one of: :cl, :symbol, :error
  (allowed-character-names *default-islisp-character-names* :type list)
  ;(irregular-symbols-p nil) ; one of: nil, :obvious-symbols, t
  ;; These are used internally
  (line-num 1 :type integer)
  )

(defvar *default-reader-options*
  (make-islisp-reader-options))

;;; Utility functions

(defvar *whitespace-chars*
  (coerce '(#\Space #\Tab #\Return #\Newline) 'string))

(defun whitespace-char-p (char)
  (find char *whitespace-chars*))

(defun %reader-error (options msg &rest args)
  (apply #'error
         (concatenate 'string "Reader error on line ~D: "
                               msg)
         (reader-options-line-num options)
         args))

(defun make-adjustable-string (&rest things)
  "Create a string that can be adjusted."
  (let ((str (make-array 16 :element-type 'character
                            :adjustable t
                            :fill-pointer 0)))
    (dolist (thing things)
      (etypecase thing
        (character
         (vector-push-extend thing str))
        (string
         (dotimes (i (length thing))
           (vector-push-extend (char thing i) str)))
        (cons
         (dolist (char thing)
           (vector-push-extend char str)))))
    str))

(defmacro push-char (ch place)
  "Extends an adjustable string."
  `(vector-push-extend ,ch ,place))

(defun skip-whitespace (stream options)
  "Skip any whitespace in the stream.
Whitespace is defined as space, tab, newline, and return."
  (do ((ch (peek-char nil stream nil nil)
           (peek-char nil stream nil nil)))
      ((or (null ch)
           (not (whitespace-char-p ch))))
    ;; Read the character, but increment the line number if newline.
    (when (eql (read-char stream) #\Linefeed)
      (incf (reader-options-line-num options)))))

(defun skip-block-comment (stream options)
  "Skips all characters until the next |#. Called after #|."
  (do ((ch (read-char stream nil nil)
           (read-char stream nil nil))
       (nesting-depth 1)
       (vertical-bar-seen-p nil)
       (sharp-seen-p nil))
      ((null ch)
       (%reader-error options "End of stream before closing |#."))
    (case ch
      (#\Linefeed
       (incf (reader-options-line-num options))
       (setq vertical-bar-seen-p nil)
       (setq sharp-seen-p nil))
      (#\|
       (if sharp-seen-p
           (incf nesting-depth)
           (setq vertical-bar-seen-p t))
       (setq sharp-seen-p nil))
      (#\#
       (if vertical-bar-seen-p
           (decf nesting-depth)
           (setq sharp-seen-p t))
       (when (zerop nesting-depth)
         (return))
       (setq vertical-bar-seen-p nil))
      (t
       (setq vertical-bar-seen-p nil)
       (setq sharp-seen-p nil)))))

(defvar *terminating-characters*
  (coerce '(#\" #\, #\( #\) #\' #\Space #\Tab #\Return #\Newline) 'string))

(defun terminating-char-p (char)
"Returns true if the character is a terminating character."
  (find char *terminating-characters*))

(defun list-ref (forms indeces)
  "Like AREF, but for nested lists. Used for filling arrays from literals."
  (let ((a forms))
    (dolist (i indeces a)
      (setq a (elt a i)))))

;;; Individual form reading functions

(defun read-hex-integer (stream options)
  "Read an integer in hexadecimal format from the stream. Is called after #x."
  (let ((number (make-adjustable-string "#x")))
    (do ((ch (char-upcase (peek-char nil stream nil))
             (char-upcase (peek-char nil stream nil))))
        ((or (null ch)
             (terminating-char-p ch)))
      (if (find ch "0123456789ABCDEF")
          (push-char (read-char stream) number)
          (%reader-error options "Illegal hexadecimal character ~A." ch)))
    (values (read-from-string number))))

(defun read-octal-integer (stream options)
  "Read an integer in hexadecimal format from the stream. Is called after #o."
  (let ((number (make-adjustable-string "#o")))
    (do ((ch (peek-char nil stream nil)
             (peek-char nil stream nil)))
        ((or (null ch)
             (terminating-char-p ch)))
      (if (find ch "01234567")
          (push-char (read-char stream) number)
          (%reader-error options "Illegal octal character ~A." ch)))
    (values (read-from-string number))))

(defun read-binary-integer (stream options)
  "Read an integer in binary format from the stream. Is called after #b."
  (let ((number (make-adjustable-string "#b")))
    (do ((ch (peek-char nil stream nil)
             (peek-char nil stream nil)))
        ((or (null ch)
             (terminating-char-p ch)))
      (if (or (char= ch #\0) (char= ch #\1))
          (push-char (read-char stream) number)
          (%reader-error options "Illegal binary character ~A." ch)))
    (values (read-from-string number))))

(defun read-character (stream options)
  "Read a character from the stream. Is called after #\."
  (let ((ch (read-char stream)))
    (if (terminating-char-p ch)
        ch
        (let ((name (list ch)))
          (do ((ch (peek-char nil stream nil nil)
                   (peek-char nil stream nil nil)))
              ((or (null ch)
                   (terminating-char-p ch)))
            (push ch name)
            (read-char stream))
          (let ((str (coerce (nreverse name) 'string)))
            (if (= (length str) 1)
                (char str 0)
                (let* ((charnames (reader-options-allowed-character-names options))
                       (char (cdr (assoc str charnames :test #'string-equal))))
                  (if (null char)
                      (%reader-error options "Illegal or unrecognized character name: \"~A\"" str))
                      char)))))))
  
(defun read-keyword (stream options)
  "Read a keyword symbol from the stream. Is called after :."
  (let ((atom (make-adjustable-string)))
    (do ((case-fun (ecase (reader-options-neutral-alphabetic-case options)
                     ((:upcase) #'char-upcase)
                     ((:downcase) #'char-downcase)))
         (escape-next nil)
         (escape-all nil)
         (nextch (peek-char nil stream nil nil)
                 (peek-char nil stream nil nil)))
        ((null nextch)
         (assert (not escape-next)))
      
      (when (terminating-char-p nextch)
        (if escape-next
            (push (read-char stream) atom)
            (return)))
      (let ((ch (read-char stream)))
        (cond (escape-next
               (push ch atom)
               (setq escape-next nil))
              ((char= ch #\|)
               (if escape-all
                   (setq escape-all nil)
                   (setq escape-all t)))
              ((char= ch #\\)
               (setq escape-next t))
              (escape-all
               (push-char ch atom))
              ((char= ch #\:)
               (%reader-error options "Illegal : seen in keyword."))
              (t
               (push-char (funcall case-fun ch) atom)))))
    
    (ecase (reader-options-keyword-behavior options)
      ((:cl :common-lisp) (values (intern atom (find-package "KEYWORD"))))
      ;(:keyword-object (make-instance '<keyword> :name atom))
      (:symbol (values (intern atom (reader-options-package options)))))))

(defun read-list (stream options)
"Read a list from the stream. Is called after (."
  (skip-whitespace stream options)
  (do ((list '()))
      ((if (interactive-stream-p stream)
           nil
           (not (listen stream)))
       (%reader-error options "End of stream before final ')'"))
    (skip-whitespace stream options)
    (if (char= (peek-char nil stream) #\))
        (progn
          (read-char stream)
          (return (nreverse list)))
        (push (read-form stream options) list))))

(defun read-vector (stream options)
  "Read a vector from the stream. Is called after #(."
  (let ((elements (read-list stream options)))
    (coerce elements 'vector)))
      
(defun read-string (stream options)
  "Read a string from the stream. Is called after \"."
  (do ((str (make-adjustable-string))
       (escaped nil)
       (ch (read-char stream nil nil)
           (read-char stream nil nil)))
      ((null ch)
       (%reader-error options "End of stream before end of string."))
    ;; Increment line number for any embedded newlines.
    (when (char= ch #\Linefeed)
      (incf (reader-options-line-num options)))
    (cond (escaped
           (push-char ch str)
           (setq escaped nil))
          ((char= ch #\\)
           (setq escaped t))
          ((char= ch #\")
           (return str))
          (t
           (push-char ch str)))))

(defun fill-array (array dims forms options)
  "Fills an array with the values of a nested list"
  (declare (ignore options))
  (let ((indeces (make-list (length dims) :initial-element 0)))
    (dotimes (m (reduce #'* dims))
      (format t "~A~&" indeces)
      (let ((elt (list-ref forms indeces)))
        (set-aref elt array indeces)
        (dotimes (i (length dims) (return))
          (incf (elt indeces i))
          (if (< (elt indeces i) (elt dims i))
              (return)
              (setf (elt indeces i) 0))))))
  array)

(defun validate-array-elements (list n options)
  "Returns true if list is a valid for an array of N dimensions.
Can error on incorrect types."
  (if (<= n 1)
      t
      (progn
        ;; Make sure all are lists
        (dolist (sublist list)
          (when (not (consp sublist))
            (%reader-error options "Expected a list in array literal, not ~A." sublist)))
        ;; Make sure all are same length
        (let ((lens (mapcar #'length list)))
          (dolist (len (rest lens))
            (when (/= (car lens) len)
              (return-from validate-array-elements nil)))
          (dolist (sublist list)
            (when (not (validate-array-elements sublist (1- n) options))
              (return-from validate-array-elements nil)))
          t))))

(defun get-array-literal-dimensions (forms rank options)
  "Returns the dimensions represented in a list of initializing forms."
  ;; Ensure all sublists are same size
  (unless (validate-array-elements forms rank options)
    (%reader-error options "Array literal of rank ~D is malformed." rank))
  ;; Get the dimensions
  (let ((dims (make-list rank))
        (form forms))
    (dotimes (i rank dims)
      (setf (elt dims i) (length form))
      (setq form (car form)))))

(defun read-array (stream options)
  "Read an array. Called after # (but we know a digit follows)."
  (let ((n 0))
    (declare (type fixnum n))
    ;; Read number and then #\A or #\a
    (do ((digits '())
         (ch (read-char stream)
             (read-char stream)))
        ((or (char= ch #\A) (char= ch #\a))
         (assert digits)
         (let ((n0 (read-from-string (coerce (nreverse digits) 'string))))
           (assert (integerp n))
           (setq n (the fixnum n0))))
      (if (find ch "0123456789")
          (push ch digits)
          (%reader-error options "Illegal character in array literal: ~A" ch)))
    ;; Make sure a list is next
    (assert (char= (peek-char nil stream) #\())
    ;; Read the form with the correct rank
    (case n
      (0 ;; NOTE: technically not included in spec
       (make-array nil :initial-element (read-form stream options)))
      (1
       ;; Read as vector
       (read-char stream) ; Skip (
       (read-vector stream options))
      (2
       ;; Optimized version for 2D array
       (let ((forms (read-form stream options)))
         (let* ((xlen (length forms))
                (ylen (length (car forms))))
           ;; Ensure all same length
           (let ((len (length (car forms))))
             (declare (type fixnum len))
             (dolist (form (cdr forms))
               (assert (= (length form) len))))
           (let ((xi 0)
                 (yi 0)
                 (arr (make-array (list xlen ylen))))
             (declare (type fixnum xi yi))
             (dolist (xform forms)
               (dolist (yform xform)
                 (setf (aref arr xi yi) yform)
                 (incf yi))
               (incf xi)
               (setq yi 0))
             arr))))
      (otherwise
       ;; Read forms for an nD array
       (let ((forms (read-form stream options)))
         (assert (consp forms))
         (let* ((dims (get-array-literal-dimensions forms n options))
                (arr (make-array dims)))
           (fill-array arr dims forms options)
           arr))))))

(defun symbol-start-char-p (ch)
  "Test for the valid starting characters for an unescaped symbol."
  (or (alpha-char-p ch)
      ;; NOTE: & is not in the list of valid starting characters in the
      ;; standard, but is needed here to handle &rest correctly.
      (find ch "<>/*=?_!$%[]^{}~&")))

(defun read-symbol (stream options)
  "Read a symbol from the stream. Is called before any character is read. Assumes (symbol-start-char-p (preview-char stream)) is true. Can be used by itself."
  (let ((atom (make-adjustable-string))
        (colon-seen nil)) ; Index of the colon in atom

    (do ((case-fun (ecase (reader-options-neutral-alphabetic-case options)
                     ((:upcase) #'char-upcase)
                     ((:downcase) #'char-downcase)))
         (escape-next nil)
         (escape-all nil)
         (nextch (peek-char nil stream nil nil)
                 (peek-char nil stream nil nil))
         (i 0
            (1+ i)))
        ((null nextch)
         (assert (not escape-next)))

      (when (terminating-char-p nextch)
        (if escape-next
            (progn
              (push-char (read-char stream) atom)
              (setq escape-next nil))
            (return)))

      (let ((ch (read-char stream)))
        (cond (escape-next
               (push-char ch atom)
               (setq escape-next nil))
              ((char= ch #\|)
               (if escape-all
                   (setq escape-all nil)
                   (setq escape-all t)))
              ((char= ch #\\)
               (setq escape-next t))
              (escape-all
               (push-char ch atom))
              ((char= ch #\:)
               (if escape-all
                   (push-char ch atom)
                   (ecase (reader-options-symbol-colon-behavior options)
                     ((:cl :common-lisp :package)
                      (if colon-seen
                          (%reader-error options "Too many colons in token ~A" (coerce atom 'string))
                          (setq colon-seen i)))
                     ((:symbol)
                      (push-char ch atom))
                     ((:error)
                      (%reader-error options "Internal colon is considered an illegal symbol character.")))))
              ;; Normal characters
              (t
               (push-char (funcall case-fun ch) atom)))))

    ;; Parse any package and intern.
    (let ((package (if colon-seen
                       (let ((package-name (subseq atom 0 colon-seen)))
                         ;; First check the current string to handle escapes and such,
                         ;; then upcase the package name and try again (for "COMMON-LISP", etc.)
                         (or (find-package package-name)
                             (find-package (string-upcase package-name))))
                       (find-package (reader-options-package options))))
          (name (if colon-seen
                    (subseq atom colon-seen)
                    atom)))
      (if colon-seen
          ;; See if symbol exists in package, then intern.
          (multiple-value-bind (sym foundp) (find-symbol name package)
            (if foundp
                sym
                (values (intern name package))))
          ;; Just intern normal symbol name.
          (values (intern name package))))))

(defun read-number (stream options)
  "Read an atom (symbol, integer, float) from the stream. Is called before any character is read. Can be used by itself."
  ;; [s]dddd[.dddd][e|E[s]dd]
  (let* ((number (make-adjustable-string)))
    ;; Get and handle first character
    (let ((first-char (read-char stream nil nil)))
      (if (or (digit-char-p first-char)
              (char= first-char #\+)
              (char= first-char #\-))
          (push-char first-char number)
          (error "Illegal number character ~A." first-char)))
    ;; Handle additional characters
    (do ((leading-sign-p nil)
         (decimal-seen-p nil)
         (e-seen nil)
         (e-sign-p nil)
         (symbolp nil)
         (char (peek-char nil stream nil nil)
               (peek-char nil stream nil nil)))
        ((or (null char)
             (terminating-char-p char))
         ;; If "+" or "-", return the symbol from the ISLISP package.
         (cond ((or (equal number "+")
                    (equal number "-"))
                (values (intern number '#:islisp)))
               ;; If "1+" or "1+", intern the symbol in ISLISP-USER.
               ((or (equal number "1+")
                    (equal number "1-"))
                (values (intern number (reader-options-package options))))
               ;; Otherwise, use CL reader to read the number.
               (t
                (values (read-from-string number)))))
      ;; Read next char to advance the stream since we know
      ;; it's non-terminating / going to be used.
      (read-char stream)
      ;; Increment e-seen if it's 0 (i.e. set last loop).
      (if (eql e-seen 0)
          (incf e-seen))
      ;; symbolp should only be set on last char of "1+" or "1-"
      (if symbolp
          (%reader-error options "Misplaced ~A in number." (char number (1- (length number)))))
      ;; Check the character
      (cond ((digit-char-p char)
             (push-char char number))
            ((char= char #\.)
             (if decimal-seen-p
                 (%reader-error options "Illegal second decimal point in number.")
                 (progn
                   (push-char char number)
                   (setq decimal-seen-p t))))
            ((or (char= char #\e)
                 (char= char #\E))
             (if e-seen
                 (%reader-error options "Illegal second E in number.")
                 (progn
                   (push-char char number)
                   (setq e-seen 0))))
            ((or (char= char #\+)
                 (char= char #\-))
             ;; e-seen is 1 if it was set last iteration
             (cond ((eql e-seen 1)
                    (push-char char number)
                    (incf e-seen))
                   ;; Add it if possibly "1+" and "1-"
                   ((and (= (length number) 1)
                         (char= (char number 0) #\1))
                    (push-char char number)
                    (setq symbolp t))
                   ((> e-seen 1)
                    (%reader-error options "Illegal second E in number."))))
            (t
             (%reader-error options "Illegal number character ~A." char))))))

(defun read-atom (stream options)
    "Read an atom (symbol, integer, float) from the stream. Is called before any character is read. Can be used by itself."
  (let ((first-char (peek-char nil stream nil nil)))
    (cond ((symbol-start-char-p first-char)
           (read-symbol stream options))
          ((or (digit-char-p first-char)
               (char= first-char #\+)
               (char= first-char #\-))
           (read-number stream options))
          (t
           (%reader-error options "Don't know how to handle character ~C." first-char)))))

(defun eval-conditional-expr (expr options)
  "Evaluates the expression part of a conditional form."
  (let ((features (reader-options-features options)))
    (etypecase expr
      ;; (:and|:or|:not ...)
      (cons
       (ecase (car expr)
         ((:and)
          (dolist (subexpr (rest expr) t)
            (unless (eval-conditional-expr subexpr options)
              (return nil))))
         ((:or)
          (dolist (subexpr (rest expr) nil)
            (when (eval-conditional-expr subexpr options)
              (return t))))
         ((:not)
          (assert (null (cddr expr)))
          (not (eval-conditional-expr (second expr) options)))))
      ;; Keyword
      (symbol
       (and (member expr features) t)))))

(defun read-conditional-expr (stream options)
  "Reads the expression part of a conditional form."
  ;; form ::= keyword | (or|and|not form)
  (skip-whitespace stream options)
  (let ((nextch (peek-char nil stream nil nil)))
    (cond ((null nextch)
           (%reader-error options "End of stream before conditional expression."))
          ;; Read in list
          ((char= nextch #\()
           (do ((form '())
                (nextch (peek-char nil stream nil nil)
                        (peek-char nil stream nil nil)))
               ((null nextch)
                (%reader-error options "End of stream before conditional expression."))
             (read-char stream) ; Advance the stream
             (if (char= nextch #\))
                 (return (nreverse form))
                 (push (read-conditional-expr stream options) form))))
          ;; Disallow numbers
          ((or (digit-char-p nextch)
               (char= nextch #\+)
               (char= nextch #\-))
           (%reader-error options "Numbers cannot be used in conditional expression."))
          ;; Symbols are read in as keywords
          ((or (symbol-start-char-p nextch)
               (char= nextch #\\)
               (char= nextch #\|))
           (read-keyword stream options))
          (t
           (%reader-error options "Illegal character used in conditional expression.")))))

(defun read-conditional (stream options)
  "Reads a conditional form (#+/#-).
Returns two values, the form and whether the form was read in or not.
Is called after # (but we know a + or - follows)."
  (let ((cond-type (read-char stream)) ; #\+ or #\-
        (cond-expr (read-conditional-expr stream options))
        (form (read-form stream options)))
    ;(assert (find cond-type "+-"))
    (if (eval-conditional-expr (if (char= cond-type #\-)
                                   (cons 'not cond-expr)
                                   cond-expr)
                               options)
        (values form t)
        (values nil nil))))

(defun read-form (stream &optional (options *default-reader-options*))
  "Read any Lisp form from the stream. Primary function that drives the reader."
  (declare (optimize (safety 0) (speed 3)))
  (skip-whitespace stream options)
  ;; Need the loop for comments
  (do* ((eos-value (reader-options-eos-value options))
        (eos-error-p (reader-options-eos-error-p options))
        (ch (peek-char nil stream nil nil)
            (peek-char nil stream nil nil)))
      (nil)
    (case ch
      ;; End of stream
      ((nil)
       (if eos-error-p
           ;(error 'end-of-file)
           (%reader-error options "End of file reached.")
           (return eos-value)))
      ((#\;)
       (read-line stream nil nil)
       (skip-whitespace stream options))
      ;; Dispatch characters
      ((#\#)
       (read-char stream)
       ;; Need to peek to keep rank for array literals
       (let ((dispatch-char (peek-char nil stream)))
         (case dispatch-char
           ((#\+ #\-)
            (if (reader-options-feature-conditionals-p options)
                (destructuring-bind (result readp)
                    (read-conditional stream options)
                  (when readp
                    (return result)))
                (%reader-error options "Reader conditionals are not enabled.")))
           ((#\')
            (read-char stream)
            (let ((fun-name (read-atom stream options)))
              (assert (symbolp fun-name))
              (return (list 'function fun-name))))
           (#\\
            (read-char stream)
            (return (read-character stream options)))
           ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
            (return (read-array stream options)))
           ((#\x #\X)
            (read-char stream)
            (return (read-hex-integer stream options)))
           ((#\o #\O)
            (read-char stream)
            (return (read-octal-integer stream options)))
           ((#\b #\B)
            (read-char stream)
            (return (read-binary-integer stream options)))
           (#\(
            (read-char stream)
            (return (read-vector stream options)))
           (#\|
            (read-char stream) ; Must remove starting |
            (skip-block-comment stream options))
           (t
            (%reader-error options "No dispatch character defined for ~S." dispatch-char)))))
      ((#\")
       (read-char stream)
       (return (read-string stream options)))
      ((#\()
         (read-char stream)
         (return (read-list stream options)))
      ((#\:)
       (read-char stream)
       (return (read-keyword stream options)))
      ((#\,)
       (read-char stream)
       (let ((next-char (peek-char nil stream)))
         ;; Check for @
         (if (char= next-char #\@)
             (progn
               (read-char stream)
               (return (list 'islisp:unquote-splicing (read-form stream options))))
             (return (list 'islisp:unquote (read-form stream options))))))
      ((#\`)
       (read-char stream)
       (let ((form (read-form stream options)))
         (return (list 'islisp:quasiquote form))))
      ((#\')
       (read-char stream)
       (let ((quote-symbol (reader-options-quote-symbol options)))
         (return (list quote-symbol (read-form stream options)))))
      ((#\))
       (%reader-error options "Misplaced ')'."))
      (t
       (return (read-atom stream options))))))

(defun islisp-read (&optional (stream *standard-input*)
                              (eos-error-p t)
                              (eos-value nil)
                              (options nil))
  "Main entry point to the ISLisp reader. Same arguments as standard READ, plus an optional ISLISP-READER object."
  (let ((options (or options
                     (copy-islisp-reader-options *default-reader-options*))))
    ;; Overwrite options based on parameters
    (setf (reader-options-eos-error-p options) eos-error-p)
    (setf (reader-options-eos-value options) eos-value)
    ;; Reset line count
    (setf (reader-options-line-num options) 1)
    (read-form stream options)))

(defun islisp-read-from-string (string &optional (eos-error-p t)
                                       (eos-value nil))
  "Utility function that's like READ-FROM-STRING, but for the ISLisp reader."
  (let ((string-stream (make-string-input-stream string)))
    (islisp-read string-stream eos-error-p eos-value)))
