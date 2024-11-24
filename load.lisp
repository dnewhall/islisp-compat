;;;; -*- Mode:Lisp; indent-tabs-mode:nil -*-
;;;; load.lisp
;;;; Copyright (c) 2011-2024 Derek Newhall
;;;;
;;;; Simply loads in all the files using LOAD, which is useful
;;;; in certain circumstances (like a munged ASDF registry).
;;;;   ex.  sbcl --load load.lisp
;;;;
;;;; You can set *islisp-compat-root* to a path to force the file
;;;; to load all its files from there.
;;;; If *islisp-compat-muffle-warnings* is true, all warnings during
;;;; load are muffled.

(defvar *islisp-compat-root*
  *default-pathname-defaults*)

(defvar *islisp-compat-muffle-warnings*
  t)

(load (if *islisp-compat-root* (merge-pathnames "packages.lisp" *islisp-compat-root*) "packages.lisp"))

;; Muffle warnings for the recursive definitions in the reader
;; and SBCL's defconstant complaining about *pi*, etc.
(handler-bind ((style-warning #'(lambda (condition)
                                  (declare (ignore condition))
                                  (when *islisp-compat-muffle-warnings*
                                    (muffle-warning)))))

  (load (if *islisp-compat-root* (merge-pathnames "islisp-sys.lisp" *islisp-compat-root*) "islisp-sys.lisp"))
  (load (if *islisp-compat-root* (merge-pathnames "bq.lisp" *islisp-compat-root*) "bq.lisp"))
  (load (if *islisp-compat-root* (merge-pathnames "reader.lisp" *islisp-compat-root*) "reader.lisp"))

  (load (if *islisp-compat-root* (merge-pathnames "functions.lisp" *islisp-compat-root*) "functions.lisp"))
  (load (if *islisp-compat-root* (merge-pathnames "specials.lisp" *islisp-compat-root*) "specials.lisp"))
  
  (pushnew :islisp-compat *features*)
  
) ; Muffle
