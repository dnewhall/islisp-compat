;;;; -*- Mode:Lisp; indent-tabs-mode:nil -*-
;;;; islisp-compat.asd
;;;; Copyright (c) 2011-2025 Derek Newhall
;;;;
;;;; The ASDF system definition.
;;;;
;;;; To use:
;;;; (require 'asdf)
;;;; (push *default-pathname-defaults* asdf:*central-registry*)
;;;; (asdf:load-system :islisp-compat)
(in-package :asdf-user)

(asdf:defsystem :islisp-compat
  :description "A complete implementation of ISLisp running on top of Common Lisp"
  :version "1.0.0"
  :author "Derek Newhall"
  :licence "CDDL-1.1"
  :components ((:file "packages")
               (:file "islisp-sys" :depends-on ("packages"))
               (:file "bq" :depends-on ("packages"))
               (:file "reader" :depends-on ("islisp-sys" "bq"))
	       (:file "functions" :depends-on ("packages" "islisp-sys" "reader"))
  	       (:file "specials" :depends-on ("packages" "islisp-sys" "functions")))
  :perform (load-op :after (op c) (provide :islisp-compat)))
