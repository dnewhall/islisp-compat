;;;; -*- Mode:Lisp; indent-tabs-mode:nil -*-
;;;; bq.lisp
;;;;
;;;; The following quasiquote support adapted from the public domain
;;;; code in Guy Steele's "Common Lisp: The Language", 2nd ed.
;;;; https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node367.html
;;;;
;;;; Correspondingly, this file is in the public domain.
(in-package #:islisp-sys)

(defvar *bq-list* (make-symbol "BQ-LIST"))
(defvar *bq-append* (make-symbol "BQ-APPEND"))
(defvar *bq-list** (make-symbol "BQ-LIST*"))
(defvar *bq-quote* (make-symbol "BQ-QUOTE"))
(defvar *bq-quote-nil* (list *bq-quote* nil))

(defun bracket (x)
  (cond ((atom x)
         (list *bq-list* (bq-process x)))
        ((eq (car x) 'islisp:unquote)
         (list *bq-list* (second x)))
        ((eq (car x) 'islisp:unquote-splicing)
         (second x))
        (t
         (list *bq-list* (bq-process x)))))

(defun bq-process (x)
  (cond ((atom x)
         (list *bq-quote* x))
        ((eq (car x) 'quasiquote)
         (bq-process (bq-completely-process (second x))))
        ((eq (car x) 'islisp:unquote)
         (second x))
        ((eq (car x) 'islisp:unquote-splicing)
         (error "Malformed quasiquote expression - `,@~S" (second x)))
        (t (do ((p x
                   (cdr p))
                (q '()
                   (cons (bracket (car p)) q)))
               ((atom p)
                (cons *bq-append*
                      (nreconc q (list (list *bq-quote* p)))))
             (when (eq (car p) 'islisp:unquote)
               (unless (null (cddr p))
                 (error "Malformed unquote expression - ,~S" p))
               (return (cons *bq-append*
                             (nreconc q (list (cadr p))))))
             (when (eq (car p) 'islisp:unquote-splicing)
               (error "Malformed unquote expression - ,@~S" p))))))

(defun maptree (fn x)
  (if (atom x)
      (funcall fn x)
      (let ((a (funcall fn (car x)))
            (d (maptree fn (cdr x))))
        (if (and (eql a (car x)) (eql d (cdr x)))
            x
            (cons a d)))))

(defun bq-simplify (x)
  (if (atom x)
      x
      (let ((x (if (eq (car x) *bq-quote*)
                   x
                   (maptree #'bq-simplify x))))
        (if (not (eq (car x) *bq-append*))
            x
            (bq-simplify-args x)))))

(defun null-or-quoted (x)
  (or (null x)
      (and (consp x)
           (eq (car x) *bq-quote*))))

(defun unquote-form-p (x)
  (and (consp x)
       (or (eq (car x) 'islisp:unquote)
           (eq (car x) 'islisp:unquote-splicing))))

(defun unquote-splicing-form-p (x)
  (and (consp x)
       (eq (car x) 'islisp:unquote-splicing)))

(defun attach-append (op item result)
  (cond ((and (null-or-quoted item)
              (null-or-quoted result))
         (list *bq-quote* (append (cadr item) (cadr result))))
        ((or (null result) (equal result *bq-quote-nil*))
         (if (unquote-splicing-form-p item) (list op item) item))
        ((and (consp result) (eq (car result) op))
         (list* (car result) item (cdr result)))
        (t (list op item result))))

(defun attach-conses (items result)
  (cond ((and (every #'null-or-quoted items)
              (null-or-quoted result))
         (list *bq-quote*
               (append (mapcar #'cadr items) (cadr result))))
        ((or (null result) (equal result *bq-quote-nil*))
         (cons *bq-list* items))
        ((and (consp result)
              (or (eq (car result) *bq-list*)
                  (eq (car result) *bq-list**)))
         (cons (car result) (append items (cdr result))))
        (t (cons *bq-list** (append items (list result))))))

(defun bq-simplify-args (x)
  (do ((args (reverse (cdr x)) (cdr args))
       (result nil
               (cond ((atom (car args))
                      (attach-append *bq-append* (car args) result))
                     ((and (eq (caar args) *bq-list*)
                           (notany #'unquote-splicing-form-p (cdar args)))
                      (attach-conses (cdar args) result))
                     ((and (eq (caar args) *bq-list**)
                           (notany #'unquote-splicing-form-p (cdar args)))
                      (attach-conses
                       (reverse (cdr (reverse (cdar args))))
                       (attach-append *bq-append*
                                      (car (last (car args)))
                                      result)))
                     ((and (eq (caar args) *bq-quote*)
                           (consp (cadar args))
                           (not (unquote-form-p (cadar args)))
                           (null (cddar args)))
                      (attach-conses (list (list *bq-quote*
                                                 (caadar args)))
                                     result))
                     (t (attach-append *bq-append*
                                       (car args)
                                       result)))))
      ((null args) result)))

(defun remove-tokens (x)
  (cond ((eq x *bq-list*) 'list)
        ((eq x *bq-append*) 'append)
        ((eq x *bq-list**) 'list*)
        ((eq x *bq-quote*) 'quote)
        ((atom x) x)
        ((and (eq (car x) *bq-list**)
              (consp (cddr x))
              (null (cdddr x)))
         (cons 'cons (maptree #'remove-tokens (cdr x))))
        (t (maptree #'remove-tokens x))))

(defun bq-completely-process (x)
  (let ((raw-result (bq-process x)))
    (remove-tokens (bq-simplify raw-result))))
