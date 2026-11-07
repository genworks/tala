;;;; -*- Mode: Lisp; Package: tala -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :tala)

;;
;; One definition, two runtimes, as in Stratum: a DSP function is an
;; ordinary Lisp function and the same form handed to Parenscript, so
;; the browser's audio thread and the server's offline render run one
;; program.  A JavaScript number and a double-float are the same IEEE
;; double, and everything here is built from + - * / and FLOOR, which
;; IEEE defines exactly: no Math.sin, no EXP.  So the two renders agree
;; to the last bit rather than to a tolerance, and the test suite holds
;; them to it.
;;
;; What the body keeps to: double-float arithmetic on typed buffers,
;; AREF, LET*, DOTIMES, LOOP WHILE, IF, WHEN, COND as a statement, and
;; calls of other DSP functions.  A value-producing COND or LET inside
;; an expression makes Parenscript wrap a closure, which allocates on
;; the audio thread: use nested IF in expressions.
;;
;; Two things Parenscript does that matter here:
;;
;; - It prints (* a (* b c)) as a * b * c, regrouping the arithmetic,
;;   and floats do not associate.  NORMALIZE-DSP flattens such forms
;;   before either side sees them, so both group from the left.
;; - It sets a global __PS_MV_REG on every return, for multiple values.
;;   That is an allocation per call, and an undeclared global is a
;;   ReferenceError in a worklet, which is a module.  DSP-JS strips it.
;;

(defvar *dsp-forms* nil
  "Alist of (name . defun form) for Parenscript, newest first.")

(defun normalize-dsp (form)
  "FORM with (* a (* b c)) flattened to (* a b c), and the same for +,
at every depth: the grouping Parenscript prints, made the Lisp grouping
too."
  (cond ((atom form) form)
        ((member (car form) '(* +))
         (cons (car form)
               (loop for arg in (cdr form)
                     for first? = t then nil
                     for inner = (normalize-dsp arg)
                     if (and (not first?) (consp inner) (eq (car inner) (car form)))
                       append (cdr inner)
                     else collect inner)))
        (t (let ((out '()))
             ;; by hand: the forms are proper lists, and dotted pairs
             ;; never reach here
             (dolist (x form (nreverse out))
               (push (normalize-dsp x) out))))))

(defun dsp-lisp-type (keyword)
  (ecase keyword
    (:buf '(simple-array double-float (*)))
    (:int 'fixnum)
    (:num 'double-float)))

(defmacro define-dsp (name (&rest args) returns &body body)
  "NAME as a Lisp function and a JavaScript one.  ARGS are (name type)
with type :buf, :int or :num.  RETURNS is :nil, for a function that
works through its buffers and returns nothing; :num, a double-float;
or :inline, a double-float inlined in Lisp, where a returned float
would otherwise be boxed.  A leading docstring stays on the Lisp side."
  (let* ((doc (when (and (stringp (first body)) (rest body)) (first body)))
         (forms (normalize-dsp (if doc (rest body) body)))
         (names (mapcar #'first args))
         (types (mapcar (lambda (a) (dsp-lisp-type (second a))) args))
         (result (if (eq returns :nil) 'null 'double-float)))
    `(progn
       ,@(when (eq returns :inline) `((declaim (inline ,name))))
       (declaim (ftype (function ,types ,result) ,name))
       (defun ,name ,names
         ,@(when doc (list doc))
         (declare (optimize (speed 1) (safety 1) (debug 0))
                  (ignorable ,@names)
                  ,@(mapcar (lambda (n ty) `(type ,ty ,n)) names types))
         ,@forms
         ,@(when (eq returns :nil) '(nil)))
       (setf *dsp-forms*
             (acons ',name '(defun ,name ,names ,@forms ,@(when (eq returns :nil) '(nil)))
                    (remove ',name *dsp-forms* :key #'car)))
       ',name)))

(defmacro define-dsp-macro (name args &body body)
  "A macro both sides expand alike."
  `(progn
     (defmacro ,name ,args ,@body)
     (ps:defpsmacro ,name ,args ,@body)))

(defmacro define-slots (prefix &rest names)
  "Symbol macros PREFIX-NAME for the offsets 0, 1, ... and PREFIX-SIZE
for how many there are, on both sides."
  (when (member "SIZE" names :test #'string=)
    ;; PREFIX-SIZE is the count: a slot of that name would be overwritten
    ;; by it, and read another slot's value with no complaint
    (error "A slot cannot be called SIZE: ~a." prefix))
  (flet ((slot (name) (intern (concatenate 'string (string prefix) "-" (string name)))))
    `(progn
       ,@(loop for name in names
               for i from 0
               collect `(define-symbol-macro ,(slot name) ,i)
               collect `(ps:define-ps-symbol-macro ,(slot name) ,i))
       (define-symbol-macro ,(slot 'size) ,(length names))
       (ps:define-ps-symbol-macro ,(slot 'size) ,(length names)))))

;;; Numbers crossing between the two kinds.  JavaScript has one kind;
;;; Lisp keeps an array index an integer and a sample a double.

(defmacro ifloor (x)
  "X, a double within reach of a fixnum, rounded down to an integer."
  `(values (floor (cl:the (double-float -1d15 1d15) ,x))))

(ps:defpsmacro ifloor (x) `(floor ,x))

(defmacro dfloor (x)
  "X rounded down, staying a double."
  `(float (ifloor ,x) 1d0))

(ps:defpsmacro dfloor (x) `(floor ,x))

(defmacro dfloat (x)
  "The integer X as a double."
  `(float ,x 1d0))

(ps:defpsmacro dfloat (x) x)

;; The buffers go by the same names in every function: S the state, P
;; the parameters, B the base of this device's state and PB of its
;; parameters.
(define-dsp-macro sg (slot) `(aref s ,slot))
(define-dsp-macro st (slot) `(aref s (+ b ,slot)))
(define-dsp-macro pv (slot) `(aref p (+ pb ,slot)))

(defun strip-multiple-value-register (js)
  "JS without Parenscript's __PS_MV_REG assignments."
  (let ((mark "__PS_MV_REG = [];"))
    (with-output-to-string (out)
      (let ((start 0))
        (loop
          (let ((hit (search mark js :start2 start)))
            (unless hit
              (write-string js out :start start)
              (return))
            (write-string js out :start start :end hit)
            (setf start (+ hit (length mark)))))))))

(defun ps-text (form)
  "The JavaScript of FORM, fit for the audio thread."
  (strip-multiple-value-register (ps:ps* form)))

(defun dsp-js ()
  "The JavaScript for every DSP function, in definition order."
  (with-output-to-string (out)
    (dolist (entry (reverse *dsp-forms*))
      (write-string (ps-text (cdr entry)) out)
      (terpri out))))

(defmacro with-double-floats (&body body)
  "BODY reading and printing floats as doubles: a value a person or a
model types is the value they see again, not its single-float neighbor."
  `(let ((*read-default-float-format* 'double-float))
     ,@body))

(defmacro with-audio-floats (&body body)
  "BODY with float traps off.  JavaScript has none, and a filter that
rings to a denormal or a guard that compares a NaN must behave as it
does there."
  #+sbcl `(sb-int:with-float-traps-masked (:overflow :invalid :divide-by-zero :inexact :underflow)
            ,@body)
  #+ccl `(let ((saved (ccl:get-fpu-mode)))
           (unwind-protect
                (progn
                  (ccl:set-fpu-mode :overflow nil :underflow nil :division-by-zero nil
                                    :invalid nil :inexact nil)
                  ,@body)
             (apply #'ccl:set-fpu-mode saved)))
  #-(or sbcl ccl) `(progn ,@body))
