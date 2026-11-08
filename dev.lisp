;;;; -*- Mode: Lisp; Package: cl-user -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;
;;; Load Tala into a running Gendl image and publish it:
;;;
;;;   (load "/projects/gw/tala/dev.lisp")
;;;
;;; then browse /tala/ on that image's HTTP port.  zpng and salza2 come
;;; from the genworks/quicklisp checkout when the image does not carry
;;; them.  A host that must set the invite gate before the doors open
;;; binds cl-user::*tala-publish* to nil first and publishes itself.
;;;

(in-package :cl-user)

(defvar *tala-publish* t
  "Whether loading this file publishes the doors.")

(let ((software #p"/projects/gw/quicklisp/dists/quicklisp/software/"))
  (dolist (pattern '("zpng-*/" "salza2-*/"))
    (dolist (dir (directory (merge-pathnames pattern software)))
      (pushnew dir asdf:*central-registry* :test #'equal))))

;; Built into the Gendl images without ASDF knowing: say so, or salza2
;; and tala go looking for them.
(dolist (name '(:trivial-gray-streams :flexi-streams))
  (when (and (find-package name) (not (asdf:find-system name nil)))
    (asdf:register-preloaded-system name)))

(pushnew (make-pathname :name nil :type nil :defaults *load-truename*)
         asdf:*central-registry* :test #'equal)

;; Two of Gendl's gwl files an older image does not carry: the invite
;; gate and sexp-agent.  From the image when it has them, else the file
;; from a Gendl checkout beside this one, or wherever INVITE_GATE_FILE
;; and SEXP_AGENT_FILE point.
(dolist (need '((:invite-gate "INVITE_GATE_FILE" "invite-gate" nil)
                (:sexp-agent "SEXP_AGENT_FILE" "sexp-agent" "TRIMMED-PICTURES")))
  (destructuring-bind (package variable name proof) need
    (unless (and (find-package package)
                 (or (null proof)
                     (let ((symbol (find-symbol proof package)))
                       (and symbol (fboundp symbol)))))
      (let ((file (or (uiop:getenv variable)
                      (probe-file (format nil "/projects/gw/gendl/gwl/source/~a.lisp" name))
                      (probe-file (merge-pathnames (format nil "../gendl/gwl/source/~a.lisp" name) *load-truename*)))))
        (cond (file (load file))
              ((find-package package))
              (t (error "Tala needs ~a (gendl gwl/source/~a.lisp): set ~a." package name variable)))))))

(asdf:load-system :tala)

;; Samples outlast a restart where the workspace has a place for them.
(when (probe-file "/projects/.state/")
  (funcall (intern "CONFIGURE-SAMPLES!" :tala) :directory "/projects/.state/tala-samples/"))

(when *tala-publish*
  (funcall (intern "PUBLISH-TALA!" :tala)))
