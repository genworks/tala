;;;; -*- Mode: Lisp; Package: gdl-user -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;; ci/run-tests.lisp -- the Lisp half of Tala's regression suite on a
;;;; COLD Gendl image, one that has never seen this repository.
;;;;
;;;; Run by .gitlab-ci.yml inside the published genworks/gendl images,
;;;; once on CCL and once on SBCL.  It loads Quicklisp from the image,
;;;; loads Tala through ASDF from the checkout (any WARNING while it
;;;; loads fails the job: a warm image cannot catch a file out of order
;;;; or a dependency nobody declared), then runs tests/run.lisp.  Exit
;;;; status: 0 every check passed, 1 a check failed or Tala loaded with
;;;; warnings, 2 something else went wrong.
;;;;
;;;; Locally, with TALA_DIR naming a checkout:
;;;;   cd /opt/gendl && TALA_DIR=/path/to/tala \
;;;;     ./gdl/program/gdl-ccl -n -b --load /path/to/tala/ci/run-tests.lisp

(in-package :gdl-user)

(defun ci-exit (code control &rest args)
  (finish-output *standard-output*)
  (format *error-output* "~&~?~%" control args)
  (finish-output *error-output*)
  (uiop:quit code))

;;; Quicklisp first: the QL package must exist before any form below
;;; naming it is READ.
(handler-case (load-quicklisp)
  (error (e) (ci-exit 2 "CI: could not load Quicklisp: ~a" e)))

(defparameter *tala-dir*
  (uiop:ensure-directory-pathname
   (or (uiop:getenv "TALA_DIR") (uiop:getenv "CI_PROJECT_DIR")
       (ci-exit 2 "CI: set TALA_DIR or CI_PROJECT_DIR to the tala checkout."))))

;;; The Gendl images carry these without ASDF knowing; loading them
;;; again from the dist over the live packages would only make noise.
(dolist (name '(:trivial-gray-streams :flexi-streams))
  (when (find-package name)
    (asdf:register-preloaded-system name)))

;;; Two of Gendl's gwl files, the invite gate and sexp-agent: from the
;;; image when it carries them as Tala needs them, else the files
;;; INVITE_GATE_FILE and SEXP_AGENT_FILE name (.gitlab-ci.yml fetches
;;; them from the Gendl repository).
(dolist (need '((:invite-gate "INVITE_GATE_FILE" nil)
                (:sexp-agent "SEXP_AGENT_FILE" "TRIMMED-PICTURES")))
  (destructuring-bind (package variable proof) need
    (handler-case
        (unless (and (find-package package)
                     (or (null proof)
                         (let ((symbol (find-symbol proof package)))
                           (and symbol (fboundp symbol)))))
          (load (or (uiop:getenv variable)
                    (ci-exit 2 "CI: no ~a in the image that Tala can use; set ~a." package variable))))
      (error (e) (ci-exit 2 "CI: ~a did not load: ~a" package e)))))

(handler-case
    (let ((warnings '()))
      (ql:quickload :zpng :silent t)
      (pushnew *tala-dir* asdf:*central-registry* :test #'equal)
      (handler-bind ((warning (lambda (w)
                                (unless (typep w 'style-warning)
                                  (push (princ-to-string w) warnings)))))
        (asdf:load-system :tala))
      (when warnings
        (ci-exit 1 "CI: ~d warning~:p while Tala loaded:~%~{  ~a~%~}"
                 (length warnings) (reverse warnings))))
  (error (e) (ci-exit 2 "CI: Tala did not load: ~a" e)))

(handler-case (load (merge-pathnames "tests/run.lisp" *tala-dir*))
  (error (e) (ci-exit 2 "CI: tests/run.lisp did not load: ~a" e)))

(multiple-value-bind (failed count)
    (handler-case (funcall (find-symbol "RUN-TESTS" :tala))
      (error (e) (ci-exit 2 "CI: run-tests failed outright: ~a" e)))
  (ci-exit (if failed 1 0) "CI: ~d checks, ~d failed~@[: ~{~a~^; ~}~]."
           count (length failed) failed))
