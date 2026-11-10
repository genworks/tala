;;;; -*- Mode: Lisp; Package: tala -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;; What a form may name: the operations (api.lisp) and nothing else of
;; the application's.  A plain package, so no name of gdl's reaches a
;; model's form.
(defpackage :tala-api
  (:use :cl))

(gwl:define-package :tala
    (:export #:song #:make-song #:publish-tala! #:configure-agent! #:configure-gate!
             #:configure-prompt-log! #:configure-samples! #:keep-activity! #:*admins* #:gate #:mint-invite! #:revoke-invite! #:invites-report
             #:run-forms #:prompt-song #:render-song #:wav-octets
             #:define-dsp #:dsp-js))
