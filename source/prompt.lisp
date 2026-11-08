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
;; The prompt tile: what the person wants, in words, done through the
;; Lisp API by a language model, or explained.  POST /tala/api/prompt
;; {prompt} sends the model docs/api.md, every control of every device
;; (from the same table the page's tooltips come from), the song in
;; words and the request; the model answers with forms; sexp-agent
;; (Gendl's gwl/source/sexp-agent.lisp) runs them through its sandbox,
;; and the page redraws.  There are no presets behind this: a style is
;; something the model knows and the controls are something it reads,
;; so "set up for Goa" is worked out each time.
;;
;; The model cannot hear.  (hear) answers a spectrogram and measures,
;; which go back to it with its next turn, so it can check what it made
;; (hear.lisp).  (point-at) and (show) reach the page, so it can teach.
;;
;; The session's lock is held while forms run and never during the
;; model's call, which takes seconds.  A prompt is one undo.
;;

(defun api-reference ()
  "docs/api.md, the model's instructions and the person's reference."
  (let ((file (merge-pathnames "docs/api.md" (tala-dir))))
    (if (probe-file file)
        (uiop:read-file-string file :external-format :utf-8)
        (format nil "The reference document ~a is missing." file))))

(defun controls-reference ()
  "Every device and control, from the table the page itself reads."
  (with-output-to-string (out)
    (format out "~%## Devices and their controls~%")
    (dolist (d *device-kinds*)
      (format out "~%### ~(~s~) — ~a (~(~a~))~%~%~a~%~%"
              (device-kind-key d) (device-kind-label d) (device-kind-type d) (device-kind-doc d))
      (if (eq (device-kind-key d) :drums)
          ;; eight pads with the same controls: say them once
          (format out "Pads 1 to 8 start as ~{~a~^, ~}, at pitches 36 to 43, and each has these controls, as :pad1-tune, :pad2-decay and so on (the defaults shown are the kick's):~%~%~{~a~}"
                  (mapcar #'first *pad-defaults*)
                  (loop for p in (subseq (device-params :drums) 0 dp-size)
                        collect (control-line p (subseq (string-downcase (param-key p)) 5))))
          (dolist (p (device-params (device-kind-key d)))
            (write-string (control-line p (string-downcase (param-key p))) out))))))

(defun control-line (param name)
  (format nil "- `:~a` ~:[~a to ~a~@[ ~a~]~*~;one of ~*~*~*~{`:~(~a~)`~^, ~}~], default ~(~a~): ~a~%"
          name (param-choices param) (param-min param) (param-max param) (param-unit param)
          (param-choices param) (param-default param) (param-doc param)))

(defun model-reference ()
  (format nil "~a~%~a" (api-reference) (controls-reference)))

(defparameter *rules*
  "You work this music program on behalf of a person, through the operations above.  They may ask you to do something, or to explain something, or both.

To do something: reply with a sentence or two saying what you will do and why it suits the request, then exactly one fenced code block marked lisp holding the forms to evaluate, in order, and nothing after it.  Use only the operations documented above, with literal arguments: numbers, strings, keywords, t, nil and lists of those; no variables, no other functions, no comments inside the block.

To explain: answer in a few plain sentences and use point-at in the block to show the person each control you name, so they learn where it is.  When nothing should change, the block holds only point-at and show forms, or is empty.

A message describing the song comes with each request; a request that follows another continues from the song as it now is.  (hear) shows you what the song sounds like: end your block with it, the forms up to it run, the spectrogram and measures arrive in the next message, and you answer again in the same form with the forms that remain, or an empty block and a sentence when it is done.  Check your work this way when you have made or changed a sound.  The operations under \"Reading the song\" and sample-info and samples come back the same way: end the block with them and their answers arrive in the next message, each as the caption of a blank gray picture, which you can ignore.  Forms after them in the block run before you have read the answer, so ask first and act in your next block."
  "What the model is told after the reference.")

(defparameter *prompt-looks* 6
  "How many times the model may hear or ask, and answer again, in one
prompt.")

;;; What a reading operation answers must reach the model, and
;;; sexp-agent sends back only failures and pictures.  So in a prompt
;;; such an answer travels as a picture: a blank one, whose caption is
;;; the answer.

(defstruct (answer (:constructor make-answer (text)))
  "What a reading operation answered, on its way to the model."
  text)

(defparameter *answering-operators*
  (mapcar (lambda (name) (intern name :tala-api))
          '("SONG-INFO" "DEVICE-KINDS" "DEVICE-CONTROLS" "DEVICE-VALUES" "CLIP-NOTES"
            "SAMPLES" "SAMPLE-INFO"))
  "The operations a model calls for what they answer.")

(defvar *blank-png* nil "A small gray picture, made when first wanted.")

(defun blank-png ()
  (or *blank-png*
      (setf *blank-png*
            (png-octets (make-array (* 3 8 8) :element-type '(unsigned-byte 8) :initial-element 128) 8 8))))

(defun answers-for-model (results passed)
  "RESULTS with each reading operation's value made an answer, and PASSED."
  (values (mapcar (lambda (r)
                    (let ((form (getf r :form)))
                      (if (and (getf r :ok) (consp form) (member (car form) *answering-operators*))
                          (list :form form :ok t
                                :value (make-answer
                                        (format nil "~a answered: ~a"
                                                (sexp-agent:form-string *agent* form)
                                                (let ((*package* (find-package :tala-api))
                                                      (*print-case* :downcase) (*print-pretty* nil)
                                                      (*print-readably* nil)
                                                      (*read-default-float-format* 'double-float))
                                                  (prin1-to-string (getf r :value))))))
                          r)))
                  results)
          passed))

(defparameter *prompt-history-turns* 6
  "How many earlier turns on a song go with each prompt.")

(defun make-tala-agent ()
  (sexp-agent:make-agent :name "Tala" :package :tala-api
                         :operators (operators-for-prompt)
                         :reference #'model-reference
                         :rules *rules*
                         :max-tokens 8000))

;; A reload keeps where the agent was pointed: making it anew would send
;; it to the public address with no key, which fails as a bad key would.
(if (boundp '*agent*)
    (setf (sexp-agent:agent-operators *agent*) (operators-for-prompt)
          (sexp-agent:agent-rules *agent*) *rules*)
    (setf *agent* (make-tala-agent)))

(defun configure-agent! (&key (messages-url nil url?) (api-key-file nil key?)
                              (model nil model?) (effort nil effort?)
                              (session-header nil header?))
  "Where the agent reaches its model and how: MESSAGES-URL (a gate, or
the API), API-KEY-FILE (nil behind a gate), MODEL, EFFORT and the
SESSION-HEADER a gate reads.  The agent as it stands."
  (when url? (setf (sexp-agent:agent-messages-url *agent*) messages-url))
  (when key? (setf (sexp-agent:agent-api-key-file *agent*) api-key-file))
  (when model? (setf (sexp-agent:agent-model *agent*) model))
  (when effort? (setf (sexp-agent:agent-effort *agent*) effort))
  (when header? (setf (sexp-agent:agent-session-header *agent*) session-header))
  *agent*)

(defvar *prompt-log-directory* nil
  "Where each prompt's record goes, one file each; nil keeps none.  What
a person types is theirs, so keeping it is the operator's decision and
something they should be told.")

(defun configure-prompt-log! (&key directory)
  "Keep a record of each prompt under DIRECTORY, or none with nil."
  (when directory (ensure-directories-exist (uiop:ensure-directory-pathname directory)))
  (setf *prompt-log-directory* (and directory (uiop:ensure-directory-pathname directory))))

(defvar *turn-count* 0)

(defun log-turn! (turn)
  "TURN, a plist, into the log.  A log that cannot be written must not
cost anyone their prompt."
  (when *prompt-log-directory*
    (handler-case
        (multiple-value-bind (second minute hour day month year)
            (decode-universal-time (get-universal-time) 0)
          (with-open-file (out (merge-pathnames
                                (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d-~6,'0d.sexp"
                                        year month day hour minute second (incf *turn-count*))
                                *prompt-log-directory*)
                               :direction :output :external-format :utf-8 :if-exists :supersede)
            (let ((*print-pretty* t) (*print-readably* nil) (*print-case* :downcase)
                  (*package* (find-package :common-lisp)))
              (prin1 turn out)
              (terpri out))))
      (serious-condition (e)
        (let ((*print-readably* nil))
          (format *error-output* "~&[tala] prompt log: ~a~%" e))))))

(defun prompt-song (session prompt &key who)
  "PROMPT answered on SESSION's song: a plist for the page, with
:reply :results :ok :declined :stopped :ui :heard (PNG octets) :usage."
  (let* ((song (session-song session))
         (began (get-universal-time))
         (*ui-actions* nil)
         (*journal* (list nil))
         (heard '())
         state history answer)
    (bt:with-lock-held ((session-lock session))
      (let ((*song* song))
        (setf state (tala-api::describe-song)
              history (reverse (the-object song prompt-log)))))
    (unwind-protect
         (setf answer
               (sexp-agent:ask *agent* prompt
                               :state-text state
                               :session-id (format nil "tala-~a" (session-id session))
                               :history history
                               :runner (lambda (code)
                                         (multiple-value-call #'answers-for-model
                                           (run-forms session code)))
                               :looks *prompt-looks*
                               :picture-of (lambda (value)
                                             (cond ((picture-p value)
                                                    (push (picture-png value) heard)
                                                    (values (picture-png value) (picture-note value)))
                                                   ((answer-p value)
                                                    (values (blank-png) (answer-text value)))))))
      ;; however it ended, what ran comes back as one undo
      (bt:with-lock-held ((session-lock session))
        ;; its own edits, and only those: an edit made on the page while
        ;; it ran is the page's own undo
        (when (car *journal*)
          (push-undo! song (cons :group (car *journal*))))
        (when answer
          (the-object song
            (set-slot! :prompt-log
                       (let ((log (cons (cons prompt (getf answer :assistant))
                                        (the-object song prompt-log))))
                         (subseq log 0 (min (length log) *prompt-history-turns*))))))))
    (log-turn! (list :who who :session (session-id session) :prompt prompt :state state
                     :model (sexp-agent:agent-model *agent*)
                     :seconds (- (get-universal-time) began)
                     :rounds (getf answer :rounds)
                     :reply (getf answer :reply)
                     :forms (getf answer :forms)
                     :ok (getf answer :ok)))
    (list :reply (getf answer :reply)
          :results (getf answer :results)
          :ok (getf answer :ok)
          :declined (getf answer :declined)
          :stopped (and (getf answer :stopped) t)
          :ui (ui-json *ui-actions*)
          :heard (reverse heard)
          :usage (getf answer :usage))))

(defun prompt-json (session result)
  (let ((usage (getf result :usage)))
    (json-text
     (obj "reply" (or (getf result :reply) "")
          "results" (results-json (getf result :results))
          "ok" (jbool (getf result :ok))
          "declined" (jbool (getf result :declined))
          "stopped" (jbool (getf result :stopped))
          "ui" (getf result :ui)
          "heard" (mapcar #'sexp-agent:base64-encode (getf result :heard))
          "usage" (obj "input" (getf usage :input 0) "output" (getf usage :output 0))
          "song" (bt:with-lock-held ((session-lock session))
                   (song-json (session-song session)))))))

;;; The door.  A model's call takes seconds, sometimes a minute, and a
;;; proxy in front of the server cuts a request at about a hundred; so
;;; the door starts the work on a thread and answers a job, and the page
;;; asks after it.

(defvar *prompt-jobs* (make-hash-table :test 'equal)
  "Job id -> (:status :working | :done | :failed :result :error :started).")

(defvar *prompt-jobs-lock* (bt:make-lock "tala prompt jobs"))

(defparameter *prompt-job-seconds* 900
  "A finished job is forgotten this long after it started.")

(defun sweep-prompt-jobs! ()
  (let ((now (get-universal-time)) (gone '()))
    (maphash (lambda (id job)
               (when (and (not (eql (getf job :status) :working))
                          (> (- now (getf job :started)) *prompt-job-seconds*))
                 (push id gone)))
             *prompt-jobs*)
    (dolist (id gone) (remhash id *prompt-jobs*))))

(defun start-prompt-job! (session prompt who)
  "The prompt started on a thread of its own; the job's id."
  (let ((id (format nil "~36r~36r" (get-universal-time) (random (expt 36 6) (make-random-state t))))
        (started (get-universal-time)))
    (bt:with-lock-held (*prompt-jobs-lock*)
      (sweep-prompt-jobs!)
      (setf (gethash id *prompt-jobs*) (list :status :working :started started)))
    (bt:make-thread
     (lambda ()
       (let ((outcome (handler-case
                          (list :status :done
                                :result (prompt-json session (prompt-song session prompt :who who)))
                        (serious-condition (e)
                          (let ((*print-readably* nil))
                            (list :status :failed :error (princ-to-string e)))))))
         (bt:with-lock-held (*prompt-jobs-lock*)
           (setf (gethash id *prompt-jobs*) (append outcome (list :started started))))))
     :name (format nil "tala prompt ~a" id))
    id))

(defun prompt-job-text (id)
  (let ((job (bt:with-lock-held (*prompt-jobs-lock*) (gethash id *prompt-jobs*))))
    (case (getf job :status)
      (:working (json-text (obj "status" "working")))
      (:done (format nil "{\"status\":\"done\",\"result\":~a}" (getf job :result)))
      (:failed (json-text (obj "status" "failed" "error" (getf job :error))))
      (t (json-text (obj "status" "unknown"))))))

(defun handle-prompt (req)
  "POST {prompt}: starts the prompt, answering {job}.  GET ?job=ID: its state."
  (let ((job (net.aserve:request-query-value "job" req :post nil)))
    (if job
        (prompt-job-text job)
        (let ((prompt (body-field req "prompt")))
          (unless (and (stringp prompt)
                       (plusp (length (string-trim '(#\Space #\Newline #\Tab) prompt))))
            (error "The prompt is empty."))
          (json-text
           (obj "job" (start-prompt-job! (request-session req) prompt
                                         (second (invite-gate:find-invite
                                                  *gate* (invite-gate:request-token *gate* req))))))))))

(defun publish-prompt! (server)
  (publish-door server "/tala/api/prompt" "application/json" #'handle-prompt)
  (publish-door server "/tala/api/reference" "text/markdown; charset=utf-8"
                (lambda (req) (declare (ignore req)) (model-reference))))

(pushnew 'publish-prompt! *tala-publishers*)
