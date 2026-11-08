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
;; The page and its doors, under /tala.  The page holds no song of its
;; own: it shows the state a door answers and sends forms back through
;; /tala/api/forms, the same operations a program or the prompt's model
;; runs.  A door is behind the invite gate, which is open with no
;; invites file, as in a development room; Gendl's invite-gate is the
;; same one Stratum uses.
;;

(defvar *gate* (invite-gate:make-gate :name "Tala" :prefix "/tala" :cookie "tala_invite")
  "Tala's invite gate; its invites file is the roster.")

(defun configure-gate! (&key (invites-file nil file?) (public-base nil base?))
  "Set the roster file (nil opens the gate) and where invite links
point; the gate as it stands."
  (when file? (setf (invite-gate:gate-invites-file *gate*) invites-file))
  (when base? (setf (invite-gate:gate-public-base *gate*) public-base))
  *gate*)

(defun gate () *gate*)
(defun mint-invite! (name) (invite-gate:mint-invite! *gate* name))
(defun revoke-invite! (token) (invite-gate:revoke-invite! *gate* token))
(defun invites-report () (invite-gate:invites-report *gate*))

(defun tala-dir ()
  "Where the checkout is: the page and the reference are read from it."
  (asdf:system-source-directory :tala))

;;; JSON, written here for one reason: a parameter must reach the
;;; browser as exactly the double it is, or the two renders part ways.

(defun obj (&rest keys-and-values)
  "A JSON object from keys and values."
  (cons :obj keys-and-values))

(defun jbool (x) (if x :true :false))

(defun write-json-string (string out)
  (write-char #\" out)
  (loop for c across string
        do (case c
             (#\" (write-string "\\\"" out))
             (#\\ (write-string "\\\\" out))
             (#\Newline (write-string "\\n" out))
             (#\Return (write-string "\\r" out))
             (#\Tab (write-string "\\t" out))
             (t (if (< (char-code c) 32)
                    (format out "\\u~4,'0x" (char-code c))
                    (write-char c out)))))
  (write-char #\" out))

(defun write-json-number (x out)
  (etypecase x
    (integer (format out "~d" x))
    (ratio (write-json-number (float x 1d0) out))
    (float (let ((*read-default-float-format* 'double-float))
             (prin1 (float x 1d0) out)))))

(defun write-json (x out)
  (cond ((null x) (write-string "[]" out))
        ((eq x :null) (write-string "null" out))
        ((eq x :true) (write-string "true" out))
        ((eq x :false) (write-string "false" out))
        ((stringp x) (write-json-string x out))
        ((keywordp x) (write-json-string (string-downcase x) out))
        ((numberp x) (write-json-number x out))
        ((and (consp x) (eq (car x) :obj))
         (write-char #\{ out)
         (loop for (k v) on (cdr x) by #'cddr
               for first? = t then nil
               do (unless first? (write-char #\, out))
                  (write-json-string k out)
                  (write-char #\: out)
                  (write-json v out))
         (write-char #\} out))
        ((or (listp x) (vectorp x))
         (write-char #\[ out)
         (let ((first? t))
           (map nil (lambda (item)
                      (unless first? (write-char #\, out))
                      (setf first? nil)
                      (write-json item out))
                x))
         (write-char #\] out))
        (t (error "No JSON for ~s." x))))

(defun json-text (x)
  (with-output-to-string (out)
    (write-json x out)))

;;; The state a page shows.

(defun device-json (device place)
  (obj "id" (getf device :id)
       "kind" (getf device :kind)
       "on" (jbool (getf device :on t))
       "key" (or (getf device :key) :null)
       "param" (getf place :param)
       "pads" (if (eq (getf device :kind) :drums)
                  (loop for pad from 1 to 8 collect (or (getf device (pad-sample-key pad)) :null))
                  :null)))

(defvar *json-tempo* 120
  "The tempo of the song being written, which an audio clip's seconds a
beat depend on.")

(defun track-json (track place index)
  (obj "id" (getf track :id)
       "index" index
       "kind" (getf place :kind)
       "name" (getf track :name)
       "color" (getf track :color 0)
       "gain" (getf track :gain 0)
       "pan" (getf track :pan 0)
       "mute" (jbool (getf track :mute))
       "solo" (jbool (getf track :solo))
       "sends" (getf track :sends)
       "param" (getf place :param)
       "instrument" (if (getf track :instrument)
                        (device-json (getf track :instrument) (getf place :instrument))
                        :null)
       "devices" (mapcar #'device-json (getf track :devices) (getf place :devices))
       "clips" (mapcar (lambda (clip)
                         (obj "id" (getf clip :id)
                              "name" (getf clip :name)
                              "start" (getf clip :start)
                              "length" (getf clip :length)
                              "loop" (or (getf clip :loop) :null)
                              "notes" (getf clip :notes)
                              "sample" (or (getf clip :sample) :null)
                              "scene" (or (getf clip :scene) :null)
                              "offset" (getf clip :offset 0)
                              "gain" (getf clip :gain 0)
                              "transpose" (getf clip :transpose 0)
                              "warp" (if (getf clip :warp) (string-downcase (getf clip :warp)) :null)
                              "bpm" (or (getf clip :bpm) :null)
                              "spb" (if (getf clip :sample) (clip-seconds-per-beat clip *json-tempo*) :null)))
                       (getf track :clips))
       "automation" (mapcar (lambda (lane)
                              (obj "device" (or (getf lane :device) :null)
                                   "param" (getf lane :param)
                                   "points" (getf lane :points)))
                            (getf track :automation))))

(defun samples-json (samples)
  (let ((layout (pool-layout samples)))
    (mapcar (lambda (sample)
              (let ((row (assoc (getf sample :id) layout)))
                (obj "id" (getf sample :id)
                     "name" (getf sample :name)
                     "seconds" (sample-seconds sample)
                     "channels" (getf sample :channels)
                     "frames" (getf sample :frames)
                     "left" (second row)
                     "words" (or (getf sample :words) :null)
                     "missing" (jbool (zerop (fourth row))))))
            samples)))

(defun song-json (song)
  "All the page needs of SONG."
  (let* ((spec (the-object song spec))
         (plan (the-object song plan))
         (placed (plan-layout plan *sample-rate*))
         (tracks (getf spec :tracks))
         (returns (getf spec :returns)))
    (multiple-value-bind (js processor) (worklet-js plan *sample-rate*)
      (declare (ignore js))
      (obj "name" (getf spec :name)
           "tempo" (getf spec :tempo)
           "numerator" (getf spec :numerator)
           "denominator" (getf spec :denominator)
           "loopOn" (jbool (getf spec :loop-on))
           "loopStart" (getf spec :loop-start)
           "loopEnd" (getf spec :loop-end)
           "quantize" (getf spec :quantize 4)
           "scenes" (mapcar (lambda (s) (obj "id" (getf s :id) "name" (getf s :name)))
                            (getf spec :scenes))
           "tracks" (let ((*json-tempo* (getf spec :tempo)))
                      (loop for track in tracks
                            for place in placed
                            for i from 0
                            collect (track-json track place i)))
           "returns" (loop for track in returns
                           for place in (nthcdr (length tracks) placed)
                           for i from 0
                           collect (track-json track place i))
           "master" (track-json (getf spec :master) (first (last placed)) 0)
           "processor" processor
           "metronomeParam" gp-metronome
           "samples" (samples-json (getf spec :samples))
           "poolStamp" (fnv-hex (pool-key (getf spec :samples)))
           "params" (the-object song params)
           "score" (the-object song score)
           "undo" (length (the-object song undo-entries))
           "redo" (length (the-object song redo-entries))))))

(defun kinds-json ()
  (mapcar (lambda (d)
            (obj "key" (device-kind-key d)
                 "label" (device-kind-label d)
                 "type" (device-kind-type d)
                 "doc" (device-kind-doc d)
                 "params" (mapcar (lambda (p)
                                    (obj "key" (param-key p)
                                         "label" (param-label p)
                                         "default" (if (param-choices p)
                                                       (position (param-default p) (param-choices p))
                                                       (param-default p))
                                         "min" (or (param-min p) 0)
                                         "max" (or (param-max p) (max 0 (1- (length (param-choices p)))))
                                         "unit" (or (param-unit p) "")
                                         "log" (jbool (eq (param-scale p) :log))
                                         "choices" (param-choices p)
                                         "doc" (or (param-doc p) "")))
                                  (device-params (device-kind-key d)))))
          *device-kinds*))

;;; The doors.

(defparameter *max-body-bytes* 4000000 "The largest request a door reads.")

(defparameter *max-sample-bytes* 64000000
  "The largest sample: about five and a half minutes of mono.")

(defparameter *upload-stall-seconds* 30
  "How long a sample's upload may pause before it is given up.")

(defvar *agent*)

(defun request-session (req)
  (session-for (or (net.aserve:request-query-value "s" req :post nil) "default")))

(defun utf-8 (string)
  (flexi-streams:string-to-octets string :external-format :utf-8))

(defun send (req ent content-type body &optional (response net.aserve:*response-ok*))
  "BODY, a string or octets, as the reply."
  (net.aserve:with-http-response (req ent :content-type content-type :response response)
    (setf (net.aserve:reply-header-slot-value req :cache-control) "no-store")
    (net.aserve:with-http-body (req ent)
      (write-sequence (if (stringp body) (utf-8 body) body)
                      (net.aserve:request-reply-stream req)))))

(defun refused (req ent)
  (send req ent "text/html; charset=utf-8" (invite-gate:refusal-page *gate*)
        net.aserve:*response-forbidden*))

(defun declared-length (req)
  (let ((declared (net.aserve:header-slot-value req :content-length)))
    (and declared (ignore-errors (parse-integer declared :junk-allowed t)))))

(defun oversized? (req limit)
  (> (or (declared-length req) 0) limit))

(defun read-body-octets (req)
  "REQ's declared body straight from the socket into one octet vector:
aserve's own reader makes a string of it, at four bytes a character."
  (let* ((length (or (declared-length req) (error "The upload does not say how long it is.")))
         (out (make-array length :element-type '(unsigned-byte 8)))
         (sock (net.aserve:request-socket req))
         (used 0))
    ;; read now: the reply's keep-alive read must not read it again
    (setf (net.aserve::request-request-body req) "")
    (when (net.aserve::request-has-continue-expectation req)
      (net.aserve::send-100-continue req))
    (loop while (< used length)
          do (let ((got (glisp:with-deadline (*upload-stall-seconds*)
                          (read-sequence out sock :start used :end (min length (+ used 1048576))))))
               (when (<= got used)
                 (error "The upload ended after ~a of ~a bytes." used length))
               (setf used got)))
    out))

(defun publish-door (server path content-type fn &key (limit *max-body-bytes*))
  "FN, a function of the request answering a string or octets, at PATH:
behind the gate, what went wrong going back in words."
  (net.aserve:publish
   :path path :server server :content-type content-type
   :function (lambda (req ent)
               (cond ((not (invite-gate:invite-allowed? *gate* req)) (refused req ent))
                     ((oversized? req limit)
                      (send req ent "text/plain" "That is too large."
                            net.aserve:*response-bad-request*))
                     (t
                      (invite-gate:ensure-invite-cookie! *gate* req)
                      (let ((result nil) (failure nil))
                        (handler-case (setf result (funcall fn req))
                          (serious-condition (e)
                            (let ((*print-readably* nil))
                              (setf failure (princ-to-string e)))))
                        (if failure
                            (send req ent "text/plain; charset=utf-8" failure
                                  net.aserve:*response-internal-server-error*)
                            (send req ent content-type result))))))))

(defun web-file (name)
  (uiop:read-file-string (merge-pathnames (format nil "web/~a" name) (tala-dir))
                         :external-format :utf-8))

(defun body-field (req name)
  "The field NAME of REQ's JSON body."
  (let ((body (yason:parse (gwl:request-body-text req))))
    (and (hash-table-p body) (gethash name body))))

(defun printed (value)
  "VALUE as the page shows it, at most 400 characters."
  (let ((text (let ((*package* (find-package :tala-api))
                    (*print-case* :downcase)
                    (*print-pretty* nil)
                    (*print-readably* nil)
                    (*read-default-float-format* 'double-float))
                (if (picture-p value) (picture-note value) (prin1-to-string value)))))
    (if (> (length text) 400)
        (format nil "~a ..." (subseq text 0 400))
        text)))

(defun run-forms (session forms)
  "FORMS, a string or list, through the sandbox on SESSION's song:
(values results passed), what sexp-agent:run-forms answers."
  (bt:with-lock-held ((session-lock session))
    (let ((*song* (session-song session)))
      (with-double-floats
        (with-audio-floats
          (sexp-agent:run-forms *agent* forms))))))

(defun results-json (results)
  (mapcar (lambda (r)
            (obj "form" (let ((form (getf r :form)))
                          (if (stringp form) form (sexp-agent:form-string *agent* form)))
                 "ok" (jbool (getf r :ok))
                 "value" (if (getf r :ok) (printed (getf r :value)) :null)
                 "error" (or (getf r :error) :null)))
          results))

(defun ui-json (actions)
  "ACTIONS, as point-at and show left them, oldest first."
  (mapcar (lambda (action)
            (destructuring-bind (kind a &optional b c) action
              (ecase kind
                (:point (obj "kind" "point" "target" a "say" (or b :null)))
                (:launch (obj "kind" "launch" "track" a "row" b))
                (:scene (obj "kind" "scene" "row" a))
                (:stop (obj "kind" "stop" "track" a))
                (:arrange (obj "kind" "arrange" "track" a))
                (:show (obj "kind" "show" "view" a "track" (or b :null) "clip" (or c :null))))))
          (reverse actions)))

(defun handle-forms (req)
  "POST {forms}: the forms run; what became of them and the song's state."
  (let* ((forms (body-field req "forms"))
         (session (request-session req))
         (*ui-actions* nil))
    (unless (stringp forms)
      (error "The request has no forms."))
    (multiple-value-bind (results passed) (run-forms session forms)
      (json-text (obj "results" (results-json results)
                      "ok" (jbool passed)
                      "ui" (ui-json *ui-actions*)
                      "song" (bt:with-lock-held ((session-lock session))
                               (song-json (session-song session))))))))

(defun song-frames (song)
  "How long a bounce of SONG is: to the last clip's end, or the loop's,
and two bars for the tails to ring out."
  (let* ((spec (the-object song spec))
         (per-bar (song-beats-per-bar spec))
         (last-beat (reduce #'max
                            (loop for track in (getf spec :tracks)
                                  append (mapcar (lambda (c) (+ (getf c :start) (getf c :length)))
                                                 (getf track :clips)))
                            :initial-value (getf spec :loop-end))))
    (round (* (+ last-beat (* 2 per-bar)) (/ 60 (getf spec :tempo)) *sample-rate*))))

(defun straight-through (song)
  "SONG with its loop off: a bounce plays to the end."
  (let ((spec (copy-tree (the-object song spec))))
    (put! spec :loop-on nil)
    (make-song spec)))

(defun handle-bounce (req)
  (let* ((session (request-session req))
         (song (bt:with-lock-held ((session-lock session))
                 (straight-through (session-song session)))))
    (multiple-value-bind (left right) (render-song song (song-frames song))
      (wav-octets left right *sample-rate*))))

(defun handle-render (req)
  "GET ?frames=N&from=BEAT: float32 frames, for the browser's parity test."
  (let* ((session (request-session req))
         (frames (parse-integer (net.aserve:request-query-value "frames" req)))
         (from (let ((text (net.aserve:request-query-value "from" req)))
                 (if text (parse-integer text) 0)))
         (song (session-song session))
         (script (let ((text (net.aserve:request-query-value "script" req)))
                   (and text
                        (map 'list (lambda (e)
                                     (list (elt e 0) (intern (string-upcase (elt e 1)) :keyword)
                                           (elt e 2) (if (> (length e) 3) (elt e 3) 0)))
                             (yason:parse text))))))
    (multiple-value-bind (left right)
        (render-song song frames :from from :script script
                                 :metronome (equal "1" (net.aserve:request-query-value "metronome" req)))
      (float32-octets left right))))

(defun handle-sample (req)
  "POST the decoded samples, a channel after the other, as float32, with
?name= and ?channels=: answers the name it took and the song's state."
  (let* ((session (request-session req))
         (name (or (net.aserve:request-query-value "name" req :post nil) "Sample"))
         (channels (parse-integer (or (net.aserve:request-query-value "channels" req :post nil) "1")))
         (octets (read-body-octets req)))
    (bt:with-lock-held ((session-lock session))
      (let* ((song (session-song session))
             (taken (import-sample! song name octets channels)))
        (json-text (obj "name" taken "song" (song-json song)))))))

(defun handle-load (req)
  (let ((text (body-field req "text"))
        (session (request-session req)))
    (bt:with-lock-held ((session-lock session))
      (let ((*song* (session-song session)))
        (replace-spec! *song* (read-song-text text))
        (json-text (obj "song" (song-json *song*)))))))

(defvar *tala-publishers* nil
  "Functions of the server that publish doors of their own, for the
files loaded after this one.  Not gwl's list, which this package
inherits.")

(defun publish-tala! ()
  "Publish the page at /tala/ and its API under /tala/api/."
  (gwl:with-all-servers (server)
    (flet ((door (path type fn) (publish-door server path type fn)))
      (net.aserve:publish-prefix
       :prefix "/tala/invite/" :server server
       :function (lambda (req ent)
                   (invite-gate:invite-door
                    *gate* req ent
                    (subseq (net.uri:uri-path (net.aserve:request-uri req)) (length "/tala/invite/")))))
      (let ((page (lambda (req) (declare (ignore req)) (web-file "index.html"))))
        (door "/tala" "text/html; charset=utf-8" page)
        (door "/tala/" "text/html; charset=utf-8" page))
      (dolist (script '("core.js" "arrange.js" "detail.js" "mixer.js" "session.js" "prompt.js" "main.js"))
        (let ((script script))
          (door (format nil "/tala/~a" script) "application/javascript; charset=utf-8"
                (lambda (req) (declare (ignore req)) (web-file script)))))
      (door "/tala/app.css" "text/css; charset=utf-8"
            (lambda (req) (declare (ignore req)) (web-file "app.css")))
      (door "/tala/api/kinds" "application/json"
            (lambda (req) (declare (ignore req)) (json-text (kinds-json))))
      (door "/tala/api/state" "application/json"
            (lambda (req)
              (let ((session (request-session req)))
                (bt:with-lock-held ((session-lock session))
                  (json-text (song-json (session-song session)))))))
      (door "/tala/api/forms" "application/json" #'handle-forms)
      (door "/tala/api/engine.js" "application/javascript; charset=utf-8"
            (lambda (req)
              (or (worklet-by-name (net.aserve:request-query-value "name" req))
                  (error "There is no such engine."))))
      (door "/tala/api/bounce.wav" "audio/wav" #'handle-bounce)
      (door "/tala/api/render.f32" "application/octet-stream" #'handle-render)
      (door "/tala/api/save" "text/plain; charset=utf-8"
            (lambda (req)
              (let ((session (request-session req)))
                (bt:with-lock-held ((session-lock session))
                  (song-text (session-song session))))))
      (door "/tala/api/load" "application/json" #'handle-load)
      (publish-door server "/tala/api/sample" "application/json" #'handle-sample
                    :limit *max-sample-bytes*)
      (door "/tala/api/pool" "application/octet-stream"
            (lambda (req)
              (let ((session (request-session req)))
                (floats-octets (pool-for (getf (the-object (session-song session) spec) :samples))))))
      (dolist (publisher *tala-publishers*)
        (funcall publisher server)))))
