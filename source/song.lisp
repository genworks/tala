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
;; The song.  What a song IS is one s-expression, its SPEC: the file,
;; what undo goes back to, what the page and the model are shown.  What
;; is worked out from it is a define-object tree -- song, tracks,
;; devices, clips -- whose computed slots are the plan, the parameters
;; and the score, each demanded only when something reads it.  An edit
;; makes a new spec and sets one slot; the tree is remade from it, and
;; the compiled program survives as long as the plan comes out the
;; same.  So there is no state to carry across a structural change, the
;; trap in a tree with settable slots on its children.
;;
;; A spec:
;;
;; (:name "..." :tempo 145 :numerator 4 :denominator 4 :loop-on t
;;  :loop-start 0 :loop-end 16 :next-id 12
;;  :tracks (track...) :returns (track...) :master track)
;;
;; a track:
;;
;; (:id 1 :name "Bass" :color 3 :gain -6 :pan 0 :mute nil :solo nil
;;  :sends (-60 -60) :instrument device :devices (device...)
;;  :clips (clip...) :automation (lane...))
;;
;; a device: (:id 2 :kind :synth :on t :key nil :values (:cutoff 400 ...)),
;; with only the values that differ from the control's default;
;; a clip: (:id 3 :name "Bass" :start 0 :length 16 :loop 4 :notes
;; ((pitch start length velocity)...)), beats throughout, the notes
;; repeating every LOOP beats;
;; an audio clip has :sample id :offset seconds :gain dB :transpose
;; semitones in place of notes, on a track whose instrument is :sampler;
;; the song's :samples are (:id :name :stamp :frames :channels :words);
;; its :scenes are the session's rows, (:id :name), and a clip with
;; :scene id sits in that row's slot instead of on the timeline, played
;; from its own start when launched; :quantize is the launch grid, beats;
;; a lane: (:device 2 :param :cutoff :points ((beat value)...)), with
;; :device nil and :param :gain or :pan for the track's own fader.
;;

(defun empty-spec (&optional (name "Untitled"))
  "A new song: nothing but a limiter on the master."
  (list :name name :tempo 120 :numerator 4 :denominator 4
        :loop-on t :loop-start 0 :loop-end 16 :quantize 4 :next-id 7
        :scenes (list (list :id 3 :name "1") (list :id 4 :name "2")
                      (list :id 5 :name "3") (list :id 6 :name "4"))
        :samples nil :tracks nil :returns nil
        :master (list :id 1 :name "Master" :color 0 :gain 0 :pan 0 :devices
                      (list (list :id 2 :kind :limiter :on t :values nil)))))

;;; Values.

(defun find-param (kind key)
  (or (find key (device-params kind) :key #'param-key)
      (error "~a has no control ~(~s~).  Its controls are: ~{~(~s~)~^ ~}."
             (device-kind-label (device-kind kind)) key
             (mapcar #'param-key (device-params kind)))))

(defun param-number (param value)
  "VALUE as the engine reads it: a choice's position, a number as a double."
  (if (param-choices param)
      (float (or (position value (param-choices param))
                 (error "~(~s~) is not a choice of ~(~s~).  It takes: ~{~(~s~)~^ ~}."
                        value (param-key param) (param-choices param)))
             1d0)
      (float value 1d0)))

(defun checked-value (param value)
  "VALUE for PARAM, or an error that says what it takes.  A number past
the control's range is held at the end, as a knob is."
  (cond ((param-choices param) (param-number param value) value)
        ((realp value) (max (param-min param) (min (param-max param) value)))
        (t (error "~(~s~) takes a number from ~a to ~a~@[ ~a~], not ~s."
                  (param-key param) (param-min param) (param-max param) (param-unit param) value))))

(defun device-value (spec key)
  "The value of control KEY in the device SPEC."
  (let ((param (find-param (getf spec :kind) key)))
    (getf (getf spec :values) key (param-default param))))

;;; The tree.

(define-object device (base-object)
  :input-slots
  (spec)

  :computed-slots
  ((kind (getf (the spec) :kind))
   (device-id (getf (the spec) :id))
   (on? (getf (the spec) :on t))
   (numbers (mapcar (lambda (param)
                      (param-number param (device-value (the spec) (param-key param))))
                    (device-params (the kind))))))

(define-object clip (base-object)
  :input-slots
  (spec)

  :computed-slots
  ((clip-id (getf (the spec) :id))
   (start (getf (the spec) :start))
   (clip-length (getf (the spec) :length))
   (repeat (let ((loop-length (getf (the spec) :loop)))
             (if (and loop-length (plusp loop-length)) loop-length (the clip-length))))
   (scene (getf (the spec) :scene))
   (relative-events
    ;; the notes as the clip plays them from its own start: (beat length
    ;; pitch velocity); an audio clip is one event with its id for a
    ;; pitch, which the score turns into its region's number
    (if (getf (the spec) :sample)
        (list (list 0 (the clip-length) (the clip-id) 127))
        (let ((out '()))
          (loop for pass from 0
                for offset = (* pass (the repeat))
                while (< offset (the clip-length))
                do (dolist (note (getf (the spec) :notes))
                     (destructuring-bind (pitch start length velocity) note
                       (let ((at (+ offset start)))
                         (when (and (< at (the clip-length)) (< start (the repeat)))
                           (push (list at (min length (- (the clip-length) at)) pitch velocity)
                                 out))))))
          (sort-events (nreverse out)))))
   (events
    ;; on the timeline
    (mapcar (lambda (e) (cons (+ (the start) (first e)) (rest e)))
            (the relative-events)))))

(defun sort-events (events)
  "EVENTS by time, and by pitch at the same time."
  (sort events (lambda (a b)
                 (or (< (first a) (first b))
                     (and (= (first a) (first b)) (< (third a) (third b)))))))

(define-object track (base-object)
  :input-slots
  (spec
   (track-kind :midi)
   (active? t))

  :computed-slots
  ((track-id (getf (the spec) :id))
   (track-name (getf (the spec) :name))
   (device-specs (getf (the spec) :devices))
   (clip-specs (getf (the spec) :clips))
   (instrument-spec (getf (the spec) :instrument))
   (plan (list :kind (the track-kind)
               :instrument (getf (the instrument-spec) :kind)
               :devices (mapcar (lambda (d) (list :kind (getf d :kind) :key (getf d :key)))
                                (the device-specs))))
   ;; the arrangement's: a clip in a scene plays only when launched
   (events
    (sort-events (loop for clip in (list-elements (the clips))
                       unless (the-object clip scene)
                         append (the-object clip events)))))

  :objects
  ((devices :type 'device
            :sequence (:size (length (the device-specs)))
            :spec (nth (the-child index) (the device-specs)))
   (clips :type 'clip
          :sequence (:size (length (the clip-specs)))
          :spec (nth (the-child index) (the clip-specs)))
   (instrument :type (if (the instrument-spec) 'device 'null-part)
               :spec (the instrument-spec))))

(define-object song (base-object)
  :computed-slots
  ((spec (empty-spec) :settable)
   (undo-entries nil :settable)
   (redo-entries nil :settable)
   (prompt-log nil :settable)

   (song-name (getf (the spec) :name))
   (tempo (getf (the spec) :tempo))
   (track-specs (getf (the spec) :tracks))
   (return-specs (getf (the spec) :returns))
   (master-spec (getf (the spec) :master))
   (any-solo? (some (lambda (track) (getf track :solo)) (the track-specs)))

   (all-tracks (append (list-elements (the tracks))
                       (list-elements (the returns))
                       (list (the master))))

   ;; the structure, and all the program depends on
   (plan (mapcar (lambda (track) (the-object track plan)) (the all-tracks)))

   ;; what the program reads
   (params (song-params self))
   (score (song-score self)))

  :objects
  ((tracks :type 'track
           :sequence (:size (length (the track-specs)))
           :spec (nth (the-child index) (the track-specs))
           :active? (let ((spec (nth (the-child index) (the track-specs))))
                      (if (the any-solo?)
                          (and (getf spec :solo) t)
                          (not (getf spec :mute)))))
   (returns :type 'track
            :sequence (:size (length (the return-specs)))
            :spec (nth (the-child index) (the return-specs))
            :track-kind :return
            :active? (not (getf (nth (the-child index) (the return-specs)) :mute)))
   (master :type 'track
           :spec (the master-spec)
           :track-kind :master)))

(defun make-song (&optional spec)
  "A song; a new one without SPEC."
  (let ((song (make-object 'song)))
    (when spec
      (the-object song (set-slot! :spec spec)))
    song))

(defun song-params (song)
  "The parameter array for SONG, in the order PLAN-LAYOUT places them."
  (let* ((spec (the-object song spec))
         (out (make-array 0 :element-type 'double-float :adjustable t :fill-pointer t)))
    (flet ((emit (x) (vector-push-extend (float x 1d0) out)))
      (emit (getf spec :tempo))
      (emit (if (getf spec :loop-on) 1 0))
      (emit (getf spec :loop-start))
      (emit (getf spec :loop-end))
      (emit (getf spec :quantize 4))
      (flet ((emit-device (device)
               (emit (if (the-object device on?) 1 0))
               (dolist (x (the-object device numbers)) (emit x))))
        (dolist (track (the-object song all-tracks))
          (let ((track-spec (the-object track spec)))
            (emit (getf track-spec :gain 0))
            (emit (getf track-spec :pan 0))
            (emit (if (the-object track active?) 1 0))
            (let ((sends (getf track-spec :sends)))
              (dotimes (i *most-returns*)
                (emit (or (nth i sends) -60))))
            (when (getf track-spec :instrument)
              (emit-device (the-object track instrument)))
            (dolist (device (list-elements (the-object track devices)))
              (emit-device device))))))
    (coerce out '(simple-array double-float (*)))))

(defun lane-index (song track-spec lane placed-track)
  "Where LANE of TRACK-SPEC points in the parameters."
  (declare (ignore song))
  (let ((device (getf lane :device))
        (key (getf lane :param)))
    (if device
        (let* ((specs (cons (getf track-spec :instrument) (getf track-spec :devices)))
               (places (cons (getf placed-track :instrument) (getf placed-track :devices)))
               (at (position device specs :key (lambda (d) (getf d :id))))
               (spec (nth at specs))
               (place (nth at places)))
          (+ (getf place :param)
             (position key (device-params (getf spec :kind)) :key #'param-key)))
        (+ (getf placed-track :param) (ecase key (:gain tp-gain) (:pan tp-pan))))))

(defun clip-region (clip samples layout sr)
  "The six numbers of the audio CLIP's region."
  (let* ((sample (find (getf clip :sample) samples :key (lambda (s) (getf s :id))))
         (row (assoc (getf sample :id) layout))
         (frames (or (fourth row) 0))
         (start (min frames (round (* (getf clip :offset 0) sr)))))
    (list (or (second row) 0) (or (third row) 0) start frames
          (d-exp2 (/ (float (getf clip :transpose 0) 1d0) 12d0))
          (db-gain (float (getf clip :gain 0) 1d0)))))

(defun song-score (song)
  "The score array for SONG: its notes, session clips, automation and
audio regions, as the engine reads them (engine.lisp)."
  (let* ((spec (the-object song spec))
         (tracks (list-elements (the-object song tracks)))
         (samples (getf spec :samples))
         (layout (pool-layout samples))
         (scenes (getf spec :scenes))
         (regions '())
         (placed (plan-layout (the-object song plan) 48000))
         (out (make-array 64 :element-type 'double-float :adjustable t :fill-pointer 0)))
    (labels ((emit (x) (vector-push-extend (float x 1d0) out))
             (put (index x) (setf (aref out index) (float x 1d0)))
             (here () (fill-pointer out))
             (playable (track events)
               ;; an audio clip's id becomes the number of its region
               (if (eq (getf (the-object track instrument-spec) :kind) :sampler)
                   (let ((clips (getf (the-object track spec) :clips)))
                     (mapcar (lambda (event)
                               (destructuring-bind (beat length id velocity) event
                                 (let ((clip (find id clips :key (lambda (c) (getf c :id)))))
                                   (push (clip-region clip samples layout *sample-rate*) regions)
                                   (list beat length (1- (length regions)) velocity))))
                             events))
                   events))
             (emit-events (events)
               (dolist (event events)
                 (dolist (x event) (emit x)))))
      (dotimes (i (+ 4 (* 2 (length tracks)))) (emit 0))
      (put 0 (length tracks))
      ;; the timeline
      (loop for track in tracks
            for i from 0
            do (let ((events (playable track (the-object track events))))
                 (put (+ 4 (* 2 i)) (here))
                 (put (+ 5 (* 2 i)) (length events))
                 (emit-events events)))
      ;; the session's clips, each slot's notes from the clip's own start
      (let ((slots
              (loop for track in tracks
                    collect (loop for scene in scenes
                                  collect (let ((clip (find (getf scene :id) (list-elements (the-object track clips))
                                                            :key (lambda (c) (the-object c scene)))))
                                            (if clip
                                                (let ((events (playable track (the-object clip relative-events))))
                                                  (prog1 (list (here) (length events) (the-object clip clip-length))
                                                    (emit-events events)))
                                                (list 0 0 0)))))))
        ;; automation
        (let ((lanes (loop for track in (the-object song all-tracks)
                           for place in placed
                           append (loop for lane in (getf (the-object track spec) :automation)
                                        when (getf lane :points)
                                          collect (cons (lane-index song (the-object track spec) lane place)
                                                        (lane-numbers track lane))))))
          (put 1 (here))
          (emit (length lanes))
          (dolist (lane lanes)
            (emit (car lane))
            (emit (length (cdr lane)))
            (dolist (point (cdr lane))
              (emit (first point))
              (emit (second point)))))
        ;; the audio regions
        (put 2 (here))
        (dolist (row (reverse regions))
          (dolist (x row) (emit x)))
        ;; the session table
        (let ((table (here)))
          (put 3 table)
          (dotimes (i (* 2 (length tracks))) (emit 0))
          (loop for rows in slots
                for i from 0
                do (put (+ table (* 2 i)) (here))
                   (put (+ table (* 2 i) 1) (length rows))
                   (dolist (row rows)
                     (dolist (x row) (emit x)))))))
    (coerce out '(simple-array double-float (*)))))

(defun lane-numbers (track lane)
  "LANE's points with each value as the engine reads it."
  (let* ((key (getf lane :param))
         (device (getf lane :device))
         (spec (the-object track spec))
         (kind (when device
                 (getf (find device (cons (getf spec :instrument) (getf spec :devices))
                             :key (lambda (d) (getf d :id)))
                       :kind)))
         (param (when kind (find-param kind key))))
    (mapcar (lambda (point)
              (list (first point)
                    (if param (param-number param (second point)) (second point))))
            (getf lane :points))))

;;; Edits.  One function makes every change.

(defparameter *undo-depth* 200 "Song states kept for undo.")

(defvar *journal* nil
  "True while a prompt runs: its forms come back as one undo, which
the prompt's own ending pushes.")

(defun edit-song! (song change)
  "CHANGE, a function of a private copy of the spec, applied: what it
returned.  An error leaves the song as it was."
  (let* ((old (the-object song spec))
         (new (copy-tree old))
         (result (funcall change new)))
    (unless *journal*
      (push-undo! song old))
    (the-object song (set-slot! :spec new))
    result))

(defun push-undo! (song old)
  (let ((entries (cons old (the-object song undo-entries))))
    (the-object song (set-slot! :undo-entries
                                (if (> (length entries) *undo-depth*)
                                    (subseq entries 0 *undo-depth*)
                                    entries)))
    (the-object song (set-slot! :redo-entries nil))))

(defun undo-song! (song)
  "Back one change; true when there was one."
  (let ((entries (the-object song undo-entries)))
    (when entries
      (the-object song (set-slot! :redo-entries (cons (the-object song spec) (the-object song redo-entries))))
      (the-object song (set-slot! :spec (first entries)))
      (the-object song (set-slot! :undo-entries (rest entries)))
      t)))

(defun redo-song! (song)
  "Forward one change; true when there was one."
  (let ((entries (the-object song redo-entries)))
    (when entries
      (the-object song (set-slot! :undo-entries (cons (the-object song spec) (the-object song undo-entries))))
      (the-object song (set-slot! :spec (first entries)))
      (the-object song (set-slot! :redo-entries (rest entries)))
      t)))

(defun put! (plist key value)
  "KEY set to VALUE in PLIST in place, which may be held by something
else, as SETF of GETF would not do for a new key."
  (let ((cell (loop for tail on plist by #'cddr
                    when (eq (car tail) key) return tail)))
    (if cell
        (setf (cadr cell) value)
        (nconc plist (list key value)))
    value))

(defun next-id! (spec)
  "A fresh id from the spec being edited."
  (prog1 (getf spec :next-id)
    (incf (getf spec :next-id))))

;;; The render, and the file.

(defun render-song (song frames &key (sr 48000) (from 0) block-size script)
  "FRAMES frames of SONG from beat FROM: (values left right).  SCRIPT
is what is launched when: ((frame :launch track row)...)."
  (render-plan (the-object song plan) (the-object song params) (the-object song score) frames
               :sr sr :from from :block-size (or block-size frames) :script script
               :pool (pool-for (getf (the-object song spec) :samples))))

(defun song-text (song)
  "SONG's spec as the text of a .tala file."
  (with-standard-io-syntax
    (let ((*print-pretty* nil) (*print-readably* nil))
      (format nil ";;; Tala song~%~s~%" (the-object song spec)))))

(defun upgrade-spec (spec)
  "SPEC with what a song from before the session lacked: four empty
scenes and a launch quantize of a bar.  SPEC itself, changed."
  (unless (getf spec :scenes)
    (put! spec :scenes (loop for name in '("1" "2" "3" "4")
                             collect (list :id (next-id! spec) :name name))))
  (unless (getf spec :quantize)
    (put! spec :quantize 4))
  spec)

(defun read-song-text (text)
  "The spec in TEXT, with no read-time evaluation."
  (let ((spec (with-standard-io-syntax
                ;; standard syntax turns read-eval back on
                (let ((*read-eval* nil))
                  (read-from-string text)))))
    (unless (and (consp spec) (keywordp (first spec)) (getf spec :master))
      (error "That is not a Tala song."))
    (upgrade-spec spec)))

;;; Sessions: each visitor's song, each with a lock of its own.

(defstruct (session (:constructor make-session (id song)))
  id song (lock (bt:make-lock "tala session")))

(defvar *sessions* (make-hash-table :test 'equal))
(defvar *sessions-lock* (bt:make-lock "tala sessions"))

(defparameter *most-sessions* 200 "Songs kept in memory; the oldest is dropped.")

(defun session-for (id)
  "The session called ID, made when new."
  (bt:with-lock-held (*sessions-lock*)
    (or (gethash id *sessions*)
        (progn
          (when (>= (hash-table-count *sessions*) *most-sessions*)
            (clrhash *sessions*))
          (setf (gethash id *sessions*) (make-session id (make-song)))))))
