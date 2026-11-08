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
;; The Lisp API.  Every control on the page is an operation here, and
;; the page drives the song by sending these same forms: there is one
;; door, so nothing can be done with the mouse that the prompt's model
;; or a REPL cannot do.  docs/api.md is the reference for all three.
;;
;; An operation is a function in tala-api of literal arguments on the
;; current song, *SONG*, answering a plist that prints plainly.
;; Tracks, devices and clips go by index or name.  An error says what
;; was wrong and what would have been right, because a model reads it
;; and tries again.
;;

(defvar *song* nil "The song the operations work on.")

(defvar *operators* nil "Every operation's symbol in tala-api.")

(defvar *ui-actions* nil
  "What the forms asked the page to show, newest first; the door binds it.")

(defmacro define-op (name lambda-list doc &body body)
  "An operation NAME in tala-api."
  (let ((symbol (intern (symbol-name name) :tala-api)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',symbol :tala-api))
       (pushnew ',symbol *operators*)
       (defun ,symbol ,lambda-list ,doc ,@body))))

(defun song ()
  (or *song* (error "No song is open.")))

(defun all-tracks (spec)
  (append (getf spec :tracks) (getf spec :returns) (list (getf spec :master))))

(defun find-track (spec track)
  "The track TRACK of SPEC: a number counts the instrument tracks, from
0; a string names any track, the returns and \"Master\" among them."
  (or (typecase track
        (integer (nth track (getf spec :tracks)))
        (string (find track (all-tracks spec) :key (lambda (x) (getf x :name)) :test #'string-equal)))
      (error "There is no track ~s.  The tracks are: ~{~s~^, ~}."
             track (mapcar (lambda (x) (getf x :name)) (all-tracks spec)))))

(defun device-label (device)
  (device-kind-label (device-kind (getf device :kind))))

(defun find-device (track device)
  "The device DEVICE of TRACK: :instrument, a number counting the
effects from 0, a kind such as :delay, or a label such as \"Delay\"."
  (or (cond ((eq device :instrument) (getf track :instrument))
            ((integerp device) (nth device (getf track :devices)))
            ((keywordp device)
             (or (and (getf track :instrument)
                      (eq (getf (getf track :instrument) :kind) device)
                      (getf track :instrument))
                 (find device (getf track :devices) :key (lambda (d) (getf d :kind)))))
            ((stringp device)
             (find device (cons (getf track :instrument) (getf track :devices))
                   :key (lambda (d) (and d (device-label d))) :test #'equalp)))
      (error "Track ~s has no device ~s.  It has: ~{~a~^, ~}."
             (getf track :name) device
             (mapcar #'device-label (remove nil (cons (getf track :instrument) (getf track :devices)))))))

(defun find-clip (track clip)
  "The clip CLIP of TRACK: its id, its name, or nil for the only one."
  (or (typecase clip
        (integer (find clip (getf track :clips) :key (lambda (c) (getf c :id))))
        (string (find clip (getf track :clips) :key (lambda (c) (getf c :name)) :test #'string-equal))
        (null (and (= 1 (length (getf track :clips))) (first (getf track :clips)))))
      (error "Track ~s has no clip ~s.  It has: ~{~s~^, ~}."
             (getf track :name) clip
             (mapcar (lambda (c) (list (getf c :id) (getf c :name))) (getf track :clips)))))

(defparameter *note-names*
  '(("C" . 0) ("C#" . 1) ("DB" . 1) ("D" . 2) ("D#" . 3) ("EB" . 3) ("E" . 4) ("F" . 5)
    ("F#" . 6) ("GB" . 6) ("G" . 7) ("G#" . 8) ("AB" . 8) ("A" . 9) ("A#" . 10) ("BB" . 10) ("B" . 11)))

(defun pitch-number (pitch)
  "PITCH as a MIDI note: a number as it is, or a name such as \"F#2\",
with C3 = 60."
  (etypecase pitch
    (real pitch)
    (string
     (let* ((split (or (position-if (lambda (c) (or (digit-char-p c) (char= c #\-))) pitch)
                       (error "~s is not a note name; say \"C3\", \"F#2\" or a MIDI number." pitch)))
            (name (string-upcase (subseq pitch 0 split)))
            (entry (assoc name *note-names* :test #'string=)))
       (unless entry (error "~s is not a note name; say \"C3\", \"F#2\" or a MIDI number." pitch))
       (+ (cdr entry) (* 12 (+ 2 (parse-integer pitch :start split))))))))

(defun note-spec (note)
  "NOTE, (pitch start length) or (pitch start length velocity), made whole."
  (destructuring-bind (pitch start length &optional (velocity 100)) note
    (list (pitch-number pitch) start length (max 1 (min 127 velocity)))))

(defun set-values! (device pairs)
  "PAIRS, a plist of controls and values, into DEVICE, whose stored
values are only those that differ from the default.  :on switches it."
  (loop for (key value) on pairs by #'cddr
        do (if (eq key :on)
               (put! device :on (and value t))
               (let* ((param (find-param (getf device :kind) key))
                      (checked (checked-value param value))
                      (values (getf device :values)))
                 (remf values key)
                 (unless (eql checked (param-default param))
                   (setf values (list* key checked values)))
                 (put! device :values values)))))

(defun new-device (spec kind pairs)
  (device-kind kind)
  (let ((device (list :id (next-id! spec) :kind kind :on t :key nil :values nil)))
    (set-values! device pairs)
    device))

(defun sample-seconds (sample)
  (/ (getf sample :frames) (float *sample-rate* 1d0)))

(defun find-sample (spec sample)
  "The sample SAMPLE of SPEC, by name or id."
  (or (find sample (getf spec :samples)
            :key (lambda (s) (if (stringp sample) (getf s :name) (getf s :id)))
            :test #'equalp)
      (error "There is no sample ~s.  The samples are: ~{~s~^, ~}."
             sample (mapcar (lambda (s) (getf s :name)) (getf spec :samples)))))

(defun unique-sample-name (spec name)
  (let ((names (mapcar (lambda (s) (getf s :name)) (getf spec :samples))))
    (if (member name names :test #'string-equal)
        (loop for i from 2
              for candidate = (format nil "~a ~d" name i)
              unless (member candidate names :test #'string-equal)
                return candidate)
        name)))

(defun import-sample! (song name octets channels)
  "OCTETS, float32 samples at the engine's rate, a channel after the
other, kept and named in SONG: the name it took.  The same sound twice
is one copy under two names."
  (multiple-value-bind (stamp frames) (store-pcm! octets channels)
    (edit-song! song
                (lambda (spec)
                  (let ((taken (unique-sample-name spec name)))
                    (put! spec :samples
                          (append (getf spec :samples)
                                  (list (list :id (next-id! spec) :name taken :stamp stamp
                                              :frames frames :channels channels :words nil))))
                    taken)))))

(defun value-text (device)
  "The values of DEVICE that differ from the default, as a plist."
  (getf device :values))

;;; Reading the song.

(define-op describe-song ()
  "The song in words: tempo, loop, each track with its instrument,
effects, clips and automation."
  (let ((spec (the-object (song) spec)))
    (with-output-to-string (out)
      (let ((*print-case* :downcase) (*print-pretty* nil)
            (*read-default-float-format* 'double-float))
        (format out "Song ~s: ~a BPM, ~a/~a, loop ~:[off~;on~], beats ~a to ~a.~%"
                (getf spec :name) (getf spec :tempo) (getf spec :numerator) (getf spec :denominator)
                (getf spec :loop-on) (getf spec :loop-start) (getf spec :loop-end))
        (format out "Scenes, the session's rows: ~:[none~;~:*~{~a~^, ~}~]; launches wait for ~a beats.~%"
                (loop for scene in (getf spec :scenes)
                      for row from 0
                      collect (format nil "~d ~s" row (getf scene :name)))
                (getf spec :quantize 4))
        (flet ((device-line (device index)
                 (format out "    ~@[~a: ~]~a~:[ (off)~;~]~@[, ducked by track ~a~]~@[: ~s~]~%"
                         index (device-label device) (getf device :on t)
                         (let ((key (getf device :key)))
                           (and key (getf (nth key (getf spec :tracks)) :name)))
                         (value-text device))))
          (flet ((track-lines (track label)
                   (format out "~a ~s: gain ~a dB, pan ~a~@[, sends ~a~]~:[~;, muted~]~:[~;, soloed~]~%"
                           label (getf track :name) (getf track :gain 0) (getf track :pan 0)
                           (getf track :sends) (getf track :mute) (getf track :solo))
                   (when (getf track :instrument)
                     (device-line (getf track :instrument) nil))
                   (loop for device in (getf track :devices)
                         for i from 0
                         do (device-line device i))
                   (dolist (clip (getf track :clips))
                     (let ((scene (find (getf clip :scene) (getf spec :scenes) :key (lambda (s) (getf s :id)))))
                       (when scene
                         (format out "    in scene ~s (row ~d):~%"
                                 (getf scene :name) (position scene (getf spec :scenes)))))
                     (if (getf clip :sample)
                         (format out "    audio clip ~a ~s: starts at beat ~a, ~a beats long, from ~a s into the sample, gain ~a dB, transpose ~a~%"
                                 (getf clip :id) (getf clip :name) (getf clip :start) (getf clip :length)
                                 (getf clip :offset 0) (getf clip :gain 0) (getf clip :transpose 0))
                         (format out "    clip ~a ~s: starts at beat ~a, ~a beats long, repeats every ~a, ~d notes~%"
                                 (getf clip :id) (getf clip :name) (getf clip :start) (getf clip :length)
                                 (or (getf clip :loop) (getf clip :length)) (length (getf clip :notes)))))
                   (dolist (lane (getf track :automation))
                     (let ((points (getf lane :points)))
                       (format out "    automation of ~(~s~)~@[ on device ~a~]: ~d points, beats ~a to ~a~%"
                               (getf lane :param) (getf lane :device) (length points)
                               (first (first points)) (first (first (last points))))))))
            (loop for track in (getf spec :tracks)
                  for i from 0
                  do (track-lines track (format nil "Track ~d" i)))
            (dolist (track (getf spec :returns))
              (track-lines track "Return"))
            (track-lines (getf spec :master) "Master")))
        (dolist (sample (getf spec :samples))
          (format out "Sample ~s: ~,2f seconds, ~a~@[, saying ~s~]~%"
                  (getf sample :name) (sample-seconds sample)
                  (if (= 2 (getf sample :channels)) "stereo" "mono")
                  (getf sample :words)))
        (unless (getf spec :tracks)
          (format out "There are no instrument tracks yet.~%"))))))

(define-op song-info ()
  "The song as a plist."
  (let ((spec (the-object (song) spec)))
    (list :name (getf spec :name) :tempo (getf spec :tempo)
          :numerator (getf spec :numerator) :denominator (getf spec :denominator)
          :loop-on (getf spec :loop-on) :loop-start (getf spec :loop-start) :loop-end (getf spec :loop-end)
          :tracks (mapcar (lambda (x) (getf x :name)) (getf spec :tracks))
          :returns (mapcar (lambda (x) (getf x :name)) (getf spec :returns))
          :undo-depth (length (the-object (song) undo-entries)))))

(define-op device-kinds ()
  "Every device: (key label type what-it-does)."
  (mapcar (lambda (d) (list (device-kind-key d) (device-kind-label d) (device-kind-type d) (device-kind-doc d)))
          *device-kinds*))

(define-op device-controls (kind)
  "The controls of device KIND: (key label default range doc)."
  (device-kind kind)
  (mapcar (lambda (param)
            (list (param-key param) (param-label param) (param-default param)
                  (or (param-choices param) (list (param-min param) (param-max param) (param-unit param)))
                  (param-doc param)))
          (device-params kind)))

(define-op device-values (track device)
  "The values of a device that differ from the defaults, and whether it is on."
  (let ((d (find-device (find-track (the-object (song) spec) track) device)))
    (list :id (getf d :id) :kind (getf d :kind) :on (getf d :on t) :values (getf d :values))))

(define-op clip-notes (track &optional clip)
  "A clip's notes: each (pitch start length velocity), in beats from the clip's start."
  (getf (find-clip (find-track (the-object (song) spec) track) clip) :notes))

(define-op samples ()
  "The song's samples: name, seconds, channels, and what they say when told."
  (mapcar (lambda (s)
            (list :name (getf s :name) :seconds (sample-seconds s)
                  :channels (getf s :channels) :words (getf s :words)))
          (getf (the-object (song) spec) :samples)))

(define-op sample-info (sample)
  "A sample's length and level, and its phrases: where the sound is, as
((start end)...) in seconds, the places to cut speech into pieces."
  (let* ((found (find-sample (the-object (song) spec) sample))
         (pcm (or (find-pcm (getf found :stamp))
                  (error "The audio of ~s is not on this server; import it again." (getf found :name)))))
    (list :name (getf found :name) :seconds (sample-seconds found)
          :channels (getf found :channels) :words (getf found :words)
          :peak (db-of (reduce #'max (pcm-data pcm) :key #'abs))
          :phrases (pcm-phrases pcm *sample-rate*))))

(define-op hear (&key (from 1) (bars 8) track scene)
  "A spectrogram and measures of BARS bars from bar FROM, of the song
or of TRACK alone (a name); with SCENE, of that scene launched at once,
from its start."
  (hear-song (song) :from (if scene 1 from) :bars bars :track track
                    :scene (and scene (scene-row (the-object (song) spec) scene))))

;;; The song.

(define-op set-tempo (bpm)
  "Set the tempo, 20 to 300 beats a minute."
  (unless (and (realp bpm) (<= 20 bpm 300))
    (error "The tempo is a number from 20 to 300, not ~s." bpm))
  (edit-song! (song) (lambda (spec) (put! spec :tempo bpm)))
  (list :tempo bpm))

(define-op set-time-signature (numerator denominator)
  "Set the time signature: 4 4, 3 4, 7 8."
  (unless (and (integerp numerator) (<= 1 numerator 32) (member denominator '(2 4 8 16)))
    (error "A time signature is a number from 1 to 32 over 2, 4, 8 or 16, not ~s/~s." numerator denominator))
  (edit-song! (song) (lambda (spec) (put! spec :numerator numerator) (put! spec :denominator denominator)))
  (list :numerator numerator :denominator denominator))

(define-op set-loop (start end &key (on t))
  "Loop from beat START to beat END; :on nil plays straight through."
  (unless (and (realp start) (realp end) (<= 0 start) (< start end))
    (error "A loop runs from a beat to a later beat, not ~s to ~s." start end))
  (edit-song! (song) (lambda (spec)
                       (put! spec :loop-start start)
                       (put! spec :loop-end end)
                       (put! spec :loop-on (and on t))))
  (list :loop-start start :loop-end end :loop-on (and on t)))

(define-op rename-song (name)
  "Name the song."
  (edit-song! (song) (lambda (spec) (put! spec :name name)))
  (list :name name))

(define-op clear-song ()
  "Start again with an empty song.  The tempo stays; undo brings all of it back."
  (edit-song! (song) (lambda (spec)
                       (let ((fresh (empty-spec (getf spec :name))))
                         (loop for key in '(:tracks :returns :master :next-id)
                               do (put! spec key (getf fresh key))))))
  (list :cleared t))

;;; Tracks.

(defun new-track (spec name instrument returns)
  (list :id (next-id! spec) :name name :color (mod (length (getf spec :tracks)) 12)
        :gain 0 :pan 0 :mute nil :solo nil
        :sends (make-list returns :initial-element -60)
        :instrument instrument :devices nil :clips nil :automation nil))

(defun check-new-name (spec name)
  (unless (and (stringp name) (plusp (length name)))
    (error "A track's name is a string."))
  (when (find name (all-tracks spec) :key (lambda (x) (getf x :name)) :test #'string-equal)
    (error "There is already a track named ~s." name)))

(define-op add-track (name &key (instrument :synth) values)
  "A new instrument track NAME playing INSTRUMENT, :synth or :drums,
with VALUES, a plist of its controls."
  (edit-song! (song)
              (lambda (spec)
                (check-new-name spec name)
                (unless (eq (device-kind-type (device-kind instrument)) :instrument)
                  (error "~(~s~) is an effect.  An instrument is :synth or :drums." instrument))
                (let* ((device (new-device spec instrument values))
                       (track (new-track spec name device (length (getf spec :returns)))))
                  (put! spec :tracks (append (getf spec :tracks) (list track)))
                  (list :track name :index (1- (length (getf spec :tracks))))))))

(define-op add-return (name)
  "A new return track NAME; every track gets a send to it, at -60 dB (off)."
  (edit-song! (song)
              (lambda (spec)
                (check-new-name spec name)
                (when (>= (length (getf spec :returns)) *most-returns*)
                  (error "A song has at most ~d return tracks." *most-returns*))
                (let ((track (new-track spec name nil 0)))
                  (put! track :clips nil)
                  (put! spec :returns (append (getf spec :returns) (list track)))
                  (dolist (other (getf spec :tracks))
                    (put! other :sends (append (getf other :sends) (list -60))))
                  (list :return name :send-index (1- (length (getf spec :returns))))))))

(define-op delete-track (track)
  "Remove a track, and its send when it is a return."
  (edit-song! (song)
              (lambda (spec)
                (let* ((found (find-track spec track))
                       (returns (getf spec :returns))
                       (tracks (getf spec :tracks))
                       (return-at (position found returns))
                       (track-at (position found tracks)))
                  (cond (return-at
                         (put! spec :returns (remove found returns))
                         (dolist (other tracks)
                           (let ((sends (copy-list (getf other :sends))))
                             (put! other :sends (append (subseq sends 0 return-at)
                                                        (nthcdr (1+ return-at) sends))))))
                        (track-at
                         (put! spec :tracks (remove found tracks))
                         ;; a compressor ducked by it goes back to its own signal,
                         ;; and the keys after it move down
                         (dolist (other (all-tracks spec))
                           (dolist (device (getf other :devices))
                             (let ((key (getf device :key)))
                               (when key
                                 (put! device :key (cond ((= key track-at) nil)
                                                         ((> key track-at) (1- key))
                                                         (t key))))))))
                        (t (error "The master cannot be deleted.")))
                  (list :deleted (getf found :name))))))

(define-op rename-track (track name)
  "Rename a track."
  (edit-song! (song) (lambda (spec)
                       (let ((found (find-track spec track)))
                         (unless (string-equal name (getf found :name))
                           (check-new-name spec name))
                         (put! found :name name))))
  (list :track name))

(define-op move-track (track index)
  "Move an instrument track to position INDEX."
  (edit-song! (song)
              (lambda (spec)
                (let* ((found (find-track spec track))
                       (tracks (getf spec :tracks))
                       (from (or (position found tracks)
                                 (error "Only an instrument track moves; ~s is not one." track)))
                       (rest (remove found tracks))
                       (to (max 0 (min index (length rest))))
                       (moved (append (subseq rest 0 to) (list found) (nthcdr to rest))))
                  ;; keys are track numbers: they follow their tracks
                  (let ((numbers (mapcar (lambda (x) (position x moved)) tracks)))
                    (dolist (other (all-tracks spec))
                      (dolist (device (getf other :devices))
                        (let ((key (getf device :key)))
                          (when key (put! device :key (nth key numbers)))))))
                  (put! spec :tracks moved)
                  (list :track (getf found :name) :from from :to to)))))

(define-op set-track (track &key gain pan (mute nil mute?) (solo nil solo?) sends color)
  "A track's mixer: GAIN in dB (-60 to 12), PAN (-1 to 1), MUTE, SOLO,
SENDS a list of dB, one per return, COLOR 0 to 11."
  (edit-song! (song)
              (lambda (spec)
                (let ((found (find-track spec track)))
                  (when gain (put! found :gain (max -60 (min 12 gain))))
                  (when pan (put! found :pan (max -1 (min 1 pan))))
                  (when mute? (put! found :mute (and mute t)))
                  (when solo? (put! found :solo (and solo t)))
                  (when color (put! found :color (mod color 12)))
                  (when sends
                    (unless (= (length sends) (length (getf spec :returns)))
                      (error "This song has ~d return tracks, so :sends takes ~:*~d numbers."
                             (length (getf spec :returns))))
                    (put! found :sends (mapcar (lambda (x) (max -60 (min 6 x))) sends)))
                  (list :track (getf found :name) :gain (getf found :gain) :pan (getf found :pan)
                        :mute (getf found :mute) :solo (getf found :solo) :sends (getf found :sends))))))

;;; Devices.

(define-op set-instrument (track kind &rest values)
  "Replace a track's instrument with a new one of KIND, with VALUES a
plist of its controls."
  (edit-song! (song)
              (lambda (spec)
                (let ((found (find-track spec track)))
                  (unless (eq (device-kind-type (device-kind kind)) :instrument)
                    (error "~(~s~) is not an instrument." kind))
                  (put! found :instrument (new-device spec kind values))
                  (list :track (getf found :name) :instrument kind)))))

(define-op add-device (track kind &rest values)
  "An effect of KIND at the end of a track's chain, with VALUES a plist
of its controls; :index n in VALUES puts it at position n."
  (edit-song! (song)
              (lambda (spec)
                (let* ((found (find-track spec track))
                       (index (getf values :index))
                       (pairs (let ((copy (copy-list values))) (remf copy :index) copy)))
                  (unless (eq (device-kind-type (device-kind kind)) :effect)
                    (error "~(~s~) is an instrument, not an effect." kind))
                  (let* ((device (new-device spec kind pairs))
                         (chain (getf found :devices))
                         (at (if index (max 0 (min index (length chain))) (length chain))))
                    (put! found :devices (append (subseq chain 0 at) (list device) (nthcdr at chain)))
                    (list :track (getf found :name) :device (device-label device)
                          :index at :id (getf device :id)))))))

(define-op delete-device (track device)
  "Take an effect off a track, and its automation."
  (edit-song! (song)
              (lambda (spec)
                (let* ((found (find-track spec track))
                       (d (find-device found device)))
                  (when (eq d (getf found :instrument))
                    (error "A track keeps its instrument; set-instrument replaces it."))
                  (put! found :devices (remove d (getf found :devices)))
                  (put! found :automation (remove (getf d :id) (getf found :automation)
                                                  :key (lambda (lane) (getf lane :device))))
                  (list :deleted (device-label d))))))

(define-op move-device (track device index)
  "Move an effect to position INDEX in its chain."
  (edit-song! (song)
              (lambda (spec)
                (let* ((found (find-track spec track))
                       (d (find-device found device))
                       (rest (remove d (getf found :devices)))
                       (at (max 0 (min index (length rest)))))
                  (put! found :devices (append (subseq rest 0 at) (list d) (nthcdr at rest)))
                  (list :device (device-label d) :index at)))))

(define-op set-device (track device &rest values)
  "A device's controls: VALUES a plist, :cutoff 800 :resonance 0.6.  A
number past a control's range is held at the end.  :on nil bypasses it."
  (edit-song! (song)
              (lambda (spec)
                (let ((d (find-device (find-track spec track) device)))
                  (set-values! d values)
                  (list :device (device-label d) :values (getf d :values) :on (getf d :on t))))))

(define-op reset-device (track device)
  "Every control of a device back to its default."
  (edit-song! (song)
              (lambda (spec)
                (let ((d (find-device (find-track spec track) device)))
                  (put! d :values nil)
                  (list :device (device-label d))))))

(define-op set-sidechain (track device source)
  "The compressor DEVICE on TRACK listens to the instrument track
SOURCE, so it ducks when SOURCE plays; nil listens to its own track."
  (edit-song! (song)
              (lambda (spec)
                (let* ((found (find-track spec track))
                       (d (find-device found device)))
                  (unless (eq (getf d :kind) :compressor)
                    (error "Only a compressor takes a sidechain."))
                  (put! d :key (when source
                                 (or (position (find-track spec source) (getf spec :tracks))
                                     (error "A sidechain listens to an instrument track; ~s is not one." source))))
                  (list :device (device-label d) :listens-to source)))))

;;; Clips and notes.

(defun check-sampler (found)
  (unless (eq (getf (getf found :instrument) :kind) :sampler)
    (error "Audio clips go on an audio track, one made with (add-track name :instrument :sampler); ~s is not one."
           (getf found :name))))

(define-op add-audio-clip (track sample &key (start 0) length (offset 0) (gain 0) (transpose 0) name scene)
  "SAMPLE on an audio TRACK from beat START.  OFFSET is where in the
sample it starts, in seconds; LENGTH, in beats, runs to the sample's end
when unsaid; GAIN is in dB; TRANSPOSE in semitones changes its pitch and
speed together.  With SCENE it goes in that scene's slot of the session."
  (edit-song! (song)
              (lambda (spec)
                (let* ((found (find-track spec track))
                       (s (find-sample spec sample))
                       (rate (expt 2d0 (/ transpose 12d0)))
                       (seconds (/ (- (sample-seconds s) offset) rate))
                       (beats (or length (/ (round (* 1000 seconds (/ (getf spec :tempo) 60))) 1000d0)))
                       (clip (list :id (next-id! spec) :name (or name (getf s :name))
                                   :start start :length beats :sample (getf s :id)
                                   :offset offset :gain gain :transpose transpose)))
                  (check-sampler found)
                  (when scene
                    (let ((in (find-scene spec scene)))
                      (check-slot-free found in)
                      (put! clip :scene (getf in :id))
                      (put! clip :start 0)))
                  (unless (and (realp start) (<= 0 start) (realp offset) (<= 0 offset)
                               (plusp seconds) (plusp beats))
                    (error "A clip starts at a beat from 0 up, at an offset within the sample, which is ~,2f seconds long."
                           (sample-seconds s)))
                  (put! found :clips (append (getf found :clips) (list clip)))
                  (list :clip (getf clip :id) :track (getf found :name) :start start :length beats)))))

(define-op set-sample (sample &key name (words nil words?))
  "Rename a sample, or say what it says: WORDS, which is the only way
the model knows."
  (edit-song! (song)
              (lambda (spec)
                (let ((s (find-sample spec sample)))
                  (when name
                    (unless (string-equal name (getf s :name))
                      (put! s :name (unique-sample-name spec name))))
                  (when words? (put! s :words words))
                  (list :name (getf s :name) :words (getf s :words))))))

(define-op delete-sample (sample)
  "Remove a sample no clip is using."
  (edit-song! (song)
              (lambda (spec)
                (let ((s (find-sample spec sample)))
                  (when (some (lambda (track)
                                (find (getf s :id) (getf track :clips)
                                      :key (lambda (c) (getf c :sample))))
                              (getf spec :tracks))
                    (error "Clips still use ~s; delete them first." (getf s :name)))
                  (put! spec :samples (remove s (getf spec :samples)))
                  (list :deleted (getf s :name))))))

(defun keep-clips-in-place! (spec id shift)
  "SPEC's clips of sample ID after SHIFT seconds were cut from its front:
the sound each one plays stays where it was in the song."
  (dolist (track (getf spec :tracks))
    (dolist (c (getf track :clips))
      (when (eql (getf c :sample) id)
        (let* ((offset (getf c :offset 0))
               (rate (expt 2d0 (/ (getf c :transpose 0) 12d0)))
               ;; what was cut inside the clip, after its start in the sample
               (late (max 0 (- shift offset)))
               (beats (/ (* late (getf spec :tempo)) 60 rate)))
          (put! c :offset (max 0 (- offset shift)))
          (when (plusp beats)
            (put! c :start (+ (getf c :start) beats))
            (put! c :length (max 0.0625 (- (getf c :length) beats)))))))))

(defun point-at-repair! (spec id as stamp frames channels shift)
  "SPEC with sample ID's sound now STAMP, or a new sample named AS beside
it: the name the repaired sound goes by."
  (let ((s (find-sample spec id)))
    (cond (as
           (let ((taken (unique-sample-name spec as)))
             (put! spec :samples
                   (append (getf spec :samples)
                           (list (list :id (next-id! spec) :name taken :stamp stamp
                                       :frames frames :channels channels :words (getf s :words)))))
             taken))
          (t
           (put! s :stamp stamp)
           (put! s :frames frames)
           (when (and shift (plusp shift))
             (keep-clips-in-place! spec id shift))
           (getf s :name)))))

(defun repair-sample! (sample process &key as)
  "PROCESS, a function of a sample's pcm and channels answering new
channels, a report and how many seconds it cut from the front, made the
sample's sound: in place, every clip using it then playing the repair
and undo bringing the original back, or with AS a new sample of that
name.  The work is done before the edit, which only points at it."
  (let* ((song (song))
         (found (find-sample (the-object song spec) sample))
         (id (getf found :id))
         (pcm (or (find-pcm (getf found :stamp))
                  (error "The audio of ~s is not on this server; import it again." (getf found :name)))))
    (multiple-value-bind (channels report shift) (funcall process pcm (pcm-channels-of pcm))
      (multiple-value-bind (stamp frames) (store-pcm! (channels-octets channels) (length channels))
        (let ((name (edit-song! song (lambda (spec)
                                       (point-at-repair! spec id as stamp frames (length channels) shift)))))
          (list* :sample name
                 :seconds (/ (round (* 1000 (/ frames *sample-rate*))) 1000d0)
                 report))))))

(define-op normalize-sample (sample &key (peak -1) as)
  "Scale a sample so its loudest moment is at PEAK dB.  With AS, the
result is a new sample of that name and the original stays."
  (repair-sample! sample (lambda (pcm channels)
                           (declare (ignore pcm))
                           (normalize-channels channels peak))
                  :as as))

(define-op gain-sample (sample db &key as)
  "Turn a sample up or down by DB."
  (unless (realp db)
    (error "Gain is a number of dB, not ~s." db))
  (repair-sample! sample (lambda (pcm channels)
                           (declare (ignore pcm))
                           (values (scale-channels channels (expt 10d0 (/ db 20))) (list :gain db)))
                  :as as))

(define-op trim-sample (sample &key (threshold -50) (pad 0.05) as)
  "Cut the quiet before a sample's first sound and after its last, quiet
being under THRESHOLD dB, keeping PAD seconds either side.  Clips using
it keep their sound where it was in the song."
  (repair-sample! sample (lambda (pcm channels)
                           (declare (ignore pcm))
                           (trim-channels channels *sample-rate* threshold pad))
                  :as as))

(define-op dehum-sample (sample &key (frequency :auto) (harmonics 8) as)
  "Take mains hum out of a sample: FREQUENCY 50, 60 or :auto to find
which, and its first HARMONICS multiples, each by a notch about 2 Hz
wide.  Says so when there is no hum to take out."
  (repair-sample! sample (lambda (pcm channels)
                           (declare (ignore pcm))
                           (dehum-channels channels *sample-rate* frequency harmonics))
                  :as as))

(define-op denoise-sample (sample &key (reduction 12) (sensitivity 1) noise as)
  "Turn down a sample's steady background noise (hiss, room tone, a fan)
by up to REDUCTION dB, learned from where nobody speaks: the gaps
between its phrases, or NOISE, ((start end)...) in seconds, when given.
SENSITIVITY above 1 takes more and risks a watery sound; below 1, less."
  (unless (and (realp reduction) (<= 1 reduction 40))
    (error "The reduction is a number of dB from 1 to 40, not ~s." reduction))
  (unless (and (realp sensitivity) (<= 0.25 sensitivity 4))
    (error "The sensitivity is a number from 0.25 to 4, not ~s." sensitivity))
  (repair-sample! sample (lambda (pcm channels)
                           (denoise-channels channels pcm *sample-rate* reduction sensitivity noise))
                  :as as))

(define-op add-clip (track &key name (start 0) (length 4) loop notes scene)
  "A clip on TRACK from beat START, LENGTH beats long.  With LOOP, its
notes repeat every LOOP beats.  NOTES as in add-notes.  With SCENE it
goes in that scene's slot of the session instead of on the timeline."
  (edit-song! (song)
              (lambda (spec)
                (let* ((found (find-track spec track))
                       (in (and scene (find-scene spec scene)))
                       (clip (list :id (next-id! spec)
                                   :name (or name (getf found :name))
                                   :start (if in 0 start) :length length :loop loop
                                   :notes (mapcar #'note-spec notes))))
                  (when in
                    (check-slot-free found in)
                    (put! clip :scene (getf in :id)))
                  (unless (getf found :instrument)
                    (error "Clips go on instrument tracks; ~s is not one." (getf found :name)))
                  (when (eq (getf (getf found :instrument) :kind) :sampler)
                    (error "~s is an audio track: it takes add-audio-clip." (getf found :name)))
                  (unless (and (realp start) (<= 0 start) (realp length) (plusp length))
                    (error "A clip starts at a beat from 0 up and has a positive length."))
                  (put! found :clips (append (getf found :clips) (list clip)))
                  (list :clip (getf clip :id) :track (getf found :name)
                        :start start :length length :notes (length notes))))))

(define-op delete-clip (track clip)
  "Remove a clip."
  (edit-song! (song)
              (lambda (spec)
                (let* ((found (find-track spec track))
                       (c (find-clip found clip)))
                  (put! found :clips (remove c (getf found :clips)))
                  (list :deleted (getf c :id))))))

(define-op set-clip (track clip &key name start length (loop nil loop?) offset gain transpose)
  "Move, resize or rename a clip; :loop sets how often its notes repeat.
On an audio clip, :offset (seconds into the sample), :gain and :transpose."
  (edit-song! (song)
              (lambda (spec)
                (let ((c (find-clip (find-track spec track) clip)))
                  (when name (put! c :name name))
                  (when start (put! c :start (max 0 start)))
                  (when length (put! c :length (max 0.0625 length)))
                  (when loop? (put! c :loop loop))
                  (when offset (put! c :offset (max 0 offset)))
                  (when gain (put! c :gain (max -60 (min 24 gain))))
                  (when transpose (put! c :transpose (max -36 (min 36 transpose))))
                  (list :clip (getf c :id) :start (getf c :start) :length (getf c :length)
                        :loop (getf c :loop))))))

(define-op trim-clip-start (track clip beats)
  "Move a clip's start BEATS later (or earlier, when negative) and keep
its end and everything it plays where they were: an audio clip begins
further into its sample, a clip's notes stay at the same places in the
song.  How the front of a clip is cut off."
  (edit-song! (song)
              (lambda (spec)
                (let* ((c (find-clip (find-track spec track) clip))
                       (start (getf c :start))
                       (length (getf c :length))
                       (rate (expt 2d0 (/ (getf c :transpose 0) 12d0)))
                       (seconds-per-beat (/ 60d0 (getf spec :tempo)))
                       ;; no further than a sixteenth before the end, no
                       ;; earlier than the song's start or the sample's
                       (most (- length 0.0625d0))
                       (least (max (- start)
                                   (if (getf c :sample)
                                       (- (/ (getf c :offset 0) seconds-per-beat rate))
                                       most-negative-double-float)))
                       (d (float (max least (min most beats)) 1d0))
                       (snap (lambda (x) (/ (round (* x 1000000)) 1000000d0))))
                  (unless (realp beats)
                    (error "Trim by a number of beats, not ~s." beats))
                  (put! c :start (funcall snap (+ start d)))
                  (put! c :length (funcall snap (- length d)))
                  (if (getf c :sample)
                      (put! c :offset (funcall snap (max 0 (+ (getf c :offset 0) (* d seconds-per-beat rate)))))
                      (let ((repeat (getf c :loop)))
                        (put! c :notes
                              (if (and repeat (< repeat length))
                                  ;; a looping pattern turns, so the same note
                                  ;; falls on the same beat of the song
                                  (mapcar (lambda (n) (list* (first n) (funcall snap (mod (- (second n) d) repeat)) (cddr n)))
                                          (getf c :notes))
                                  (loop for n in (getf c :notes)
                                        for at = (- (second n) d)
                                        when (>= at 0)
                                          collect (list* (first n) (funcall snap at) (cddr n)))))))
                  (list :clip (getf c :id) :start (getf c :start) :length (getf c :length)
                        :offset (getf c :offset))))))

(define-op duplicate-clip (track clip start)
  "A copy of a clip at beat START."
  (edit-song! (song)
              (lambda (spec)
                (let* ((found (find-track spec track))
                       (c (copy-tree (find-clip found clip))))
                  (put! c :id (next-id! spec))
                  (put! c :start start)
                  (put! found :clips (append (getf found :clips) (list c)))
                  (list :clip (getf c :id) :start start)))))

(define-op add-notes (track clip notes)
  "NOTES into a clip: each (pitch start length) or (pitch start length
velocity), the pitch a MIDI number or a name, C3 = 60, the times in
beats from the clip's start, velocity 1 to 127."
  (edit-song! (song)
              (lambda (spec)
                (let ((c (find-clip (find-track spec track) clip)))
                  (put! c :notes (append (getf c :notes) (mapcar #'note-spec notes)))
                  (list :clip (getf c :id) :notes (length (getf c :notes)))))))

(define-op set-notes (track clip notes)
  "A clip's notes replaced by NOTES, as in add-notes."
  (edit-song! (song)
              (lambda (spec)
                (let ((c (find-clip (find-track spec track) clip)))
                  (put! c :notes (mapcar #'note-spec notes))
                  (list :clip (getf c :id) :notes (length (getf c :notes)))))))

(define-op add-steps (track clip pitch steps &key (step 0.25) (velocity 100) (accent 127) (gate 0.9) (start 0))
  "A step pattern into a clip: STEPS a string, x a note, X an accented
one, anything else a rest; each character STEP beats long (0.25 is a
sixteenth) from beat START of the clip, each note GATE of its step long.
The drum kit's pads are pitches 36 to 43."
  (unless (stringp steps)
    (error "Steps are a string such as \"x...x...x...x...\"."))
  (let ((notes (loop for c across steps
                     for i from 0
                     when (char-equal c #\x)
                       collect (list pitch (+ start (* i step)) (* step gate)
                                     (if (char= c #\X) accent velocity)))))
    (tala-api::add-notes track clip notes)))

(define-op record-notes (track notes &key name quantize)
  "A take: NOTES as played, each (pitch start length velocity) with START
in beats of the song, made a new clip on TRACK spanning the bars they
fall in.  QUANTIZE, a grid in beats (0.25 a sixteenth), moves each start
to the nearest line.  How the page keeps what was played while
recording."
  (unless (and (listp notes) notes)
    (error "A take is a list of notes, (pitch start length velocity) each; this one is empty."))
  (edit-song! (song)
              (lambda (spec)
                (let* ((found (find-track spec track))
                       (bar (song-beats-per-bar spec))
                       (played (mapcar #'note-spec notes))
                       (grid (and quantize (realp quantize) (plusp quantize) quantize))
                       (placed (if grid
                                   (mapcar (lambda (n) (list* (first n) (* grid (round (second n) grid)) (cddr n))) played)
                                   played))
                       (from (* bar (floor (reduce #'min placed :key #'second) bar)))
                       (to (* bar (max (1+ (floor from bar))
                                       (ceiling (reduce #'max placed :key (lambda (n) (+ (second n) (third n)))) bar))))
                       (clip (list :id (next-id! spec) :name (or name "Take")
                                   :start from :length (- to from) :loop nil
                                   :notes (mapcar (lambda (n) (list* (first n) (- (second n) from) (cddr n)))
                                                  (sort placed #'< :key #'second)))))
                  (unless (getf found :instrument)
                    (error "A take goes on an instrument track; ~s is not one." (getf found :name)))
                  (when (eq (getf (getf found :instrument) :kind) :sampler)
                    (error "~s is an audio track: notes go on a synth or drum track." (getf found :name)))
                  (put! found :clips (append (getf found :clips) (list clip)))
                  (list :clip (getf clip :id) :start from :length (- to from) :notes (length placed))))))

(define-op quantize-notes (track clip grid &key (strength 1))
  "A clip's note starts moved toward the nearest line of GRID beats (0.25
a sixteenth, 0.5 an eighth); STRENGTH 1 all the way, 0.5 halfway, which
keeps some of the feel."
  (unless (and (realp grid) (plusp grid))
    (error "The grid is a number of beats, such as 0.25 for sixteenths, not ~s." grid))
  (edit-song! (song)
              (lambda (spec)
                (let ((c (find-clip (find-track spec track) clip))
                      (s (max 0 (min 1 strength))))
                  (put! c :notes (mapcar (lambda (n)
                                           (let* ((start (second n))
                                                  (line (* grid (round start grid))))
                                             (list* (first n) (+ start (* s (- line start))) (cddr n))))
                                         (getf c :notes)))
                  (list :clip (getf c :id) :grid grid :strength s)))))

(define-op transpose-notes (track clip semitones)
  "Every note of a clip moved by SEMITONES."
  (edit-song! (song)
              (lambda (spec)
                (let ((c (find-clip (find-track spec track) clip)))
                  (put! c :notes (mapcar (lambda (n) (cons (max 0 (min 127 (+ (first n) semitones))) (rest n)))
                                         (getf c :notes)))
                  (list :clip (getf c :id) :semitones semitones)))))

;;; The session: scenes, slots, launching.

(defun find-scene (spec scene)
  "The scene SCENE of SPEC: a number counts the rows from 0, a string
names one."
  (or (typecase scene
        (integer (nth scene (getf spec :scenes)))
        (string (find scene (getf spec :scenes) :key (lambda (s) (getf s :name)) :test #'string-equal)))
      (error "There is no scene ~s.  The scenes are: ~{~s~^, ~}."
             scene (mapcar (lambda (s) (getf s :name)) (getf spec :scenes)))))

(defun scene-row (spec scene)
  (position (find-scene spec scene) (getf spec :scenes)))

(defun slot-clip (track scene-id)
  (find scene-id (getf track :clips) :key (lambda (c) (getf c :scene))))

(defun check-slot-free (track scene)
  (when (slot-clip track (getf scene :id))
    (error "Track ~s already has a clip in scene ~s; delete it or choose another scene."
           (getf track :name) (getf scene :name))))

(define-op add-scene (name &key index)
  "A new scene, a row of the session, called NAME (nil for its number),
at the end or at INDEX."
  (edit-song! (song)
              (lambda (spec)
                (let* ((scenes (getf spec :scenes))
                       (scene (list :id (next-id! spec)
                                    :name (or name (princ-to-string (1+ (length scenes))))))
                       (at (if index (max 0 (min index (length scenes))) (length scenes))))
                  (put! spec :scenes (append (subseq scenes 0 at) (list scene) (nthcdr at scenes)))
                  (list :scene (getf scene :name) :row at)))))

(define-op rename-scene (scene name)
  "Name a scene."
  (edit-song! (song) (lambda (spec) (put! (find-scene spec scene) :name name)))
  (list :scene name))

(define-op delete-scene (scene)
  "Remove a scene and the clips in its slots."
  (edit-song! (song)
              (lambda (spec)
                (let ((found (find-scene spec scene)))
                  (dolist (track (getf spec :tracks))
                    (put! track :clips (remove (getf found :id) (getf track :clips)
                                               :key (lambda (c) (getf c :scene)))))
                  (put! spec :scenes (remove found (getf spec :scenes)))
                  (list :deleted (getf found :name))))))

(define-op duplicate-scene (scene &optional name)
  "A copy of a scene and its clips, in the row after it: the start of a variation."
  (edit-song! (song)
              (lambda (spec)
                (let* ((found (find-scene spec scene))
                       (copy (list :id (next-id! spec) :name (or name (format nil "~a copy" (getf found :name)))))
                       (scenes (getf spec :scenes))
                       (at (1+ (position found scenes))))
                  (put! spec :scenes (append (subseq scenes 0 at) (list copy) (nthcdr at scenes)))
                  (dolist (track (getf spec :tracks))
                    (let ((clip (slot-clip track (getf found :id))))
                      (when clip
                        (let ((new (copy-tree clip)))
                          (put! new :id (next-id! spec))
                          (put! new :scene (getf copy :id))
                          (put! track :clips (append (getf track :clips) (list new)))))))
                  (list :scene (getf copy :name) :row at)))))

(define-op set-launch-quantize (beats)
  "Launches wait for the next multiple of BEATS: 4 is a bar of 4/4, 1 a
beat, 0 at once."
  (unless (and (realp beats) (<= 0 beats 64))
    (error "The launch quantize is a number of beats from 0 to 64, not ~s." beats))
  (edit-song! (song) (lambda (spec) (put! spec :quantize beats)))
  (list :quantize beats))

(defun ui-track-index (spec track)
  (or (position (find-track spec track) (getf spec :tracks))
      (error "Only an instrument track plays clips; ~s is not one." track)))

(define-op launch (track scene)
  "Play the clip in TRACK's slot of SCENE, looping, from the next
launch boundary; an empty slot stops the track.  A performance on the
page, not an edit: undo does not take it back."
  (let ((spec (the-object (song) spec)))
    (push (list :launch (ui-track-index spec track) (scene-row spec scene)) *ui-actions*)
    (list :launching track :scene scene)))

(define-op launch-scene (scene)
  "Play every clip in SCENE's row together; tracks with an empty slot stop."
  (let ((spec (the-object (song) spec)))
    (push (list :scene (scene-row spec scene)) *ui-actions*)
    (list :launching-scene scene)))

(define-op stop-clips (&optional track)
  "Stop TRACK's clip, or every track's, at the next launch boundary."
  (let ((spec (the-object (song) spec)))
    (push (list :stop (if track (ui-track-index spec track) -1)) *ui-actions*)
    (list :stopping (or track :all))))

(define-op back-to-arrangement (&optional track)
  "TRACK, or every track, plays its arrangement again from where the song is."
  (let ((spec (the-object (song) spec)))
    (push (list :arrange (if track (ui-track-index spec track) -1)) *ui-actions*)
    (list :back-to-arrangement (or track :all))))

(define-op arrange-scene (scene start &key length)
  "Copy SCENE's clips onto the timeline from beat START, LENGTH beats
long (the longest clip's length when unsaid), each repeating as it
loops in the session: how a song is built from its scenes."
  (edit-song! (song)
              (lambda (spec)
                (let* ((found (find-scene spec scene))
                       (clips (loop for track in (getf spec :tracks)
                                    for clip = (slot-clip track (getf found :id))
                                    when clip collect (cons track clip)))
                       (span (or length (reduce #'max clips :key (lambda (c) (getf (cdr c) :length))
                                                            :initial-value 0))))
                  (unless clips
                    (error "Scene ~s has no clips." (getf found :name)))
                  (unless (and (realp start) (>= start 0) (plusp span))
                    (error "A scene is placed at a beat from 0 up, for a positive length."))
                  (loop for (track . clip) in clips
                        do (let ((new (copy-tree clip)))
                             (remf new :scene)
                             (put! new :id (next-id! spec))
                             (put! new :start start)
                             (if (getf clip :sample)
                                 (put! new :length (min span (getf clip :length)))
                                 (progn
                                   (put! new :loop (or (getf clip :loop) (getf clip :length)))
                                   (put! new :length span)))
                             (put! track :clips (append (getf track :clips) (list new)))))
                  (list :scene (getf found :name) :start start :length span :clips (length clips))))))

;;; Automation.

(defun lane-address (found device param)
  "The :device id and control a lane of FOUND points at, checked:
device nil means the track's :gain or :pan."
  (if device
      (let ((d (find-device found device)))
        (find-param (getf d :kind) param)
        (getf d :id))
      (progn
        (unless (member param '(:gain :pan))
          (error "With no device a lane moves the track's :gain or :pan, not ~(~s~)." param))
        nil)))

(define-op set-automation (track device param points)
  "PARAM of DEVICE (nil for the track's :gain or :pan) follows POINTS,
each (beat value), in straight lines from one to the next, over the
song's time, replacing any lane it had."
  (edit-song! (song)
              (lambda (spec)
                (let* ((found (find-track spec track))
                       (id (lane-address found device param))
                       (sorted (sort (mapcar (lambda (p)
                                               (unless (and (consp p) (realp (first p)))
                                                 (error "An automation point is (beat value), not ~s." p))
                                               (list (first p) (second p)))
                                             points)
                                     #'< :key #'first))
                       (kept (remove-if (lambda (lane)
                                          (and (eql (getf lane :device) id) (eq (getf lane :param) param)))
                                        (getf found :automation))))
                  (put! found :automation (append kept (list (list :device id :param param :points sorted))))
                  (list :track (getf found :name) :param param :points (length sorted))))))

(define-op clear-automation (track device param)
  "Remove a lane."
  (edit-song! (song)
              (lambda (spec)
                (let* ((found (find-track spec track))
                       (id (lane-address found device param)))
                  (put! found :automation
                        (remove-if (lambda (lane)
                                     (and (eql (getf lane :device) id) (eq (getf lane :param) param)))
                                   (getf found :automation)))
                  (list :cleared param)))))

;;; Undo.

(define-op undo ()
  "Take back the last change, or the whole of the last prompt.  Within a
prompt, after it has made a change, the last change it made."
  (if (and *journal* (car *journal*))
      ;; the prompt correcting itself: its own edits are not on the list yet
      (let ((song (song)))
        (the-object song (set-slot! :spec (revert-entry (pop (car *journal*)) (the-object song spec))))
        (list :undone t))
      (list :undone (undo-song! (song)))))

(define-op redo ()
  "Put back what undo took."
  (list :redone (redo-song! (song))))

;;; Showing the person.

(define-op point-at (target &optional say)
  "Light up a control on the page, with SAY in a note beside it: the
way to teach.  TARGET names it: \"tempo\", \"play\", \"loop\", \"browser\",
\"arrangement\", \"mixer\", \"track:NAME\", \"track:NAME:gain\", \"device:TRACK:LABEL\",
\"device:TRACK:LABEL:CONTROL\" or \"clip:ID\"."
  (push (list :point target say) *ui-actions*)
  (list :pointing-at target))

(define-op show (view &optional track clip)
  "Open a view: :arrangement, :mixer, or for TRACK :devices or, with
CLIP, :notes (the piano roll)."
  (unless (member view '(:arrangement :mixer :devices :notes))
    (error "The views are :arrangement, :mixer, :devices and :notes."))
  (push (list :show view track clip) *ui-actions*)
  (list :showing view))

(defun operators-for-prompt ()
  "What the model may run: every operation."
  (reverse *operators*))

(defparameter *reading-operators*
  (mapcar (lambda (name) (intern name :tala-api))
          '("DESCRIBE-SONG" "SONG-INFO" "DEVICE-KINDS" "DEVICE-CONTROLS" "DEVICE-VALUES" "CLIP-NOTES"
            "SAMPLES" "SAMPLE-INFO"
            "HEAR" "POINT-AT" "SHOW" "UNDO" "REDO"
            "LAUNCH" "LAUNCH-SCENE" "STOP-CLIPS" "BACK-TO-ARRANGEMENT"))
  "The operations that leave nothing for a prompt's undo to take back.")
