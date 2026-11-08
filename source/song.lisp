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

(defun song-beats-per-bar (spec)
  (* (getf spec :numerator) (/ 4 (getf spec :denominator))))

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
      (emit 0)
      (emit (song-beats-per-bar spec))
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

;;; Undo.  Two people can edit one song at once: the person on the page
;;; and the prompt's model, whose forms run for a minute while the page
;;; stays live.  So an undo entry is not a copy of the song to go back
;;; to, which would take back whatever else happened since; it is an
;;; edit, (:edit old new), and undoing it takes back only what changed
;;; from OLD to NEW, by a three-way merge into the song as it is now
;;; (merge3).  When nothing else has happened since, that is exactly OLD.
;;; A prompt's edits are one entry, (:group edit...), newest first.
;;;
;;; Undoing is itself an edit, from the song as it was to the song as it
;;; is, and that edit is what redo takes back, by the same merge.  Specs
;;; are never changed once set, so an entry shares them, not copies.

(defparameter *undo-depth* 200 "Edits kept for undo.")

(defvar *journal* nil
  "While a prompt runs, a cons whose car collects its edits, newest
first: the prompt's ending makes them one undo.")

;;; Versions share what they have in common.  An edit works on a private
;;; copy, and then every part of the copy equal to the old version is
;;; replaced by the old version's own: a version costs only what changed,
;;; so two hundred undo entries of a long song cost about one song, and
;;; the merge finds the unchanged parts by EQ at once.  It is safe because
;;; no version is changed once it is set: every edit copies first.

(defun share (new old)
  "NEW with each part equal to its counterpart in OLD replaced by that
counterpart: (values result same?), SAME? when all of NEW was OLD's.
Lists of things with ids are matched by id, so a thing deleted or added
before others does not stop them being shared."
  (cond ((eq new old) (values old t))
        ((and (consp new) (consp old))
         (if (and (id-list? new) (id-list? old))
             (let ((same (= (length new) (length old)))
                   (out '()))
               (loop for thing in new
                     for twin = (find (getf thing :id) old :key (lambda (e) (getf e :id)))
                     do (multiple-value-bind (part part-same) (if twin (share thing twin) (values thing nil))
                          (unless (and part-same (eq twin (nth (length out) old)))
                            (setf same nil))
                          (push part out)))
               (if same (values old t) (values (nreverse out) nil)))
             ;; element by element down the list, the tail kept when it is
             ;; the old one's
             (let ((same t) (out '()) (a new) (b old))
               (loop while (and (consp a) (consp b))
                     do (multiple-value-bind (part part-same) (share (car a) (car b))
                          (unless part-same (setf same nil))
                          (push part out))
                        (setf a (cdr a) b (cdr b)))
               (multiple-value-bind (tail tail-same) (if (or (consp a) (consp b))
                                                         (values a nil)
                                                         (share a b))
                 (if (and same tail-same)
                     (values old t)
                     (let ((result tail))
                       (dolist (part out result)
                         (push part result))))))))
        ((and (not (consp new)) (not (consp old)) (equal new old)) (values old t))
        (t (values new nil))))

(defun edit-song! (song change)
  "CHANGE, a function of a private copy of the spec, applied: what it
returned.  An error leaves the song as it was."
  (let* ((old (the-object song spec))
         (copy (copy-tree old))
         (result (funcall change copy))
         (new (share copy old)))
    (if *journal*
        (push (list :edit old new) (car *journal*))
        (push-undo! song (list :edit old new)))
    (the-object song (set-slot! :spec new))
    result))

(defun replace-spec! (song spec)
  "SPEC for SONG's whole spec, as one edit."
  (push-undo! song (list :edit (the-object song spec) spec))
  (the-object song (set-slot! :spec spec)))

(defun push-undo! (song entry)
  "ENTRY on the undo list; a new edit forgets what could be redone."
  (let ((entries (cons entry (the-object song undo-entries))))
    (the-object song (set-slot! :undo-entries
                                (if (> (length entries) *undo-depth*)
                                    (subseq entries 0 *undo-depth*)
                                    entries)))
    (the-object song (set-slot! :redo-entries nil))))

(defun revert-entry (entry current)
  "The spec CURRENT with ENTRY's change taken back, and the entry that
would take that back in turn: (values spec mirror)."
  (case (car entry)
    (:edit
     (destructuring-bind (old new) (rest entry)
       (let ((back (if (eq current new) old (merge3 new current old))))
         (values back (list :edit current back)))))
    (:group
     ;; newest first; the mirrors come out newest first too, for redo
     ;; takes back the last of them first
     (let ((spec current) (mirrors '()))
       (dolist (edit (rest entry))
         (multiple-value-bind (back mirror) (revert-entry edit spec)
           (push mirror mirrors)
           (setf spec back)))
       (values spec (cons :group mirrors))))
    ;; an entry from before edits were pairs: a copy of the song
    (t (revert-entry (list :edit entry current) current))))

(defun undo-song! (song)
  "Back one change, leaving anything done since it; true when there was one."
  (let ((entries (the-object song undo-entries)))
    (when entries
      (multiple-value-bind (spec mirror) (revert-entry (first entries) (the-object song spec))
        (the-object song (set-slot! :undo-entries (rest entries)))
        (the-object song (set-slot! :redo-entries (cons mirror (the-object song redo-entries))))
        (the-object song (set-slot! :spec spec))
        t))))

(defun redo-song! (song)
  "Forward one change: the undo before it taken back; true when there was one."
  (let ((entries (the-object song redo-entries)))
    (when entries
      (multiple-value-bind (spec mirror) (revert-entry (first entries) (the-object song spec))
        (the-object song (set-slot! :redo-entries (rest entries)))
        (the-object song (set-slot! :undo-entries (cons mirror (the-object song undo-entries))))
        (the-object song (set-slot! :spec spec))
        t))))

;;; The merge.  OURS is the song now, BASE what an edit made, THEIRS what
;;; it started from: the result is OURS with the edit's own changes taken
;;; back.  Where OURS changed the same thing since, OURS wins, since it
;;; is the later word.  Plists merge key by key; a list of plists with
;;; ids (tracks, devices, clips, scenes, samples) merges thing by thing,
;;; matched by id; anything else (notes, points, sends) is one value.

(defun plist? (x)
  (and (listp x)
       (evenp (length x))
       (loop for (k) on x by #'cddr always (keywordp k))))

(defun id-list? (x)
  (and (listp x)
       (every (lambda (e) (and (consp e) (plist? e) (getf e :id))) x)))

(defun merge3 (base ours theirs)
  "OURS with the changes from BASE to THEIRS made, where OURS has not
changed the same thing since BASE."
  (cond ((equal ours base) theirs)
        ((or (equal theirs base) (equal ours theirs)) ours)
        ((and (consp ours) (consp theirs) (id-list? base) (id-list? ours) (id-list? theirs))
         (merge-things base ours theirs))
        ((and (consp ours) (consp theirs) (plist? base) (plist? ours) (plist? theirs))
         (merge-plists base ours theirs))
        (t ours)))

(defun merge-plists (base ours theirs)
  (let ((keys (remove-duplicates (append (loop for (k) on ours by #'cddr collect k)
                                         (loop for (k) on theirs by #'cddr collect k))
                                 :from-end t))
        (absent '#:absent))
    (loop for key in keys
          for value = (merge3 (getf base key absent) (getf ours key absent) (getf theirs key absent))
          unless (eq value absent)
            append (list key value))))

(defun merge-things (base ours theirs)
  (flet ((by-id (list id) (find id list :key (lambda (e) (getf e :id))))
         (ids (list) (mapcar (lambda (e) (getf e :id)) list)))
    (let* ((kept
             ;; what OURS has, merged; what the edit added and OURS left as
             ;; it was goes, but not what OURS has worked on since
             (loop for mine in ours
                   for id = (getf mine :id)
                   for was = (by-id base id)
                   for then = (by-id theirs id)
                   nconc (cond ((and was then) (list (merge3 was mine then)))
                               ((and was (not then)) (unless (equal was mine) (list mine)))
                               (t (list mine)))))
           ;; what the edit took away comes back
           (restored (remove-if (lambda (e) (or (by-id base (getf e :id)) (by-id ours (getf e :id))))
                                theirs))
           (order (if (equal (ids ours) (ids base))
                      ;; OURS kept the order, so the edit's order stands
                      (ids theirs)
                      (ids ours)))
           (result (append (remove-if-not (lambda (e) (member (getf e :id) order)) kept)
                           (remove-if (lambda (e) (member (getf e :id) order)) kept))))
      (setf result (stable-sort (copy-list result) #'<
                                :key (lambda (e) (or (position (getf e :id) order) most-positive-fixnum))))
      ;; each restored thing after the one before it in THEIRS
      (dolist (thing restored result)
        (let* ((at (position (getf thing :id) theirs :key (lambda (e) (getf e :id))))
               (before (loop for i from (1- at) downto 0
                             for id = (getf (nth i theirs) :id)
                             when (by-id result id) return id))
               (place (if before (1+ (position before result :key (lambda (e) (getf e :id)))) 0)))
          (setf result (append (subseq result 0 place) (list thing) (nthcdr place result))))))))

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

(defun render-song (song frames &key (sr 48000) (from 0) block-size script metronome)
  "FRAMES frames of SONG from beat FROM: (values left right).  SCRIPT
is what is launched when: ((frame :launch track row)...); METRONOME
clicks the beats, as the page does when its metronome is on."
  (render-plan (the-object song plan)
               (let ((params (copy-seq (the-object song params))))
                 (when metronome (setf (aref params gp-metronome) 1d0))
                 params)
               (the-object song score) frames
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
