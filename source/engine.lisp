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
;; Plan and parameters, as in Stratum.  A song's PLAN is its structure
;; alone: which tracks, which instrument on each, which devices in what
;; order, what ducks what.  From the plan comes one program, in one
;; form, that both runtimes take: Parenscript makes it the AudioWorklet
;; the browser plays and COMPILE makes it the renderer the server
;; bounces with.  Everything else -- every knob, note and automation
;; point -- is data that program reads, in P (the parameters) and SC
;; (the score), so a knob or a note never rebuilds anything; *BUILDS*
;; counts, and the tests hold it to that.
;;
;; S is the engine's whole state, one array of doubles laid out by
;; PLAN-LAYOUT.  Nothing allocates while it runs, which on the audio
;; thread is the difference between sound and a dropout.
;;
;; The score, SC: [tracks, offset of the lanes, offset of the regions,
;; offset of the session table, then for each track its offset and count,
;; the notes (beat length pitch velocity), the session clips' notes, the
;; lanes: count, and each lane's parameter index, count, (beat
;; value)..., the regions, six numbers each (devices.lisp), and the
;; session table: for each track the offset and count of its slots, and
;; for each slot the offset and count of its notes and its length, zero
;; for an empty slot].  POOL is the song's recorded sound (samples.lisp).
;;
;; The session: a track plays its arrangement (MODE 0) until a clip is
;; launched on it (MODE 1), and then that clip, looping, until it is
;; sent back.  A launch waits for the next multiple of the song's
;; QUANTIZE beats on the CLOCK, so it lands on the bar however late the
;; click came; SLOT and QUEUED hold a row plus one, so zero is none and
;; a QUEUED of -1 stops the track.
;;

(define-slots tr bus-l bus-r gl gr cl cr ready peak idx mode slot origin sidx queued qat out-l out-r send0 send1 send2 send3 send4 send5 send6 send7)
(define-slots tp gain pan active send0 send1 send2 send3 send4 send5 send6 send7)

(defparameter *most-returns* 8 "The return tracks a song may have.")

(defparameter *sample-rate* 48000
  "The rate of everything: the page asks its AudioContext for this one,
so the worklet and the server run the same frames.  The browser
resamples for the device, if it must, after the engine.")

(define-dsp transport-tick ((s :buf) (p :buf)) :nil
  (setf (sg g-bpf) (/ (aref p gp-tempo) (* 60d0 (sg g-sr))))
  (setf (sg g-loop-on) (aref p gp-loop-on))
  (setf (sg g-loop-start) (aref p gp-loop-start))
  (setf (sg g-loop-end) (aref p gp-loop-end)))

(define-dsp engine-setup ((s :buf) (sr :num)) :nil
  (setf (sg g-sr) sr)
  (setf (sg g-isr) (/ 1d0 sr)))

(define-dsp transport-start ((s :buf) (beat :num)) :nil
  (setf (sg g-beat) beat)
  (setf (sg g-clock) beat)
  (setf (sg g-seek) beat)
  (setf (sg g-reseek) 1d0)
  (setf (sg g-playing) 1d0))

(define-dsp transport-stop ((s :buf)) :nil
  (setf (sg g-playing) 0d0))

(define-dsp frame-step ((s :buf)) :nil
  (let* ((tick (+ (sg g-tick) 1d0))
         (sub (+ (sg g-sub) 1d0)))
    (setf (sg g-tick) (if (>= tick 64d0) 0d0 tick))
    (setf (sg g-sub) (if (>= sub 16d0) 0d0 sub)))
  (when (> (sg g-playing) 0.5d0)
    (let* ((beat (+ (sg g-beat) (sg g-bpf)))
           (from (sg g-loop-start))
           (to (sg g-loop-end)))
      (setf (sg g-clock) (+ (sg g-clock) (sg g-bpf)))
      (if (and (> (sg g-loop-on) 0.5d0) (> to from) (>= beat to))
          (progn
            (setf (sg g-beat) (- beat (- to from)))
            (setf (sg g-seek) from)
            (setf (sg g-reseek) 1d0))
          (setf (sg g-beat) beat)))))

(define-dsp seq-seek ((s :buf) (sc :buf) (b :int) (ti :int)) :nil
  "Track TI's next note: the first at or after the seek beat."
  (let* ((o (ifloor (aref sc (+ 4 (* 2 ti)))))
         (n (ifloor (aref sc (+ 5 (* 2 ti)))))
         (seek (sg g-seek))
         (i 0))
    (loop while (and (< i n) (< (aref sc (+ o (* 4 i))) seek))
          do (setf i (+ i 1)))
    (setf (st tr-idx) (dfloat i))))

(define-dsp seq-due ((s :buf) (sc :buf) (b :int) (ti :int)) :inline
  "A positive number when track TI's next note has come."
  (let* ((o (ifloor (aref sc (+ 4 (* 2 ti)))))
         (n (ifloor (aref sc (+ 5 (* 2 ti)))))
         (i (ifloor (st tr-idx))))
    (if (and (< i n) (<= (aref sc (+ o (* 4 i))) (sg g-beat))) 1d0 0d0)))

(define-dsp seq-note ((s :buf) (sc :buf) (b :int) (ti :int)) :nil
  "Past track TI's current note."
  (setf (st tr-idx) (+ (st tr-idx) 1d0)))

(define-dsp seq-offset ((s :buf) (sc :buf) (b :int) (ti :int)) :num
  "Where track TI's current note is in the score, as a double."
  (+ (aref sc (+ 4 (* 2 ti))) (* 4d0 (dfloor (st tr-idx)))))

;;; The session.

(define-dsp session-slot ((sc :buf) (ti :int) (row :num)) :inline
  "Where row ROW of track TI's slots is in the score, or -1 when the slot
is empty or there is no such row."
  (let* ((t0 (+ (ifloor (aref sc 3)) (* 2 ti)))
         (base (ifloor (aref sc t0)))
         (count (ifloor (aref sc (+ t0 1))))
         (r (ifloor row)))
    (if (and (>= r 0) (< r count) (> (aref sc (+ base (* 3 r) 2)) 0d0))
        (dfloat (+ base (* 3 r)))
        -1d0)))

(define-dsp session-queue ((s :buf) (p :buf) (b :int) (row :num)) :nil
  "Launch row ROW on the track at B at the next boundary; -1 stops it."
  (let* ((q (aref p gp-quantize))
         (clock (sg g-clock)))
    (setf (st tr-queued) (if (< row 0d0) -1d0 (+ row 1d0)))
    (setf (st tr-qat) (if (> q 0d0)
                          (* q (dfloor (/ (+ clock q -0.000001d0) q)))
                          clock))))

(define-dsp session-launch ((s :buf) (b :int)) :inline
  "A positive number when a queued launch takes place this frame."
  (let* ((queued (st tr-queued))
         (fire (and (not (= queued 0d0)) (>= (sg g-clock) (st tr-qat)))))
    (when fire
      (setf (st tr-mode) 1d0)
      (setf (st tr-slot) (if (< queued 0d0) 0d0 queued))
      (setf (st tr-origin) (st tr-qat))
      (setf (st tr-sidx) 0d0)
      (setf (st tr-queued) 0d0))
    (if fire 1d0 0d0)))

(define-dsp session-wrap ((s :buf) (sc :buf) (b :int) (ti :int)) :nil
  "The playing clip begun again each time its length has passed."
  (when (and (> (st tr-mode) 0.5d0) (> (st tr-slot) 0.5d0))
    (let* ((at (session-slot sc ti (- (st tr-slot) 1d0))))
      (if (< at 0d0)
          ;; its clip was taken away
          (setf (st tr-slot) 0d0)
          (let* ((len (aref sc (+ (ifloor at) 2))))
            (loop while (>= (- (sg g-clock) (st tr-origin)) len)
                  do (setf (st tr-origin) (+ (st tr-origin) len))
                     (setf (st tr-sidx) 0d0)))))))

(define-dsp session-due ((s :buf) (sc :buf) (b :int) (ti :int)) :inline
  "A positive number when the playing clip's next note has come."
  (let* ((result 0d0))
    (when (and (> (st tr-mode) 0.5d0) (> (st tr-slot) 0.5d0))
      (let* ((at (session-slot sc ti (- (st tr-slot) 1d0))))
        (when (>= at 0d0)
          (let* ((a (ifloor at))
                 (o (ifloor (aref sc a)))
                 (n (ifloor (aref sc (+ a 1))))
                 (i (ifloor (st tr-sidx))))
            (when (and (< i n) (<= (aref sc (+ o (* 4 i))) (- (sg g-clock) (st tr-origin))))
              (setf result 1d0))))))
    result))

(define-dsp session-event ((s :buf) (sc :buf) (b :int) (ti :int)) :num
  "Where the playing clip's current note is in the score."
  (let* ((a (ifloor (session-slot sc ti (- (st tr-slot) 1d0)))))
    (+ (aref sc a) (* 4d0 (dfloor (st tr-sidx))))))

(define-dsp session-seek ((s :buf) (sc :buf) (b :int) (ti :int)) :nil
  "The playing clip's next note found again, after its notes changed."
  (when (and (> (st tr-mode) 0.5d0) (> (st tr-slot) 0.5d0))
    (let* ((at (session-slot sc ti (- (st tr-slot) 1d0))))
      (when (>= at 0d0)
        (let* ((a (ifloor at))
               (o (ifloor (aref sc a)))
               (n (ifloor (aref sc (+ a 1))))
               (pos (- (sg g-clock) (st tr-origin)))
               (i 0))
          (loop while (and (< i n) (< (aref sc (+ o (* 4 i))) pos))
                do (setf i (+ i 1)))
          (setf (st tr-sidx) (dfloat i)))))))

(define-dsp session-arrange ((s :buf) (b :int)) :nil
  "The track back to its arrangement, from where the song is now."
  (setf (st tr-mode) 0d0)
  (setf (st tr-slot) 0d0)
  (setf (st tr-queued) 0d0)
  (setf (sg g-seek) (sg g-beat))
  (setf (sg g-reseek) 1d0))

(define-dsp session-stop ((s :buf) (b :int)) :nil
  "Nothing playing or waiting on the track; it keeps its mode."
  (setf (st tr-slot) 0d0)
  (setf (st tr-queued) 0d0))

(define-dsp lane-value ((sc :buf) (q :int) (m :int) (beat :num)) :num
  "The lane of M points at SC[Q] at BEAT: a straight line from point to
point, level before the first and after the last."
  (let* ((i 0))
    (loop while (and (< i m) (<= (aref sc (+ q (* 2 i))) beat))
          do (setf i (+ i 1)))
    (if (= i 0)
        (aref sc (+ q 1))
        (if (= i m)
            (aref sc (+ q (* 2 (- m 1)) 1))
            (let* ((a (+ q (* 2 (- i 1))))
                   (b0 (aref sc a))
                   (b1 (aref sc (+ a 2)))
                   (v0 (aref sc (+ a 1)))
                   (v1 (aref sc (+ a 3))))
              (+ v0 (* (- v1 v0) (/ (- beat b0) (max 0.000001d0 (- b1 b0))))))))))

(define-dsp auto-apply ((s :buf) (p :buf) (sc :buf)) :nil
  "Every automation lane written into the parameters."
  (let* ((o (ifloor (aref sc 1)))
         (n (ifloor (aref sc o)))
         (i (+ o 1))
         (beat (sg g-beat)))
    (dotimes (l n)
      (let* ((m (ifloor (aref sc (+ i 1))))
             (q (+ i 2)))
        (when (> m 0)
          (setf (aref p (ifloor (aref sc i))) (lane-value sc q m beat)))
        (setf i (+ q (* 2 m)))))))

(define-dsp track-tick ((s :buf) (p :buf) (b :int) (pb :int)) :nil
  (let* ((g (* (db-gain (pv tp-gain)) (pv tp-active) 1.4142135623730951d0))
         (a (* (+ (pv tp-pan) 1d0) 0.125d0)))
    (setf (st tr-gl) (* g (d-sin (+ a 0.25d0))))
    (setf (st tr-gr) (* g (d-sin a)))
    ;; a fader starts where it stands, or the first hit fades in
    (when (< (st tr-ready) 0.5d0)
      (setf (st tr-ready) 1d0)
      (setf (st tr-cl) (st tr-gl))
      (setf (st tr-cr) (st tr-gr)))
    (dotimes (i 8)
      (let* ((db (aref p (+ pb tp-send0 i))))
        (setf (aref s (+ b tr-send0 i)) (if (> db -59.9d0) (db-gain db) 0d0))))))

(define-dsp track-out ((s :buf) (b :int) (dest :int)) :nil
  "The track through its fader into the bus at DEST."
  (let* ((gl (+ (st tr-cl) (* 0.003d0 (- (st tr-gl) (st tr-cl)))))
         (gr (+ (st tr-cr) (* 0.003d0 (- (st tr-gr) (st tr-cr)))))
         (x (* (st tr-bus-l) gl))
         (y (* (st tr-bus-r) gr))
         ;; a NaN must not reach the output, or stay there
         (l (if (= x x) x 0d0))
         (r (if (= y y) y 0d0)))
    (setf (st tr-cl) gl)
    (setf (st tr-cr) gr)
    (setf (st tr-out-l) l)
    (setf (st tr-out-r) r)
    (setf (st tr-peak) (max (abs l) (abs r) (* (st tr-peak) 0.9997d0)))
    (when (>= dest 0)
      (setf (aref s dest) (+ (aref s dest) l))
      (setf (aref s (+ dest 1)) (+ (aref s (+ dest 1)) r)))))

(define-dsp track-send ((s :buf) (b :int) (i :int) (dest :int)) :nil
  "The track, after its fader, into return I's bus at DEST."
  (let* ((level (aref s (+ b tr-send0 i))))
    (when (> level 0d0)
      (setf (aref s dest) (+ (aref s dest) (* (st tr-out-l) level)))
      (setf (aref s (+ dest 1)) (+ (aref s (+ dest 1)) (* (st tr-out-r) level))))))

;;; The plan's layout: where each thing's state and parameters live.

(defun param-count (kind)
  (length (device-params kind)))

(defun plan-layout (plan sr)
  "PLAN, a list of track plans, each (:kind :instrument :devices), with
every track and device given its place: (values placed state-size
param-size).  A device's on switch sits just before its parameters."
  (let ((state g-size)
        (param gp-size))
    (flet ((place-device (kind &rest more)
             (let* ((d (device-kind kind))
                    (size (funcall (device-kind-state d) sr))
                    (placed (list* :kind kind :state state :param (1+ param) more)))
               (incf state size)
               (incf param (1+ (param-count kind)))
               placed)))
      (let ((placed
              (loop for track in plan
                    for tb = state
                    for pb = param
                    do (incf state tr-size)
                       (incf param tp-size)
                    collect (list :kind (getf track :kind) :state tb :param pb
                                  :instrument (when (getf track :instrument)
                                                (place-device (getf track :instrument)))
                                  :devices (mapcar (lambda (d)
                                                     (place-device (getf d :kind) :key (getf d :key)))
                                                   (getf track :devices))))))
        (values placed state param)))))

(defun tracks-of-kind (placed kind)
  (remove kind placed :key (lambda (x) (getf x :kind)) :test-not #'eq))

(defun note-on-form (instrument)
  "The call that starts a note on INSTRUMENT, placed, with PITCH VEL GATE."
  (let ((b (getf instrument :state))
        (pb (getf instrument :param)))
    (ecase (getf instrument :kind)
      (:synth `(synth-note-on s p ,b ,pb pitch vel gate))
      (:drums `(drum-note-on s p ,b ,pb pitch vel gate))
      ;; a clip's event carries its region where a note has a pitch
      (:sampler `(sampler-start s sc ,b pitch vel gate 0d0)))))

(defun release-forms (instrument)
  "What ends INSTRUMENT's sounding notes, for a launch."
  (let ((b (getf instrument :state)))
    (case (getf instrument :kind)
      (:synth `((synth-note-off s ,b -1d0)))
      (:sampler `((sampler-release s ,b)))
      (t '(nil)))))

(defun instrument-run-form (instrument bus)
  "The call that makes INSTRUMENT's sound on the bus at BUS."
  (let ((b (getf instrument :state))
        (pb (getf instrument :param)))
    (if (eq (getf instrument :kind) :sampler)
        `(sampler-run s pool ,b ,bus)
        `(,(device-kind-run (device-kind (getf instrument :kind))) s p ,b ,pb ,bus))))

(defun device-run-form (device own-bus placed)
  "The call that runs DEVICE, placed, on the bus at OWN-BUS, when it is on."
  (let* ((d (device-kind (getf device :kind)))
         (key (getf device :key)))
    `(when (> (aref p ,(1- (getf device :param))) 0.5d0)
       (,(device-kind-run d) s p ,(getf device :state) ,(getf device :param) ,own-bus
        ,@(when (eq (getf device :kind) :compressor)
            (list (if key
                      (+ (getf (nth key placed) :state) tr-bus-l)
                      own-bus)))))))

(defun tick-forms (track)
  "The control-rate updates of TRACK, placed."
  (append (list `(track-tick s p ,(getf track :state) ,(getf track :param)))
          (loop for d in (cons (getf track :instrument) (getf track :devices))
                for tick = (and d (device-kind-tick (device-kind (getf d :kind))))
                when tick collect `(,tick s p ,(getf d :state) ,(getf d :param)))))

(defun clear-forms (track)
  (let ((bus (+ (getf track :state) tr-bus-l)))
    `((setf (aref s ,bus) 0d0)
      (setf (aref s ,(1+ bus)) 0d0))))

(defun track-frame-forms (track index placed master-bus returns)
  "One frame of the instrument track numbered INDEX."
  (let* ((tb (getf track :state))
         (bus (+ tb tr-bus-l))
         (instrument (getf track :instrument)))
    `(,@(clear-forms track)
      ,@(when instrument
          `((when (> (sg g-playing) 0.5d0)
              ;; a launch ends what the track was playing
              (when (> (session-launch s ,tb) 0.5d0)
                ,@(release-forms instrument))
              (session-wrap s sc ,tb ,index)
              (loop while (> (session-due s sc ,tb ,index) 0.5d0)
                    do (let* ((e (ifloor (session-event s sc ,tb ,index)))
                              (pitch (aref sc (+ e 2)))
                              (vel (aref sc (+ e 3)))
                              (gate (aref sc (+ e 1))))
                         ,(note-on-form instrument)
                         (setf (aref s ,(+ tb tr-sidx)) (+ (aref s ,(+ tb tr-sidx)) 1d0))))
              (when (< (aref s ,(+ tb tr-mode)) 0.5d0)
                (loop while (> (seq-due s sc ,tb ,index) 0.5d0)
                      do (let* ((e (ifloor (seq-offset s sc ,tb ,index)))
                                (pitch (aref sc (+ e 2)))
                                (vel (aref sc (+ e 3)))
                                (gate (aref sc (+ e 1))))
                           ,(note-on-form instrument)
                           (seq-note s sc ,tb ,index)))))
            ,(instrument-run-form instrument bus)))
      ,@(mapcar (lambda (d) (device-run-form d bus placed)) (getf track :devices))
      (track-out s ,tb ,master-bus)
      ,@(loop for r in returns
              for i from 0
              collect `(track-send s ,tb ,i ,(+ (getf r :state) tr-bus-l))))))

(defun engine-forms (plan sr)
  "The program for PLAN at SR: a list of (name lambda-list . body), with
the state and parameter sizes after it as values."
  (multiple-value-bind (placed state-size param-size) (plan-layout plan sr)
    (let* ((tracks (tracks-of-kind placed :midi))
           (returns (tracks-of-kind placed :return))
           (master (first (tracks-of-kind placed :master)))
           (master-bus (+ (getf master :state) tr-bus-l))
           (forms
             `((engine-tick (s p sc)
                 (transport-tick s p)
                 (when (> (sg g-playing) 0.5d0)
                   (auto-apply s p sc))
                 ,@(mapcan #'tick-forms (copy-list placed))
                 nil)
               (engine-frames (s p sc pool ol orr n)
                 (dotimes (f n)
                   (when (< (sg g-tick) 0.5d0)
                     (engine-tick s p sc))
                   (when (> (sg g-reseek) 0.5d0)
                     ,@(loop for track in tracks
                             for i from 0
                             collect `(seq-seek s sc ,(getf track :state) ,i)
                             when (eq (getf (getf track :instrument) :kind) :sampler)
                               collect `(sampler-chase s sc ,(getf (getf track :instrument) :state) ,i))
                     (setf (sg g-reseek) 0d0))
                   (when (> (sg g-refind) 0.5d0)
                     ,@(loop for track in tracks
                             for i from 0
                             collect `(session-seek s sc ,(getf track :state) ,i))
                     (setf (sg g-refind) 0d0))
                   ,@(loop for track in tracks
                           for i from 0
                           append (track-frame-forms track i placed master-bus returns))
                   ,@(loop for r in returns
                           append (mapcar (lambda (d) (device-run-form d (+ (getf r :state) tr-bus-l) placed))
                                          (getf r :devices))
                           collect `(track-out s ,(getf r :state) ,master-bus)
                           append (clear-forms r))
                   ,@(mapcar (lambda (d) (device-run-form d master-bus placed))
                             (getf master :devices))
                   (track-out s ,(getf master :state) -1)
                   ,@(clear-forms master)
                   (setf (aref ol f) (aref s ,(+ (getf master :state) tr-out-l)))
                   (setf (aref orr f) (aref s ,(+ (getf master :state) tr-out-r)))
                   (frame-step s))
                 nil)
               (engine-init (s)
                 ;; each noise its own seed, by track: a seed taken from
                 ;; where the state sits would change the sound whenever
                 ;; the layout grew
                 ,@(loop for track in tracks
                         for i from 0
                         for instrument = (getf track :instrument)
                         for b = (getf instrument :state)
                         when (eq (getf instrument :kind) :synth)
                           collect `(setf (aref s ,(+ b sy-noise)) ,(float (+ 12345 (* 7919 i)) 1d0))
                         when (eq (getf instrument :kind) :drums)
                           collect `(setf (aref s ,(+ b dk-noise)) ,(float (+ 54321 (* 7919 i)) 1d0)))
                 nil)
               (engine-note (s p ti pitch vel gate)
                 (cond ,@(loop for track in tracks
                               for i from 0
                               unless (eq (getf (getf track :instrument) :kind) :sampler)
                                 collect `((= ti ,i) ,(note-on-form (getf track :instrument)))))
                 nil)
               (engine-note-off (s ti pitch)
                 (cond ,@(loop for track in tracks
                               for i from 0
                               for instrument = (getf track :instrument)
                               when (eq (getf instrument :kind) :synth)
                                 collect `((= ti ,i) (synth-note-off s ,(getf instrument :state) pitch))
                               when (eq (getf instrument :kind) :sampler)
                                 collect `((= ti ,i) (sampler-release s ,(getf instrument :state)))))
                 nil))))
      (values (mapcar #'normalize-dsp forms) state-size param-size placed))))

;;; The two builds.  Each is kept by its plan, so a song that comes
;;; back to a structure it had pays nothing.

(defvar *builds* (list :lisp 0 :js 0)
  "How many programs have been built, of each kind.  The tests read it:
a parameter or a note must not add to it.")

(defvar *engines-lock* (bt:make-lock "tala engines"))

(defvar *lisp-engines* (make-hash-table :test 'equal))
(defvar *js-engines* (make-hash-table :test 'equal))

(defparameter *most-engines* 64
  "Compiled plans kept before the table starts over.")

(defun plan-key (plan sr)
  (let ((*print-pretty* nil) (*print-readably* nil) (*print-case* :downcase))
    (format nil "~d ~s" sr plan)))

(defun fnv-hex (string)
  "A short stable name for STRING, the same on every Lisp: 32-bit FNV-1a."
  (let ((h 2166136261))
    (loop for c across string
          do (setf h (logand #xffffffff (* 16777619 (logxor h (logand #xff (char-code c)))))))
    (format nil "~(~8,'0x~)" h)))

(defstruct (engine (:constructor make-engine (frames tick init state-size param-size placed)))
  "A compiled plan: FRAMES runs it, INIT seeds it, STATE-SIZE and
PARAM-SIZE are its arrays' lengths, PLACED its layout."
  frames tick init state-size param-size placed)

(defun lisp-lambda-form (forms)
  "FORMS as one function that returns the engine's two entry points."
  (let ((buffer '(simple-array double-float (*))))
    (flet ((clause (name)
             (destructuring-bind (lambda-list . body) (cdr (assoc name forms))
               `(,name ,lambda-list
                       (declare (ignorable ,@lambda-list)
                                ,@(loop for v in lambda-list
                                        for type = (case v
                                                     ((s p sc ol orr) buffer)
                                                     (pool '(simple-array single-float (*)))
                                                     (t 'fixnum))
                                        collect `(type ,type ,v)))
                       ,@body))))
      `(lambda ()
         (declare (optimize (speed 1) (safety 1) (debug 0) (compilation-speed 2)))
         (labels (,(clause 'engine-tick)
                  ,(clause 'engine-frames)
                  ,(clause 'engine-init))
           (list #'engine-frames #'engine-tick #'engine-init))))))

(defun lisp-engine (plan sr)
  "The renderer for PLAN at SR, built when it is new."
  (let ((key (plan-key plan sr)))
    (bt:with-lock-held (*engines-lock*)
      (or (gethash key *lisp-engines*)
          (multiple-value-bind (forms state-size param-size placed) (engine-forms plan sr)
            (when (>= (hash-table-count *lisp-engines*) *most-engines*)
              (clrhash *lisp-engines*))
            (incf (getf *builds* :lisp))
            (destructuring-bind (frames tick init) (funcall (compile nil (lisp-lambda-form forms)))
              (setf (gethash key *lisp-engines*)
                    (make-engine frames tick init state-size param-size placed))))))))

(defparameter *worklet-class*
  "class TalaEngine extends AudioWorkletProcessor {
  constructor(options) {
    super();
    const o = options.processorOptions;
    this.s = new Float64Array(STATE_SIZE);
    this.p = Float64Array.from(o.params);
    this.sc = Float64Array.from(o.score);
    this.pool = o.pool;
    this.script = o.script || [];
    this.si = 0;
    this.frame = 0;
    this.quanta = 0;
    engineSetup(this.s, sampleRate);
    engineInit(this.s);
    if (o.playing) transportStart(this.s, o.beat);
    else this.s[G_BEAT] = o.beat;
    this.port.onmessage = (e) => this.message(e.data);
  }
  message(m) {
    const s = this.s;
    switch (m.t) {
    case 'params': this.p.set(m.params); break;
    case 'param': this.p[m.i] = m.v; break;
    case 'pool': this.pool = m.pool; break;
    case 'score':
      this.sc = Float64Array.from(m.score);
      s[G_SEEK] = s[G_BEAT]; s[G_RESEEK] = 1; s[G_REFIND] = 1;
      break;
    case 'play': transportStart(s, m.beat); break;
    case 'stop':
      transportStop(s);
      for (let i = 0; i < TRACKS; i++) { engineNoteOff(s, i, -1); sessionStop(s, TRACK_STATE[i]); }
      break;
    case 'seek': s[G_BEAT] = m.beat; s[G_SEEK] = m.beat; s[G_RESEEK] = 1; break;
    case 'note': engineNote(s, this.p, m.track, m.pitch, m.vel, -1); break;
    case 'noteoff': engineNoteOff(s, m.track, m.pitch); break;
    case 'launch': sessionQueue(s, this.p, TRACK_STATE[m.track], m.row); break;
    case 'arrange':
      for (let i = 0; i < TRACKS; i++) {
        if (m.track < 0 || m.track === i) { sessionArrange(s, TRACK_STATE[i]); engineNoteOff(s, i, -1); }
      }
      break;
    }
  }
  // what a script asks at frame F, as a message would: for tests that
  // hold a performance to the server's render
  render(out, n) {
    let at = 0;
    while (at < n) {
      while (this.si < this.script.length && this.script[this.si][0] <= this.frame + at) {
        const [, t, track, row] = this.script[this.si++];
        this.message({ t, track, row });
      }
      const next = this.si < this.script.length ? Math.min(n, this.script[this.si][0] - this.frame) : n;
      if (at === 0 && next === n) {
        engineFrames(this.s, this.p, this.sc, this.pool, out[0], out[1] || out[0], n);
      } else {
        engineFrames(this.s, this.p, this.sc, this.pool, out[0].subarray(at, next),
                     (out[1] || out[0]).subarray(at, next), next - at);
      }
      at = next;
    }
    this.frame += n;
  }
  process(inputs, outputs) {
    const out = outputs[0];
    this.render(out, out[0].length);
    if (++this.quanta >= 12) {
      this.quanta = 0;
      const s = this.s;
      const peaks = new Array(METERS.length);
      for (let i = 0; i < METERS.length; i++) peaks[i] = s[METERS[i]];
      const session = new Array(TRACKS);
      for (let i = 0; i < TRACKS; i++) {
        const b = TRACK_STATE[i];
        session[i] = [s[b + TR_MODE], s[b + TR_SLOT], s[b + TR_QUEUED], s[b + TR_ORIGIN]];
      }
      this.port.postMessage({ beat: s[G_BEAT], clock: s[G_CLOCK], playing: s[G_PLAYING] > 0.5, peaks, session });
    }
    return true;
  }
}
"
  "The worklet around the generated functions.")

(defun worklet-by-name (name)
  "The module of the processor NAME, when it has been built."
  (bt:with-lock-held (*engines-lock*)
    (gethash name *js-engines*)))

(defun worklet-js (plan sr)
  "The AudioWorklet module for PLAN at SR, built when it is new; the
processor registers under a name of its own, since a name cannot be
used twice in a context: (values js processor-name)."
  (let* ((name (format nil "tala-~a" (fnv-hex (plan-key plan sr)))))
    (values
     (bt:with-lock-held (*engines-lock*)
       (or (gethash name *js-engines*)
           (multiple-value-bind (forms state-size param-size placed) (engine-forms plan sr)
             (declare (ignore param-size))
             (when (>= (hash-table-count *js-engines*) *most-engines*)
               (clrhash *js-engines*))
             (incf (getf *builds* :js))
             (setf (gethash name *js-engines*)
                   (with-output-to-string (out)
                     (format out "'use strict';~%")
                     (write-string (dsp-js) out)
                     (dolist (form forms)
                       (write-string (ps-text `(defun ,(car form) ,(cadr form) ,@(cddr form))) out)
                       (terpri out))
                     (format out "const STATE_SIZE = ~d;~%const TRACKS = ~d;~%" state-size
                             (length (tracks-of-kind placed :midi)))
                     (format out "const G_BEAT = ~d, G_PLAYING = ~d, G_SEEK = ~d, G_RESEEK = ~d, G_CLOCK = ~d, G_REFIND = ~d;~%"
                             g-beat g-playing g-seek g-reseek g-clock g-refind)
                     (format out "const TR_MODE = ~d, TR_SLOT = ~d, TR_QUEUED = ~d, TR_ORIGIN = ~d;~%"
                             tr-mode tr-slot tr-queued tr-origin)
                     (format out "const TRACK_STATE = [~{~d~^, ~}];~%"
                             (mapcar (lambda (track) (getf track :state)) (tracks-of-kind placed :midi)))
                     (format out "const METERS = [~{~d~^, ~}];~%"
                             (mapcar (lambda (track) (+ (getf track :state) tr-peak)) placed))
                     (write-string *worklet-class* out)
                     (format out "registerProcessor(~s, TalaEngine);~%" name))))))
     name)))

;;; The offline render.

(defun double-buffer (n)
  (make-array n :element-type 'double-float :initial-element 0d0))

(defun double-vector (sequence)
  "SEQUENCE as a buffer the engine can read."
  (map '(simple-array double-float (*)) (lambda (x) (float x 1d0)) sequence))

(defparameter *empty-pool* (make-array 1 :element-type 'single-float :initial-element 0f0)
  "The pool of a song with no recorded sound.")

(defun apply-script-event (s p placed event)
  "EVENT, (frame kind track row), done to S as the worklet's message of
that kind would do it: :launch row ROW on track TRACK (-1 stops it), or
:arrange track TRACK back to its arrangement (-1 every track)."
  (destructuring-bind (frame kind track &optional row) event
    (declare (ignore frame))
    (let ((tracks (tracks-of-kind placed :midi)))
      (ecase kind
        (:launch (session-queue s p (getf (nth track tracks) :state) (float row 1d0)))
        (:arrange (loop for placed-track in tracks
                        for i from 0
                        when (or (< track 0) (= track i))
                          do (session-arrange s (getf placed-track :state))))))))

(defun render-plan (plan params score frames &key (sr 48000) (from 0d0) (block-size frames)
                                                  (pool *empty-pool*) script)
  "FRAMES frames of PLAN, from beat FROM, as two double buffers: left
and right, what the worklet plays.  BLOCK-SIZE only proves that the
frames do not depend on how they are asked for."
  (let* ((engine (lisp-engine plan sr))
         (s (double-buffer (engine-state-size engine)))
         (p (double-vector params))
         (sc (double-vector score))
         (left (double-buffer frames))
         (right (double-buffer frames))
         (run (engine-frames engine)))
    (with-audio-floats
      (engine-setup s (float sr 1d0))
      (funcall (engine-init engine) s)
      (transport-start s (float from 1d0))
      (if (and (>= block-size frames) (null script))
          (funcall run s p sc pool left right frames)
          ;; in pieces: at most BLOCK-SIZE, and cut where SCRIPT acts,
          ;; as the worklet cuts its quanta
          (let ((events (sort (copy-list script) #'< :key #'first))
                (done 0))
            (loop while (< done frames)
                  do (loop while (and events (<= (first (first events)) done))
                           do (apply-script-event s p (engine-placed engine) (pop events)))
                     (let* ((stop (min frames (+ done block-size)
                                       (if events (max (1+ done) (first (first events))) frames)))
                            (n (- stop done))
                            (l (double-buffer n))
                            (r (double-buffer n)))
                       (funcall run s p sc pool l r n)
                       (replace left l :start1 done)
                       (replace right r :start1 done)
                       (setf done stop))))))
    (values left right)))
