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
;; The devices: two instruments and the effects.  A device is three
;; things: its parameters, each with its range and what it does, in
;; words; a TICK, which turns parameters into coefficients every 64
;; frames; and a RUN, which works on a bus, frame by frame.  Every word
;; of a parameter's :doc reaches the page's tooltips and the prompt's
;; model, so a control is explained once and there are no presets to
;; keep: a sound is a set of values, and the model works them out from
;; what the controls do.
;;

(defstruct (param (:constructor make-param (key label &key (default 0) min max unit scale choices doc)))
  "One control.  KEY names it in forms; CHOICES, when given, are the
keywords it takes, and the engine sees the position in that list."
  key label default min max unit scale choices doc)

(defvar *param-tables* (make-hash-table)
  "Device kind -> the list of its params, in engine order.")

(defun device-params (kind)
  (gethash kind *param-tables*))

(defmacro define-params (prefix kind &rest params)
  "The controls of device KIND, each (key label . keys of make-param),
and for each one the slot PREFIX-KEY, its offset among the device's
parameters."
  `(progn
     (define-slots ,prefix ,@(mapcar (lambda (p) (intern (symbol-name (first p)))) params))
     (setf (gethash ,kind *param-tables*)
           (list ,@(mapcar (lambda (p) `(make-param ,@(mapcar (lambda (x) (if (and (consp x) (keywordp (car x))) `',x x)) p)))
                           params)))))

;;; The synth.

(define-params sp :synth
  (:osc1-wave "Osc 1 wave" :default :saw :choices (:saw :square :triangle :sine)
   :doc "Shape of oscillator 1. Saw is bright and full of harmonics; square is hollow; triangle is soft; sine has no harmonics.")
  (:osc1-octave "Osc 1 octave" :default 0 :min -3 :max 3 :doc "Octaves above or below the played note.")
  (:osc1-level "Osc 1 level" :default 0.8 :min 0 :max 1 :doc "How much of oscillator 1 reaches the filter.")
  (:osc2-wave "Osc 2 wave" :default :saw :choices (:saw :square :triangle :sine)
   :doc "Shape of oscillator 2.")
  (:osc2-octave "Osc 2 octave" :default 0 :min -3 :max 3 :doc "Octaves above or below the played note.")
  (:osc2-semitones "Osc 2 semitones" :default 0 :min -12 :max 12 :doc "Semitones from the played note: 7 is a fifth above.")
  (:osc2-detune "Osc 2 detune" :default 0 :min -100 :max 100 :unit "cents"
   :doc "A small offset against oscillator 1. Around 5 to 15 cents beats slowly and sounds wide and thick.")
  (:osc2-level "Osc 2 level" :default 0 :min 0 :max 1 :doc "How much of oscillator 2 reaches the filter.")
  (:fm "FM amount" :default 0 :min 0 :max 1
   :doc "Oscillator 2 bends the phase of oscillator 1. Metallic, bell-like, growling tones; works at low levels. Oscillator 2 need not be heard for this.")
  (:sub-level "Sub level" :default 0 :min 0 :max 1 :doc "A sine one octave below the note: weight under a bass.")
  (:noise-level "Noise level" :default 0 :min 0 :max 1 :doc "White noise into the filter: breath, air, risers.")
  (:drive "Drive" :default 0 :min 0 :max 24 :unit "dB" :doc "Saturation before the filter: adds harmonics and grit.")
  (:filter-type "Filter type" :default :low-pass :choices (:low-pass :band-pass :high-pass)
   :doc "Low pass removes highs, band pass keeps a band, high pass removes lows.")
  (:slope "Filter slope" :default :db24 :choices (:db12 :db24)
   :doc "How steeply the filter cuts. 24 dB is darker and more squelchy, 12 dB gentler.")
  (:cutoff "Cutoff" :default 2000 :min 20 :max 20000 :unit "Hz" :scale :log
   :doc "Where the filter starts cutting. Lower is darker. The control most often automated.")
  (:resonance "Resonance" :default 0.2 :min 0 :max 1
   :doc "Emphasis at the cutoff. High values whistle and give the squelchy acid sound.")
  (:filter-env "Filter envelope" :default 0 :min -96 :max 96 :unit "semitones"
   :doc "How far the filter envelope moves the cutoff. Positive with a short decay gives a plucky bite.")
  (:key-track "Key tracking" :default 0 :min 0 :max 1 :doc "How far the cutoff follows the note played; 1 follows fully.")
  (:filter-attack "Filter attack" :default 0.001 :min 0.0005 :max 10 :unit "s" :scale :log :doc "Filter envelope rise time.")
  (:filter-decay "Filter decay" :default 0.2 :min 0.001 :max 10 :unit "s" :scale :log :doc "Filter envelope fall time to its sustain.")
  (:filter-sustain "Filter sustain" :default 0 :min 0 :max 1 :doc "Filter envelope level while the note is held.")
  (:filter-release "Filter release" :default 0.2 :min 0.001 :max 10 :unit "s" :scale :log :doc "Filter envelope fall time after the note ends.")
  (:attack "Attack" :default 0.002 :min 0.0005 :max 10 :unit "s" :scale :log :doc "How fast the note rises. Long values swell like pads.")
  (:decay "Decay" :default 0.3 :min 0.001 :max 10 :unit "s" :scale :log :doc "How fast the note falls to its sustain level.")
  (:sustain "Sustain" :default 0.8 :min 0 :max 1 :doc "Level while the note is held. Zero gives plucks.")
  (:release "Release" :default 0.1 :min 0.001 :max 10 :unit "s" :scale :log :doc "How long the note rings after it ends.")
  (:lfo-wave "LFO wave" :default :sine :choices (:sine :triangle :saw :square :random) :doc "Shape of the slow modulator.")
  (:lfo-rate "LFO rate" :default 2 :min 0.01 :max 40 :unit "Hz" :scale :log :doc "LFO speed, used when LFO sync is 0.")
  (:lfo-sync "LFO sync" :default 0 :min 0 :max 32 :unit "beats"
   :doc "LFO period in beats, locked to the tempo: 0.25 is a sixteenth, 4 a bar. Zero uses LFO rate instead.")
  (:lfo-pitch "LFO to pitch" :default 0 :min 0 :max 24 :unit "semitones" :doc "Vibrato depth.")
  (:lfo-cutoff "LFO to cutoff" :default 0 :min 0 :max 8 :unit "octaves" :doc "Filter wobble depth.")
  (:lfo-level "LFO to level" :default 0 :min 0 :max 1 :doc "Tremolo depth.")
  (:voices "Voices" :default 8 :min 1 :max 8
   :doc "Notes that can sound at once. 1 makes the synth monophonic, and overlapping notes then slide.")
  (:glide "Glide" :default 0 :min 0 :max 1 :unit "s" :doc "With one voice, the time to slide from note to note.")
  (:velocity "Velocity" :default 0.5 :min 0 :max 1 :doc "How far note velocity changes the loudness.")
  (:level "Level" :default -6 :min -60 :max 12 :unit "dB" :doc "The synth's output level."))

(define-slots sy att dec sus rel fatt fdec fsus frel lfodt glide age acc dgain noise level)
(define-slots v active pitch target vel gate age ph1 ph2 ph3 lfo lfoheld amp-stage amp fenv-stage fenv
  ic1 ic2 ic3 ic4 dt1 dt2 dt3 lfov c0 c1 c2 c3)

(define-dsp synth-tick ((s :buf) (p :buf) (b :int) (pb :int)) :nil
  (let* ((sr (sg g-sr))
         (sync (pv sp-lfo-sync)))
    (setf (st sy-att) (/ 1d0 (* (pv sp-attack) sr)))
    (setf (st sy-dec) (decay-coef (pv sp-decay) sr))
    (setf (st sy-sus) (pv sp-sustain))
    (setf (st sy-rel) (decay-coef (pv sp-release) sr))
    (setf (st sy-fatt) (/ 1d0 (* (pv sp-filter-attack) sr)))
    (setf (st sy-fdec) (decay-coef (pv sp-filter-decay) sr))
    (setf (st sy-fsus) (pv sp-filter-sustain))
    (setf (st sy-frel) (decay-coef (pv sp-filter-release) sr))
    (setf (st sy-lfodt) (if (> sync 0d0)
                            (/ (sg g-bpf) sync)
                            (* (pv sp-lfo-rate) (sg g-isr))))
    ;; the glide moves once per 16 frames
    (setf (st sy-glide) (if (> (pv sp-glide) 0.0005d0)
                            (- 1d0 (d-exp2 (/ -96d0 (* (pv sp-glide) sr))))
                            1d0))
    (setf (st sy-dgain) (db-gain (pv sp-drive)))
    (setf (st sy-level) (db-gain (pv sp-level)))
    (when (< (st sy-noise) 1d0)
      (setf (st sy-noise) (+ 12345d0 (dfloat b))))))

(define-dsp synth-note-on ((s :buf) (p :buf) (b :int) (pb :int) (pitch :num) (vel :num) (gate :num)) :nil
  (let* ((n (max 1 (min 8 (ifloor (pv sp-voices)))))
         (pick -1)
         (steal 0)
         (oldest 1d300))
    (dotimes (v n)
      (let* ((o (+ b sy-size (* v v-size))))
        (when (and (< pick 0) (< (aref s (+ o v-active)) 0.5d0))
          (setf pick v))
        (when (< (aref s (+ o v-age)) oldest)
          (setf oldest (aref s (+ o v-age)))
          (setf steal v))))
    (let* ((o (+ b sy-size (* (if (< pick 0) steal pick) v-size)))
           ;; one voice, still sounding: a slide, not a new note
           (legato (and (= n 1)
                        (> (aref s (+ o v-active)) 0.5d0)
                        (< (aref s (+ o v-amp-stage)) 2.5d0))))
      (setf (st sy-age) (+ (st sy-age) 1d0))
      (setf (aref s (+ o v-age)) (st sy-age))
      (setf (aref s (+ o v-target)) pitch)
      (setf (aref s (+ o v-vel)) vel)
      (setf (aref s (+ o v-gate)) gate)
      (unless legato
        (unless (and (= n 1) (> (aref s (+ o v-active)) 0.5d0))
          (setf (aref s (+ o v-pitch)) pitch)
          (setf (aref s (+ o v-ph1)) 0d0)
          (setf (aref s (+ o v-ph2)) 0d0)
          (setf (aref s (+ o v-ph3)) 0d0)
          (setf (aref s (+ o v-lfo)) 0d0)
          (setf (aref s (+ o v-amp)) 0d0)
          (setf (aref s (+ o v-fenv)) 0d0)
          ;; a zero step makes the voice work out its pitch at once
          (setf (aref s (+ o v-dt1)) 0d0))
        (setf (aref s (+ o v-amp-stage)) 1d0)
        (setf (aref s (+ o v-fenv-stage)) 1d0))
      (setf (aref s (+ o v-active)) 1d0))))

(define-dsp synth-note-off ((s :buf) (b :int) (pitch :num)) :nil
  "Release the voices on PITCH, or all of them when PITCH is negative."
  (dotimes (v 8)
    (let* ((o (+ b sy-size (* v v-size))))
      (when (and (> (aref s (+ o v-active)) 0.5d0)
                 (or (< pitch 0d0) (= (aref s (+ o v-target)) pitch)))
        (setf (aref s (+ o v-gate)) 0d0)
        (setf (aref s (+ o v-amp-stage)) 3d0)
        (setf (aref s (+ o v-fenv-stage)) 3d0)))))

(define-dsp synth-slow ((s :buf) (p :buf) (b :int) (pb :int) (o :int)) :nil
  "What moves once per 16 frames: pitch, the oscillators' steps, the filter."
  (let* ((isr (sg g-isr))
         (pitch (+ (aref s (+ o v-pitch))
                   (* (st sy-glide) (- (aref s (+ o v-target)) (aref s (+ o v-pitch))))))
         (lfo (aref s (+ o v-lfov)))
         (played (+ pitch (* lfo (pv sp-lfo-pitch))))
         (hz (pitch-hz played))
         (cut (* (pv sp-cutoff)
                 (d-exp2 (+ (/ (* (aref s (+ o v-fenv)) (pv sp-filter-env)) 12d0)
                            (* lfo (pv sp-lfo-cutoff))
                            (/ (* (pv sp-key-track) (- pitch 60d0)) 12d0))))))
    (setf (aref s (+ o v-pitch)) pitch)
    (setf (aref s (+ o v-dt1)) (min 0.45d0 (* hz (d-exp2 (pv sp-osc1-octave)) isr)))
    (setf (aref s (+ o v-dt2))
          (min 0.45d0 (* hz isr (d-exp2 (+ (pv sp-osc2-octave)
                                           (/ (pv sp-osc2-semitones) 12d0)
                                           (/ (pv sp-osc2-detune) 1200d0))))))
    (setf (aref s (+ o v-dt3)) (* hz 0.5d0 isr))
    (svf-coefs s (+ o v-c0) cut (pv sp-resonance))))

(define-dsp synth-voice ((s :buf) (p :buf) (b :int) (pb :int) (o :int)) :nil
  (let* ((gate (aref s (+ o v-gate))))
    ;; a negative gate is a key held down, which the key's release ends
    (when (> gate 0d0)
      (let* ((left (- gate (sg g-bpf))))
        (setf (aref s (+ o v-gate)) left)
        (when (<= left 0d0)
          (setf (aref s (+ o v-gate)) 0d0)
          (setf (aref s (+ o v-amp-stage)) 3d0)
          (setf (aref s (+ o v-fenv-stage)) 3d0))))
    (env-step s (+ o v-amp-stage) (st sy-att) (st sy-dec) (st sy-sus) (st sy-rel))
    (env-step s (+ o v-fenv-stage) (st sy-fatt) (st sy-fdec) (st sy-fsus) (st sy-frel))
    (let* ((ph (+ (aref s (+ o v-lfo)) (st sy-lfodt))))
      (when (>= ph 1d0)
        (setf (aref s (+ o v-lfoheld)) (next-noise s (+ b sy-noise))))
      (setf (aref s (+ o v-lfo)) (wrap-phase ph))
      (setf (aref s (+ o v-lfov))
            (lfo-wave (aref s (+ o v-lfo)) (pv sp-lfo-wave) (aref s (+ o v-lfoheld)))))
    (when (or (< (sg g-sub) 0.5d0) (<= (aref s (+ o v-dt1)) 0d0))
      (synth-slow s p b pb o))
    (let* ((dt1 (max 0.00001d0 (aref s (+ o v-dt1))))
           (dt2 (max 0.00001d0 (aref s (+ o v-dt2))))
           (dt3 (aref s (+ o v-dt3)))
           (ph1 (aref s (+ o v-ph1)))
           (ph2 (aref s (+ o v-ph2)))
           (ph3 (aref s (+ o v-ph3)))
           (o2 (osc-wave ph2 dt2 (pv sp-osc2-wave)))
           (fm (pv sp-fm))
           (o1 (osc-wave (wrap-phase (+ ph1 (* fm o2))) dt1 (pv sp-osc1-wave)))
           (x (+ (* o1 (pv sp-osc1-level))
                 (* o2 (pv sp-osc2-level))
                 (* (d-sin ph3) (pv sp-sub-level))
                 (* (next-noise s (+ b sy-noise)) (pv sp-noise-level))))
           (driven (if (> (pv sp-drive) 0.01d0) (d-soft (* x (st sy-dgain))) x))
           (mode (pv sp-filter-type))
           (y1 (svf-step s (+ o v-ic1) (+ o v-c0) driven mode))
           (y (if (> (pv sp-slope) 0.5d0) (svf-step s (+ o v-ic3) (+ o v-c0) y1 mode) y1))
           (sens (pv sp-velocity))
           (loud (+ (- 1d0 sens) (* sens (/ (aref s (+ o v-vel)) 127d0))))
           (trem (- 1d0 (* (pv sp-lfo-level) 0.5d0 (+ 1d0 (aref s (+ o v-lfov))))))
           (amp (aref s (+ o v-amp))))
      (setf (aref s (+ o v-ph1)) (wrap-phase (+ ph1 dt1)))
      (setf (aref s (+ o v-ph2)) (wrap-phase (+ ph2 dt2)))
      (setf (aref s (+ o v-ph3)) (wrap-phase (+ ph3 dt3)))
      (setf (st sy-acc) (+ (st sy-acc) (* y amp loud trem)))
      (when (and (> (aref s (+ o v-amp-stage)) 2.5d0) (< amp 0.00002d0))
        (setf (aref s (+ o v-active)) 0d0)))))

(define-dsp synth-run ((s :buf) (p :buf) (b :int) (pb :int) (bus :int)) :nil
  (setf (st sy-acc) 0d0)
  (dotimes (v 8)
    (let* ((o (+ b sy-size (* v v-size))))
      (when (> (aref s (+ o v-active)) 0.5d0)
        (synth-voice s p b pb o))))
  (let* ((y (* (st sy-acc) (st sy-level))))
    (setf (aref s bus) (+ (aref s bus) y))
    (setf (aref s (+ bus 1)) (+ (aref s (+ bus 1)) y))))

;;; The drum kit: eight pads, each a small synthesizer of its own (a
;;; sine that falls in pitch, filtered noise and a click), so a kick, a
;;; hat or a clap is a set of values.

(define-slots dp tune sweep sweep-time decay tone noise noise-decay noise-cutoff click drive level pan)

(defparameter *pad-defaults*
  ;;       tune sweep swt   decay tone noise ndec  ncut  click drive level pan
  '(("Kick"   50  36 0.03  0.30  1.0  0.0  0.02  8000  0.2  0.0  -3  0)
    ("Snare" 185  12 0.02  0.12  0.6  0.7  0.16  2000  0.2  0.0  -6  0)
    ("Clap"  900   0 0.01  0.02  0.0  1.0  0.20  1200  0.4  0.0  -8  0)
    ("Closed hat" 3000 0 0.01 0.02 0.0 0.8 0.05 8000 0.1 0.0 -12  0.1)
    ("Open hat"   3000 0 0.01 0.02 0.0 0.8 0.35 7000 0.1 0.0 -14 -0.1)
    ("Tom"   110  12 0.05  0.35  1.0  0.1  0.05  3000  0.2  0.0  -6  0.2)
    ("Rim"   400   5 0.01  0.04  0.7  0.3  0.03  3000  0.5  0.0 -10 -0.2)
    ("Shaker" 5000 0 0.01 0.01 0.0 0.6 0.08 6000 0.0 0.0 -16 0.3))
  "A kit's starting values: sounds to start from, as a new synth starts
as a plain saw.  Anything else is set through the pads' parameters.")

(setf (gethash :drums *param-tables*)
      (loop for (name . values) in *pad-defaults*
            for pad from 1
            append (flet ((key (word) (intern (format nil "PAD~d-~a" pad word) :keyword))
                          (label (word) (format nil "~a ~a" name word)))
                     (destructuring-bind (tune sweep sweep-time decay tone noise noise-decay noise-cutoff
                                          click drive level pan)
                         values
                       (list
                        (make-param (key "TUNE") (label "tune") :default tune :min 20 :max 8000 :unit "Hz" :scale :log
                                    :doc "Pitch the body settles on. A kick's body is 40 to 60 Hz.")
                        (make-param (key "SWEEP") (label "sweep") :default sweep :min 0 :max 72 :unit "semitones"
                                    :doc "How far above the tune the body starts. This fall is what makes a kick thump.")
                        (make-param (key "SWEEP-TIME") (label "sweep time") :default sweep-time :min 0.001 :max 0.5 :unit "s" :scale :log
                                    :doc "How fast the pitch falls. Shorter is clickier and tighter.")
                        (make-param (key "DECAY") (label "decay") :default decay :min 0.005 :max 4 :unit "s" :scale :log
                                    :doc "How long the body rings. A short tight kick leaves room for the bass.")
                        (make-param (key "TONE") (label "tone") :default tone :min 0 :max 1 :doc "Level of the body.")
                        (make-param (key "NOISE") (label "noise") :default noise :min 0 :max 1 :doc "Level of the noise: the sizzle of a hat, the snap of a snare.")
                        (make-param (key "NOISE-DECAY") (label "noise decay") :default noise-decay :min 0.005 :max 4 :unit "s" :scale :log :doc "How long the noise rings.")
                        (make-param (key "NOISE-CUTOFF") (label "noise cutoff") :default noise-cutoff :min 100 :max 16000 :unit "Hz" :scale :log :doc "Noise below this is taken out. Higher is thinner.")
                        (make-param (key "CLICK") (label "click") :default click :min 0 :max 1 :doc "A very short attack transient, which helps a drum cut through.")
                        (make-param (key "DRIVE") (label "drive") :default drive :min 0 :max 24 :unit "dB" :doc "Saturation: a harder, louder hit.")
                        (make-param (key "LEVEL") (label "level") :default level :min -60 :max 12 :unit "dB" :doc "The pad's level.")
                        (make-param (key "PAN") (label "pan") :default pan :min -1 :max 1 :doc "Left -1 to right 1."))))))

(define-slots dk noise)
(define-slots dv active ph penv pcoef amp acoef namp ncoef click lp vel)

(define-dsp drum-note-on ((s :buf) (p :buf) (b :int) (pb :int) (pitch :num) (vel :num) (gate :num)) :nil
  (let* ((pad (- (ifloor pitch) 36)))
    (when (and (>= pad 0) (< pad 8))
      (let* ((o (+ b dk-size (* pad dv-size)))
             (q (+ pb (* pad dp-size)))
             (sr (sg g-sr)))
        (setf (aref s (+ o dv-active)) 1d0)
        (setf (aref s (+ o dv-ph)) 0d0)
        (setf (aref s (+ o dv-penv)) 1d0)
        (setf (aref s (+ o dv-pcoef)) (decay-coef (aref p (+ q dp-sweep-time)) sr))
        (setf (aref s (+ o dv-amp)) 1d0)
        (setf (aref s (+ o dv-acoef)) (decay-coef (aref p (+ q dp-decay)) sr))
        (setf (aref s (+ o dv-namp)) 1d0)
        (setf (aref s (+ o dv-ncoef)) (decay-coef (aref p (+ q dp-noise-decay)) sr))
        (setf (aref s (+ o dv-click)) 1d0)
        (setf (aref s (+ o dv-vel)) (/ vel 127d0))
        (when (< (st dk-noise) 1d0)
          (setf (st dk-noise) (+ 54321d0 (dfloat b))))))))

(define-dsp drum-run ((s :buf) (p :buf) (b :int) (pb :int) (bus :int)) :nil
  (let* ((isr (sg g-isr)))
    (dotimes (pad 8)
      (let* ((o (+ b dk-size (* pad dv-size)))
             (q (+ pb (* pad dp-size))))
        (when (> (aref s (+ o dv-active)) 0.5d0)
          (let* ((penv (aref s (+ o dv-penv)))
                 (hz (* (aref p (+ q dp-tune))
                        (d-exp2 (/ (* (aref p (+ q dp-sweep)) penv) 12d0))))
                 (ph (wrap-phase (+ (aref s (+ o dv-ph)) (min 0.45d0 (* hz isr)))))
                 (amp (aref s (+ o dv-amp)))
                 (namp (aref s (+ o dv-namp)))
                 (click (aref s (+ o dv-click)))
                 (white (next-noise s (+ b dk-noise)))
                 (c (min 1d0 (* 6.283185307179586d0 (aref p (+ q dp-noise-cutoff)) isr)))
                 (lp (+ (aref s (+ o dv-lp)) (* c (- white (aref s (+ o dv-lp))))))
                 (x (+ (* (d-sin ph) amp (aref p (+ q dp-tone)))
                       (* (- white lp) namp (aref p (+ q dp-noise)))
                       (* white click (aref p (+ q dp-click)))))
                 (drive (aref p (+ q dp-drive)))
                 (y (* (if (> drive 0.01d0) (d-soft (* x (db-gain drive))) x)
                       (aref s (+ o dv-vel))
                       (db-gain (aref p (+ q dp-level)))))
                 (pan (* (+ (aref p (+ q dp-pan)) 1d0) 0.125d0)))
            (setf (aref s (+ o dv-ph)) ph)
            (setf (aref s (+ o dv-lp)) lp)
            (setf (aref s (+ o dv-penv)) (* penv (aref s (+ o dv-pcoef))))
            (setf (aref s (+ o dv-amp)) (* amp (aref s (+ o dv-acoef))))
            (setf (aref s (+ o dv-namp)) (* namp (aref s (+ o dv-ncoef))))
            (setf (aref s (+ o dv-click)) (* click 0.99d0))
            (setf (aref s bus) (+ (aref s bus) (* y 1.4142135623730951d0 (d-sin (+ pan 0.25d0)))))
            (setf (aref s (+ bus 1)) (+ (aref s (+ bus 1)) (* y 1.4142135623730951d0 (d-sin pan))))
            (when (and (< amp 0.00005d0) (< namp 0.00005d0))
              (setf (aref s (+ o dv-active)) 0d0))))))))

;;; The audio player: a track of recorded sound.  A clip is a row of the
;;; score's region table, six numbers (where the left and right channels
;;; are in the pool, the first and last frame, the rate, the gain), and
;;; its event's pitch is that row's number; so moving, trimming or
;;; retuning a clip is data, and the program is the same.

(define-params sm :sampler
  (:fade "Fade" :default 3 :min 0.1 :max 200 :unit "ms" :scale :log
   :doc "How fast a clip fades in and out at its edges. A few milliseconds stops clicks; more softens a cut through a word.")
  (:level "Level" :default 0 :min -60 :max 12 :unit "dB" :doc "The level of every clip on this track."))

(define-slots sa fade level age)
(define-slots sv active pos end left right rate gain gate stage env age)

(define-dsp sampler-tick ((s :buf) (p :buf) (b :int) (pb :int)) :nil
  (setf (st sa-fade) (/ 1000d0 (* (pv sm-fade) (sg g-sr))))
  (setf (st sa-level) (db-gain (pv sm-level))))

(define-dsp sampler-release ((s :buf) (b :int)) :nil
  (dotimes (v 4)
    (let* ((o (+ b sa-size (* v sv-size))))
      (when (> (aref s (+ o sv-active)) 0.5d0)
        (setf (aref s (+ o sv-stage)) 2d0)))))

(define-dsp sampler-start ((s :buf) (sc :buf) (b :int) (region :num) (vel :num) (gate :num) (skip :num)) :nil
  "Region number REGION on a free voice for GATE beats, SKIP beats in."
  (let* ((row (+ (ifloor (aref sc 2)) (* 6 (ifloor region))))
         (rate (aref sc (+ row 4)))
         (pos (+ (aref sc (+ row 2)) (/ (* skip rate) (sg g-bpf))))
         (end (aref sc (+ row 3)))
         (pick -1)
         (steal 0)
         (oldest 1d300))
    (when (< pos (- end 1d0))
      (dotimes (v 4)
        (let* ((o (+ b sa-size (* v sv-size))))
          (when (and (< pick 0) (< (aref s (+ o sv-active)) 0.5d0))
            (setf pick v))
          (when (< (aref s (+ o sv-age)) oldest)
            (setf oldest (aref s (+ o sv-age)))
            (setf steal v))))
      (let* ((o (+ b sa-size (* (if (< pick 0) steal pick) sv-size))))
        (setf (st sa-age) (+ (st sa-age) 1d0))
        (setf (aref s (+ o sv-age)) (st sa-age))
        (setf (aref s (+ o sv-active)) 1d0)
        (setf (aref s (+ o sv-pos)) pos)
        (setf (aref s (+ o sv-end)) end)
        (setf (aref s (+ o sv-left)) (aref sc row))
        (setf (aref s (+ o sv-right)) (aref sc (+ row 1)))
        (setf (aref s (+ o sv-rate)) rate)
        (setf (aref s (+ o sv-gain)) (* (aref sc (+ row 5)) (/ vel 127d0)))
        (setf (aref s (+ o sv-gate)) gate)
        (setf (aref s (+ o sv-stage)) 1d0)
        (setf (aref s (+ o sv-env)) 0d0)))))

(define-dsp sampler-chase ((s :buf) (sc :buf) (b :int) (ti :int)) :nil
  "After a jump: what was playing stops, and a clip the new place falls
inside starts from there.  A long clip must not wait for its beginning."
  (sampler-release s b)
  (let* ((o (ifloor (aref sc (+ 4 (* 2 ti)))))
         (n (ifloor (aref sc (+ 5 (* 2 ti)))))
         (seek (sg g-seek)))
    (dotimes (i n)
      (let* ((e (+ o (* 4 i)))
             (start (aref sc e))
             (stop (+ start (aref sc (+ e 1)))))
        (when (and (< start seek) (> stop seek))
          (sampler-start s sc b (aref sc (+ e 2)) (aref sc (+ e 3)) (- stop seek) (- seek start)))))))

(define-dsp sampler-run ((s :buf) (pool :pool) (b :int) (bus :int)) :nil
  (let* ((fade (st sa-fade))
         (level (st sa-level)))
    (dotimes (v 4)
      (let* ((o (+ b sa-size (* v sv-size))))
        (when (> (aref s (+ o sv-active)) 0.5d0)
          (let* ((gate (- (aref s (+ o sv-gate)) (sg g-bpf)))
                 (pos (aref s (+ o sv-pos)))
                 (rate (aref s (+ o sv-rate)))
                 (end (aref s (+ o sv-end)))
                 (i (ifloor pos))
                 (f (- pos (dfloat i)))
                 (j (if (>= (dfloat (+ i 1)) end) i (+ i 1)))
                 (lo (ifloor (aref s (+ o sv-left))))
                 (ro (ifloor (aref s (+ o sv-right))))
                 (a (dfloat (aref pool (+ lo i))))
                 (c (dfloat (aref pool (+ ro i))))
                 (l (+ a (* f (- (dfloat (aref pool (+ lo j))) a))))
                 (r (+ c (* f (- (dfloat (aref pool (+ ro j))) c))))
                 (stage (aref s (+ o sv-stage)))
                 (env (aref s (+ o sv-env)))
                 (next (if (< stage 1.5d0) (min 1d0 (+ env fade)) (- env fade)))
                 (g (* next (aref s (+ o sv-gain)) level))
                 (ahead (+ pos rate (/ rate fade))))
            (setf (aref s (+ o sv-gate)) gate)
            (setf (aref s (+ o sv-env)) next)
            (setf (aref s (+ o sv-pos)) (+ pos rate))
            ;; fade out in time for the clip's end or the sample's
            (when (or (<= gate 0d0) (>= ahead end))
              (setf (aref s (+ o sv-stage)) 2d0))
            (when (or (<= next 0d0) (>= (+ pos rate) (- end 1d0)))
              (setf (aref s (+ o sv-active)) 0d0))
            (setf (aref s bus) (+ (aref s bus) (* l g)))
            (setf (aref s (+ bus 1)) (+ (aref s (+ bus 1)) (* r g)))))))))

;;; Effects.  Each takes the bus in place, two slots, left and right.

(define-dsp mix-into ((s :buf) (bus :int) (l :num) (r :num) (wet :num)) :nil
  "The bus become WET parts L and R and the rest what it was."
  (let* ((dry (- 1d0 wet)))
    (setf (aref s bus) (+ (* (aref s bus) dry) (* l wet)))
    (setf (aref s (+ bus 1)) (+ (* (aref s (+ bus 1)) dry) (* r wet)))))

(define-dsp beat-lfo-step ((s :buf) (i :int) (rate :num) (sync :num)) :inline
  "Advance the phase at S[I] by RATE Hz, or a period of SYNC beats when
SYNC is positive; the phase."
  (let* ((ph (wrap-phase (+ (aref s i)
                            (if (> sync 0d0) (/ (sg g-bpf) sync) (* rate (sg g-isr)))))))
    (setf (aref s i) ph)
    ph))

;; Auto filter

(define-params fp :filter
  (:type "Type" :default :low-pass :choices (:low-pass :band-pass :high-pass)
   :doc "Low pass removes highs, band pass keeps a band, high pass removes lows.")
  (:cutoff "Cutoff" :default 2000 :min 20 :max 20000 :unit "Hz" :scale :log
   :doc "Where the filter starts cutting. Automate this for sweeps and build-ups.")
  (:resonance "Resonance" :default 0.2 :min 0 :max 1 :doc "Emphasis at the cutoff.")
  (:drive "Drive" :default 0 :min 0 :max 24 :unit "dB" :doc "Saturation before the filter.")
  (:lfo-rate "LFO rate" :default 1 :min 0.01 :max 40 :unit "Hz" :scale :log :doc "Wobble speed when LFO sync is 0.")
  (:lfo-sync "LFO sync" :default 0 :min 0 :max 32 :unit "beats" :doc "Wobble period in beats, locked to the tempo; 0 uses LFO rate.")
  (:lfo-depth "LFO depth" :default 0 :min 0 :max 8 :unit "octaves" :doc "How far the wobble moves the cutoff.")
  (:mix "Mix" :default 1 :min 0 :max 1 :doc "How much of the filtered signal is heard."))

(define-slots fs c0 c1 c2 c3 l1 l2 l3 l4 r1 r2 r3 r4 lfo dgain)

(define-dsp fx-filter-tick ((s :buf) (p :buf) (b :int) (pb :int)) :nil
  (setf (st fs-dgain) (db-gain (pv fp-drive))))

(define-dsp fx-filter ((s :buf) (p :buf) (b :int) (pb :int) (bus :int)) :nil
  (let* ((ph (beat-lfo-step s (+ b fs-lfo) (pv fp-lfo-rate) (pv fp-lfo-sync)))
         (mode (pv fp-type))
         (g (st fs-dgain))
         (driven (> (pv fp-drive) 0.01d0))
         (xl (if driven (d-soft (* (aref s bus) g)) (aref s bus)))
         (xr (if driven (d-soft (* (aref s (+ bus 1)) g)) (aref s (+ bus 1)))))
    (when (< (sg g-sub) 0.5d0)
      (svf-coefs s (+ b fs-c0)
                 (* (pv fp-cutoff) (d-exp2 (* (d-sin ph) (pv fp-lfo-depth))))
                 (pv fp-resonance)))
    (let* ((l1 (svf-step s (+ b fs-l1) (+ b fs-c0) xl mode))
           (r1 (svf-step s (+ b fs-r1) (+ b fs-c0) xr mode))
           (l2 (svf-step s (+ b fs-l3) (+ b fs-c0) l1 mode))
           (r2 (svf-step s (+ b fs-r3) (+ b fs-c0) r1 mode)))
      (mix-into s bus l2 r2 (pv fp-mix)))))

;; Saturator

(define-params dr :drive
  (:amount "Amount" :default 6 :min 0 :max 36 :unit "dB" :doc "How hard the signal is pushed into the curve: more harmonics, more grit.")
  (:tone "Tone" :default 12000 :min 200 :max 20000 :unit "Hz" :scale :log :doc "Top of the saturated sound. Lower tames fizz.")
  (:output "Output" :default -3 :min -24 :max 12 :unit "dB" :doc "Level after the drive.")
  (:mix "Mix" :default 1 :min 0 :max 1 :doc "How much of the driven signal is heard."))

(define-slots ds gain out coef l r)

(define-dsp fx-drive-tick ((s :buf) (p :buf) (b :int) (pb :int)) :nil
  (setf (st ds-gain) (db-gain (pv dr-amount)))
  (setf (st ds-out) (db-gain (pv dr-output)))
  (setf (st ds-coef) (min 1d0 (* 6.283185307179586d0 (pv dr-tone) (sg g-isr)))))

(define-dsp fx-drive ((s :buf) (p :buf) (b :int) (pb :int) (bus :int)) :nil
  (let* ((c (st ds-coef))
         (l (+ (st ds-l) (* c (- (d-soft (* (aref s bus) (st ds-gain))) (st ds-l)))))
         (r (+ (st ds-r) (* c (- (d-soft (* (aref s (+ bus 1)) (st ds-gain))) (st ds-r))))))
    (setf (st ds-l) l)
    (setf (st ds-r) r)
    (mix-into s bus (* l (st ds-out)) (* r (st ds-out)) (pv dr-mix))))

;; Three-band EQ

(define-params eq :eq
  (:low "Low" :default 0 :min -48 :max 12 :unit "dB" :doc "Gain below Low frequency. Cut it to clear mud from everything but kick and bass.")
  (:mid "Mid" :default 0 :min -48 :max 12 :unit "dB" :doc "Gain between the two frequencies.")
  (:high "High" :default 0 :min -48 :max 12 :unit "dB" :doc "Gain above High frequency. Raise it for air.")
  (:low-freq "Low frequency" :default 250 :min 30 :max 1000 :unit "Hz" :scale :log :doc "Where the low band ends.")
  (:high-freq "High frequency" :default 2500 :min 800 :max 16000 :unit "Hz" :scale :log :doc "Where the high band starts."))

(define-slots es lc0 lc1 lc2 lc3 hc0 hc1 hc2 hc3 l1 l2 r1 r2 l3 l4 r3 r4 gl gm gh)

(define-dsp fx-eq-tick ((s :buf) (p :buf) (b :int) (pb :int)) :nil
  (svf-coefs s (+ b es-lc0) (pv eq-low-freq) 0.3d0)
  (svf-coefs s (+ b es-hc0) (pv eq-high-freq) 0.3d0)
  (setf (st es-gl) (db-gain (pv eq-low)))
  (setf (st es-gm) (db-gain (pv eq-mid)))
  (setf (st es-gh) (db-gain (pv eq-high))))

(define-dsp fx-eq ((s :buf) (p :buf) (b :int) (pb :int) (bus :int)) :nil
  (let* ((xl (aref s bus))
         (xr (aref s (+ bus 1)))
         (ll (svf-step s (+ b es-l1) (+ b es-lc0) xl 0d0))
         (lr (svf-step s (+ b es-r1) (+ b es-lc0) xr 0d0))
         (hl (svf-step s (+ b es-l3) (+ b es-hc0) xl 2d0))
         (hr (svf-step s (+ b es-r3) (+ b es-hc0) xr 2d0)))
    (setf (aref s bus) (+ (* ll (st es-gl)) (* (- (- xl ll) hl) (st es-gm)) (* hl (st es-gh))))
    (setf (aref s (+ bus 1)) (+ (* lr (st es-gl)) (* (- (- xr lr) hr) (st es-gm)) (* hr (st es-gh))))))

;; Delay: four seconds a side at most.

(define-params dl :delay
  (:time-left "Time left" :default 0.75 :min 0.0625 :max 8 :unit "beats" :doc "Left echo time in beats: 0.75 is a dotted eighth, the classic trance echo.")
  (:time-right "Time right" :default 0.75 :min 0.0625 :max 8 :unit "beats" :doc "Right echo time in beats.")
  (:feedback "Feedback" :default 0.45 :min 0 :max 0.97 :doc "How much of each echo comes back. High values ring on for bars.")
  (:ping-pong "Ping-pong" :default 1 :min 0 :max 1 :doc "1 bounces the echoes between left and right.")
  (:low-cut "Low cut" :default 200 :min 20 :max 2000 :unit "Hz" :scale :log :doc "Echoes lose their lows here, so they stay out of the bass.")
  (:high-cut "High cut" :default 6000 :min 500 :max 20000 :unit "Hz" :scale :log :doc "Echoes lose their highs here: darker repeats sit behind the dry sound.")
  (:mix "Mix" :default 0.3 :min 0 :max 1 :doc "How much echo is heard. On a return track use 1."))

(define-slots dls pos cur-l cur-r lp-l lp-r hp-l hp-r lc hc)

(define-dsp fx-delay-tick ((s :buf) (p :buf) (b :int) (pb :int)) :nil
  (setf (st dls-lc) (min 1d0 (* 6.283185307179586d0 (pv dl-low-cut) (sg g-isr))))
  (setf (st dls-hc) (min 1d0 (* 6.283185307179586d0 (pv dl-high-cut) (sg g-isr)))))

(define-dsp fx-delay ((s :buf) (p :buf) (b :int) (pb :int) (bus :int)) :nil
  (let* ((size (* 4 (ifloor (sg g-sr))))
         (o1 (+ b dls-size))
         (o2 (+ o1 size))
         (pos (ifloor (st dls-pos)))
         (most (- (dfloat size) 2d0))
         (want-l (min most (/ (pv dl-time-left) (sg g-bpf))))
         (want-r (min most (/ (pv dl-time-right) (sg g-bpf))))
         (cur-l (+ (st dls-cur-l) (* 0.0005d0 (- want-l (st dls-cur-l)))))
         (cur-r (+ (st dls-cur-r) (* 0.0005d0 (- want-r (st dls-cur-r)))))
         (tap-l (read-tap s o1 size pos (max 1d0 cur-l)))
         (tap-r (read-tap s o2 size pos (max 1d0 cur-r)))
         (lp-l (+ (st dls-lp-l) (* (st dls-hc) (- tap-l (st dls-lp-l)))))
         (lp-r (+ (st dls-lp-r) (* (st dls-hc) (- tap-r (st dls-lp-r)))))
         (hp-l (+ (st dls-hp-l) (* (st dls-lc) (- lp-l (st dls-hp-l)))))
         (hp-r (+ (st dls-hp-r) (* (st dls-lc) (- lp-r (st dls-hp-r)))))
         (fb-l (* (- lp-l hp-l) (pv dl-feedback)))
         (fb-r (* (- lp-r hp-r) (pv dl-feedback)))
         (pp (pv dl-ping-pong))
         (xl (aref s bus))
         (xr (aref s (+ bus 1)))
         ;; ping-pong: the input enters at the left, each echo crosses
         (in-l (+ (* pp (* 0.5d0 (+ xl xr))) (* (- 1d0 pp) xl)))
         (in-r (* (- 1d0 pp) xr)))
    ;; a tempo jump must not sweep the echo through every delay between
    (when (< (st dls-cur-l) 1d0)
      (setf cur-l want-l)
      (setf cur-r want-r))
    (setf (st dls-cur-l) cur-l)
    (setf (st dls-cur-r) cur-r)
    (setf (st dls-lp-l) lp-l)
    (setf (st dls-lp-r) lp-r)
    (setf (st dls-hp-l) hp-l)
    (setf (st dls-hp-r) hp-r)
    (setf (aref s (+ o1 pos)) (+ in-l (* pp fb-r) (* (- 1d0 pp) fb-l)))
    (setf (aref s (+ o2 pos)) (+ in-r (* pp fb-l) (* (- 1d0 pp) fb-r)))
    (setf (st dls-pos) (dfloat (if (>= (+ pos 1) size) 0 (+ pos 1))))
    (mix-into s bus tap-l tap-r (pv dl-mix))))

;; Chorus and flanger: one short modulated delay.

(define-params ch :chorus
  (:rate "Rate" :default 0.4 :min 0.01 :max 10 :unit "Hz" :scale :log :doc "Modulation speed.")
  (:sync "Sync" :default 0 :min 0 :max 64 :unit "beats" :doc "Modulation period in beats; 0 uses Rate. Flangers often sweep over 8 or 16 beats.")
  (:delay "Delay" :default 12 :min 0.2 :max 30 :unit "ms" :doc "Base delay. 10 to 25 ms is chorus (wide); under 5 ms is flanger (jet sweep).")
  (:depth "Depth" :default 3 :min 0 :max 15 :unit "ms" :doc "How far the delay moves.")
  (:feedback "Feedback" :default 0 :min -0.95 :max 0.95 :doc "Signal fed back: strong values give the metallic flanger sound.")
  (:spread "Stereo spread" :default 0.25 :min 0 :max 0.5 :doc "Offset between left and right modulation: stereo width.")
  (:mix "Mix" :default 0.5 :min 0 :max 1 :doc "How much of the delayed signal is heard."))

(define-slots cs pos lfo)

(define-dsp fx-chorus-tick ((s :buf) (p :buf) (b :int) (pb :int)) :nil
  nil)

(define-dsp fx-chorus ((s :buf) (p :buf) (b :int) (pb :int) (bus :int)) :nil
  (let* ((sr (sg g-sr))
         (size (ifloor (* sr 0.0625d0)))
         (o1 (+ b cs-size))
         (o2 (+ o1 size))
         (pos (ifloor (st cs-pos)))
         (ph (beat-lfo-step s (+ b cs-lfo) (pv ch-rate) (pv ch-sync)))
         (ms (* sr 0.001d0))
         (dl-l (* ms (+ (pv ch-delay) (* (pv ch-depth) 0.5d0 (+ 1d0 (d-sin ph))))))
         (dl-r (* ms (+ (pv ch-delay) (* (pv ch-depth) 0.5d0 (+ 1d0 (d-sin (+ ph (pv ch-spread))))))))
         (l (read-tap s o1 size pos (max 1d0 dl-l)))
         (r (read-tap s o2 size pos (max 1d0 dl-r)))
         (fb (pv ch-feedback)))
    (setf (aref s (+ o1 pos)) (+ (aref s bus) (* l fb)))
    (setf (aref s (+ o2 pos)) (+ (aref s (+ bus 1)) (* r fb)))
    (setf (st cs-pos) (dfloat (if (>= (+ pos 1) size) 0 (+ pos 1))))
    (mix-into s bus l r (pv ch-mix))))

;; Phaser: six all-pass stages a side.

(define-params ph :phaser
  (:rate "Rate" :default 0.3 :min 0.01 :max 10 :unit "Hz" :scale :log :doc "Sweep speed.")
  (:sync "Sync" :default 0 :min 0 :max 64 :unit "beats" :doc "Sweep period in beats; 0 uses Rate.")
  (:center "Center" :default 800 :min 50 :max 8000 :unit "Hz" :scale :log :doc "Middle of the sweep.")
  (:depth "Depth" :default 2 :min 0 :max 5 :unit "octaves" :doc "How far the sweep travels each way.")
  (:feedback "Feedback" :default 0.5 :min 0 :max 0.95 :doc "Strength of the notches: more is more vocal and swirling.")
  (:mix "Mix" :default 0.5 :min 0 :max 1 :doc "How much of the swept signal is heard."))

(define-slots ps lfo coef fb-l fb-r)

(define-dsp fx-phaser-tick ((s :buf) (p :buf) (b :int) (pb :int)) :nil
  nil)

(define-dsp fx-phaser ((s :buf) (p :buf) (b :int) (pb :int) (bus :int)) :nil
  (let* ((ph (beat-lfo-step s (+ b ps-lfo) (pv ph-rate) (pv ph-sync))))
    (when (< (sg g-sub) 0.5d0)
      (let* ((hz (* (pv ph-center) (d-exp2 (* (d-sin ph) (pv ph-depth)))))
             (g (d-tan (max 0.0002d0 (min 0.49d0 (* hz (sg g-isr)))))))
        (setf (st ps-coef) (/ (- g 1d0) (+ g 1d0)))))
    (let* ((a (st ps-coef))
           (o (+ b ps-size))
           (l (+ (aref s bus) (* (st ps-fb-l) (pv ph-feedback))))
           (r (+ (aref s (+ bus 1)) (* (st ps-fb-r) (pv ph-feedback)))))
      (dotimes (i 6)
        (let* ((zl (aref s (+ o i)))
               (zr (aref s (+ o 6 i)))
               (yl (+ (* a l) zl))
               (yr (+ (* a r) zr)))
          (setf (aref s (+ o i)) (- l (* a yl)))
          (setf (aref s (+ o 6 i)) (- r (* a yr)))
          (setf l yl)
          (setf r yr)))
      (setf (st ps-fb-l) l)
      (setf (st ps-fb-r) r)
      (mix-into s bus (* 0.5d0 (+ l (aref s bus))) (* 0.5d0 (+ r (aref s (+ bus 1)))) (pv ph-mix)))))

;; Reverb: eight delay lines mixed through a Hadamard matrix.

(define-params rv :reverb
  (:decay "Decay" :default 2.5 :min 0.1 :max 20 :unit "s" :scale :log :doc "Time for the tail to fall 60 dB. Long tails turn sounds into space and pads.")
  (:room-size "Room size" :default 0.8 :min 0.2 :max 1 :doc "Size of the room: larger spaces the echoes further apart.")
  (:damping "Damping" :default 0.4 :min 0 :max 1 :doc "How fast highs die in the tail. More is darker and softer.")
  (:predelay "Predelay" :default 20 :min 0 :max 200 :unit "ms" :doc "Gap before the tail starts, which keeps the dry sound clear.")
  (:mix "Mix" :default 0.25 :min 0 :max 1 :doc "How much reverb is heard. On a return track use 1."))

(define-slots rs pre damp p0 p1 p2 p3 p4 p5 p6 p7 g0 g1 g2 g3 g4 g5 g6 g7
  n0 n1 n2 n3 n4 n5 n6 n7 t0 t1 t2 t3 t4 t5 t6 t7)

(define-dsp reverb-line ((s :buf) (p :buf) (b :int) (pb :int) (i :int) (frames :num)) :nil
  "Line I's length and feedback.  FRAMES is its length at 48 kHz and
full size: the lengths are primes, so the echoes do not line up."
  (let* ((sr (sg g-sr))
         (n (dfloor (* frames (pv rv-room-size) (/ sr 48000d0)))))
    (setf (aref s (+ b rs-n0 i)) n)
    (setf (aref s (+ b rs-g0 i))
          (d-exp2 (/ (* -9.965784284662087d0 n) (* (pv rv-decay) sr))))))

(define-dsp fx-reverb-tick ((s :buf) (p :buf) (b :int) (pb :int)) :nil
  (reverb-line s p b pb 0 2063d0)
  (reverb-line s p b pb 1 2657d0)
  (reverb-line s p b pb 2 3049d0)
  (reverb-line s p b pb 3 3739d0)
  (reverb-line s p b pb 4 4261d0)
  (reverb-line s p b pb 5 4621d0)
  (reverb-line s p b pb 6 5261d0)
  (reverb-line s p b pb 7 6007d0)
  (setf (st rs-damp) (- 1d0 (* 0.9d0 (pv rv-damping)))))

(define-dsp butterfly ((s :buf) (i :int) (j :int)) :nil
  (let* ((a (aref s i))
         (c (aref s j)))
    (setf (aref s i) (+ a c))
    (setf (aref s j) (- a c))))

(define-dsp hadamard ((s :buf) (t0 :int)) :nil
  "The eight values at S[T0] through the matrix, which mixes each into
every other without changing their energy but for a factor of sqrt 8."
  (dotimes (i 4)
    (butterfly s (+ t0 (* i 2)) (+ t0 (* i 2) 1)))
  (dotimes (i 2)
    (butterfly s (+ t0 (* i 4)) (+ t0 (* i 4) 2))
    (butterfly s (+ t0 (* i 4) 1) (+ t0 (* i 4) 3)))
  (dotimes (i 4)
    (butterfly s (+ t0 i) (+ t0 i 4))))

(define-dsp fx-reverb ((s :buf) (p :buf) (b :int) (pb :int) (bus :int)) :nil
  (let* ((sr (sg g-sr))
         (line (ifloor (+ 1d0 (* sr 0.13d0))))
         (pre-size (ifloor (* sr 0.25d0)))
         (lines (+ b rs-size))
         (damp-state (+ lines (* 8 line)))
         (pre-l (+ damp-state 8))
         (pre-r (+ pre-l pre-size))
         (pp (ifloor (st rs-pre)))
         (delay (max 1d0 (* (pv rv-predelay) sr 0.001d0)))
         (in-l (read-tap s pre-l pre-size pp delay))
         (in-r (read-tap s pre-r pre-size pp delay))
         (damp (st rs-damp))
         (t0 (+ b rs-t0)))
    (setf (aref s (+ pre-l pp)) (aref s bus))
    (setf (aref s (+ pre-r pp)) (aref s (+ bus 1)))
    (setf (st rs-pre) (dfloat (if (>= (+ pp 1) pre-size) 0 (+ pp 1))))
    (dotimes (i 8)
      (let* ((pos (ifloor (aref s (+ b rs-p0 i))))
             (x (aref s (+ lines (* i line) pos)))
             (lp (+ (aref s (+ damp-state i)) (* damp (- x (aref s (+ damp-state i)))))))
        (setf (aref s (+ damp-state i)) lp)
        (setf (aref s (+ t0 i)) lp)))
    (let* ((wet-l (+ (aref s t0) (aref s (+ t0 2)) (aref s (+ t0 4)) (aref s (+ t0 6))))
           (wet-r (+ (aref s (+ t0 1)) (aref s (+ t0 3)) (aref s (+ t0 5)) (aref s (+ t0 7)))))
      (hadamard s t0)
      (dotimes (i 8)
        (let* ((pos (ifloor (aref s (+ b rs-p0 i))))
               (n (ifloor (aref s (+ b rs-n0 i))))
               (input (if (= (mod i 2) 0) in-l in-r)))
          (setf (aref s (+ lines (* i line) pos))
                (+ (* input 0.35355339059327373d0)
                   (* (aref s (+ t0 i)) (aref s (+ b rs-g0 i)) 0.35355339059327373d0)))
          (setf (aref s (+ b rs-p0 i)) (dfloat (if (>= (+ pos 1) n) 0 (+ pos 1))))))
      (mix-into s bus (* wet-l 0.5d0) (* wet-r 0.5d0) (pv rv-mix)))))

;; Compressor, and sidechain ducking when another track is its key.

(define-params cp :compressor
  (:threshold "Threshold" :default -18 :min -60 :max 0 :unit "dB" :doc "Level above which it turns the signal down.")
  (:ratio "Ratio" :default 4 :min 1 :max 30 :doc "How hard it turns down: 2 is gentle, 10 is hard, 20 is limiting.")
  (:attack "Attack" :default 5 :min 0.05 :max 200 :unit "ms" :scale :log :doc "How fast it reacts. Slow lets transients through, which is punchy.")
  (:release "Release" :default 80 :min 5 :max 2000 :unit "ms" :scale :log :doc "How fast it recovers. When ducking to the kick, set it to breathe in time with the beat.")
  (:makeup "Makeup" :default 0 :min 0 :max 24 :unit "dB" :doc "Gain after compression.")
  (:mix "Mix" :default 1 :min 0 :max 1 :doc "Dry blend: below 1 is parallel compression."))

(define-slots cps env gain target acoef rcoef)

(define-dsp fx-compressor-tick ((s :buf) (p :buf) (b :int) (pb :int)) :nil
  (let* ((sr (sg g-sr)))
    (setf (st cps-acoef) (decay-coef (* (pv cp-attack) 0.001d0) sr))
    (setf (st cps-rcoef) (decay-coef (* (pv cp-release) 0.001d0) sr))
    (when (< (st cps-gain) 0.000001d0)
      (setf (st cps-gain) 1d0)
      (setf (st cps-target) 1d0))))

(define-dsp fx-compressor ((s :buf) (p :buf) (b :int) (pb :int) (bus :int) (key :int)) :nil
  (let* ((level (max (abs (aref s key)) (abs (aref s (+ key 1)))))
         (env (st cps-env))
         (next (if (> level env)
                   (+ level (* (- env level) (st cps-acoef)))
                   (+ level (* (- env level) (st cps-rcoef))))))
    (setf (st cps-env) next)
    (when (< (sg g-sub) 0.5d0)
      (let* ((over (- (gain-db (max next 0.000001d0)) (pv cp-threshold)))
             (down (if (> over 0d0) (* over (- 1d0 (/ 1d0 (max 1d0 (pv cp-ratio))))) 0d0)))
        (setf (st cps-target) (db-gain (- (pv cp-makeup) down)))))
    (let* ((g (+ (st cps-gain) (* 0.05d0 (- (st cps-target) (st cps-gain))))))
      (setf (st cps-gain) g)
      (mix-into s bus (* (aref s bus) g) (* (aref s (+ bus 1)) g) (pv cp-mix)))))

;; Utility and limiter.

(define-params ut :utility
  (:gain "Gain" :default 0 :min -60 :max 24 :unit "dB" :doc "Level.")
  (:width "Width" :default 1 :min 0 :max 2 :doc "Stereo width: 0 is mono, 1 unchanged, 2 very wide. Keep bass near 0.")
  (:pan "Balance" :default 0 :min -1 :max 1 :doc "Left -1 to right 1."))

(define-slots us gain)

(define-dsp fx-utility-tick ((s :buf) (p :buf) (b :int) (pb :int)) :nil
  (setf (st us-gain) (db-gain (pv ut-gain))))

(define-dsp fx-utility ((s :buf) (p :buf) (b :int) (pb :int) (bus :int)) :nil
  (let* ((l (aref s bus))
         (r (aref s (+ bus 1)))
         (m (* 0.5d0 (+ l r)))
         (d (* 0.5d0 (- l r) (pv ut-width)))
         (a (* (+ (pv ut-pan) 1d0) 0.125d0))
         (g (* (st us-gain) 1.4142135623730951d0)))
    (setf (aref s bus) (* (+ m d) g (d-sin (+ a 0.25d0))))
    (setf (aref s (+ bus 1)) (* (- m d) g (d-sin a)))))

(define-params lm :limiter
  (:ceiling "Ceiling" :default -0.3 :min -24 :max 0 :unit "dB" :doc "The loudest the output may be.")
  (:input "Input" :default 0 :min 0 :max 24 :unit "dB" :doc "Push into the limiter: louder, but with less punch at high settings.")
  (:release "Release" :default 100 :min 5 :max 1000 :unit "ms" :scale :log :doc "How fast the level recovers."))

(define-slots ls gain ceiling input rcoef)

(define-dsp fx-limiter-tick ((s :buf) (p :buf) (b :int) (pb :int)) :nil
  (setf (st ls-ceiling) (db-gain (pv lm-ceiling)))
  (setf (st ls-input) (db-gain (pv lm-input)))
  (setf (st ls-rcoef) (decay-coef (* (pv lm-release) 0.001d0) (sg g-sr)))
  (when (< (st ls-gain) 0.000001d0)
    (setf (st ls-gain) 1d0)))

(define-dsp fx-limiter ((s :buf) (p :buf) (b :int) (pb :int) (bus :int)) :nil
  (let* ((l (* (aref s bus) (st ls-input)))
         (r (* (aref s (+ bus 1)) (st ls-input)))
         (peak (max (abs l) (abs r)))
         (ceiling (st ls-ceiling))
         (want (if (> peak ceiling) (/ ceiling peak) 1d0))
         (g (if (< want (st ls-gain))
                want
                (+ 1d0 (* (- (st ls-gain) 1d0) (st ls-rcoef))))))
    (setf (st ls-gain) g)
    (setf (aref s bus) (* l g))
    (setf (aref s (+ bus 1)) (* r g))))

;;; What the engine generator needs to know of each kind.

(defstruct (device-kind (:constructor make-device-kind (key label type tick run state doc)))
  "KEY names the kind; TYPE is :instrument or :effect; TICK and RUN the
DSP functions; STATE the frames of state it needs at a sample rate, a
function; DOC what it is for."
  key label type tick run state doc)

(defvar *device-kinds* nil "Every device-kind, in the browser's order.")

(defun device-kind (key)
  (or (find key *device-kinds* :key #'device-kind-key)
      (error "~s is not a device.  The devices are: ~{~(~s~)~^, ~}."
             key (mapcar #'device-kind-key *device-kinds*))))

(defun reverb-state (sr)
  (let ((line (floor (+ 1d0 (* sr 0.13d0))))
        (pre (floor (* sr 0.25d0))))
    (+ rs-size (* 8 line) 8 (* 2 pre))))

(setf *device-kinds*
      (list
       (make-device-kind :synth "Synth" :instrument 'synth-tick 'synth-run
                         (lambda (sr) (declare (ignore sr)) (+ sy-size (* 8 v-size)))
                         "A polyphonic subtractive synthesizer: two oscillators with FM, sub and noise, a resonant filter, two envelopes and an LFO. Basses, leads, pads, plucks, acid lines and effects.")
       (make-device-kind :drums "Drums" :instrument nil 'drum-run
                         (lambda (sr) (declare (ignore sr)) (+ dk-size (* 8 dv-size)))
                         "A drum kit of eight synthesized pads, played by pitches 36 to 43.")
       (make-device-kind :sampler "Audio" :instrument 'sampler-tick 'sampler-run
                         (lambda (sr) (declare (ignore sr)) (+ sa-size (* 4 sv-size)))
                         "A track of recorded sound: spoken word, field recordings, loops. It plays audio clips, up to four at once.")
       (make-device-kind :filter "Auto Filter" :effect 'fx-filter-tick 'fx-filter
                         (lambda (sr) (declare (ignore sr)) fs-size)
                         "A resonant filter with an LFO. For sweeps, build-ups and rhythmic wobble.")
       (make-device-kind :eq "EQ Three" :effect 'fx-eq-tick 'fx-eq
                         (lambda (sr) (declare (ignore sr)) es-size)
                         "Three bands of gain. For carving each sound a place in the mix.")
       (make-device-kind :drive "Saturator" :effect 'fx-drive-tick 'fx-drive
                         (lambda (sr) (declare (ignore sr)) ds-size)
                         "Soft clipping: warmth at low amounts, distortion at high.")
       (make-device-kind :compressor "Compressor" :effect 'fx-compressor-tick 'fx-compressor
                         (lambda (sr) (declare (ignore sr)) cps-size)
                         "Evens out level, or with a sidechain ducks this track when another plays.")
       (make-device-kind :delay "Delay" :effect 'fx-delay-tick 'fx-delay
                         (lambda (sr) (+ dls-size (* 2 4 sr)))
                         "Tempo-synced echoes, with ping-pong and filtered repeats.")
       (make-device-kind :chorus "Chorus" :effect 'fx-chorus-tick 'fx-chorus
                         (lambda (sr) (+ cs-size (* 2 (floor (* sr 0.0625d0)))))
                         "A modulated short delay: chorus for width, flanger for jet sweeps.")
       (make-device-kind :phaser "Phaser" :effect 'fx-phaser-tick 'fx-phaser
                         (lambda (sr) (declare (ignore sr)) (+ ps-size 12))
                         "Sweeping notches: swirl and motion.")
       (make-device-kind :reverb "Reverb" :effect 'fx-reverb-tick 'fx-reverb #'reverb-state
                         "A room or hall behind the sound. Best on a return track.")
       (make-device-kind :utility "Utility" :effect 'fx-utility-tick 'fx-utility
                         (lambda (sr) (declare (ignore sr)) us-size)
                         "Gain, stereo width and balance.")
       (make-device-kind :limiter "Limiter" :effect 'fx-limiter-tick 'fx-limiter
                         (lambda (sr) (declare (ignore sr)) ls-size)
                         "Holds the level under a ceiling. Belongs last on the master.")))
