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
;; The arithmetic of sound, from + - * / alone (dual.lisp says why):
;; sine, tangent, powers of two, logarithms, soft clipping and noise;
;; then the oscillator, the state-variable filter and the envelope
;; everything else is made of.
;;

;; The engine's state, at the start of S.  BEAT is the song position;
;; BPF how far a frame moves it; SUB and TICK count frames to the next
;; slow update (16) and the next control update (64); RESEEK asks the
;; sequencers to find their place again, from beat SEEK.
(define-slots g beat playing bpf sr isr sub tick reseek seek loop-on loop-start loop-end)

;; The song's own parameters, at the start of P.
(define-slots gp tempo loop-on loop-start loop-end)

(define-dsp d-sin ((x :num)) :inline
  "sin(2 pi X): X in turns, any size."
  (let* ((r (- x (dfloor (+ x 0.5d0))))
         (q (if (> r 0.25d0) (- 0.5d0 r) (if (< r -0.25d0) (- -0.5d0 r) r)))
         (y (* q 6.283185307179586d0))
         (y2 (* y y)))
    (* y (+ 1d0 (* y2 (+ -0.16666666666666666d0
                         (* y2 (+ 0.008333333333333333d0
                                  (* y2 (+ -0.0001984126984126984d0
                                           (* y2 (+ 2.7557319223985893d-6
                                                    (* y2 -2.505210838544172d-8)))))))))))))

(define-dsp d-tan ((x :num)) :num
  "tan(pi X) for X in (0, 0.5)."
  (/ (d-sin (* x 0.5d0)) (d-sin (+ (* x 0.5d0) 0.25d0))))

(define-dsp d-exp2 ((x :num)) :num
  "2 to the X, held within 2^-60 and 2^60.  The whole part is exact."
  (let* ((c (max -60d0 (min 60d0 x)))
         (e (dfloor c))
         (f (- c e))
         (y (+ 1d0 (* f (+ 0.6931471805599453d0
                           (* f (+ 0.2402265069591007d0
                                   (* f (+ 0.05550410866482158d0
                                           (* f (+ 0.009618129107628477d0
                                                   (* f (+ 0.0013333558146428443d0
                                                           (* f (+ 0.00015403530393381608d0
                                                                   (* f (+ 1.5252733804059841d-5
                                                                           (* f 1.3215486790144307d-6))))))))))))))))))
    (loop while (>= e 1d0)
          do (setf y (* y 2d0))
             (setf e (- e 1d0)))
    (loop while (< e 0d0)
          do (setf y (* y 0.5d0))
             (setf e (+ e 1d0)))
    y))

(define-dsp d-log2 ((x :num)) :num
  "The base-two logarithm of X, read as 2^-40 below that."
  (let* ((m (max x 9.094947017729282d-13))
         (e 0d0))
    (loop while (>= m 2d0)
          do (setf m (* m 0.5d0))
             (setf e (+ e 1d0)))
    (loop while (< m 1d0)
          do (setf m (* m 2d0))
             (setf e (- e 1d0)))
    (let* ((y (/ (- m 1d0) (+ m 1d0)))
           (y2 (* y y)))
      (+ e (* 2.885390081777927d0 y
              (+ 1d0 (* y2 (+ 0.3333333333333333d0
                              (* y2 (+ 0.2d0
                                       (* y2 (+ 0.14285714285714285d0
                                                (* y2 0.1111111111111111d0)))))))))))))

(define-dsp db-gain ((db :num)) :num
  "DB decibels as a linear gain."
  (d-exp2 (* db 0.16609640474436813d0)))

(define-dsp gain-db ((g :num)) :num
  "The linear gain G in decibels."
  (* (d-log2 g) 6.020599913279624d0))

(define-dsp pitch-hz ((pitch :num)) :num
  "The frequency of a MIDI PITCH, which may be fractional."
  (* 440d0 (d-exp2 (/ (- pitch 69d0) 12d0))))

(define-dsp decay-coef ((seconds :num) (sr :num)) :num
  "The multiplier per frame that falls 60 dB in SECONDS."
  (d-exp2 (/ -9.965784284662087d0 (* (max seconds 0.0001d0) sr))))

(define-dsp d-soft ((x :num)) :inline
  "A smooth limit at plus and minus one, close to tanh."
  (let* ((c (max -3d0 (min 3d0 x)))
         (c2 (* c c)))
    (/ (* c (+ 27d0 c2)) (+ 27d0 (* 9d0 c2)))))

(define-dsp next-noise ((s :buf) (i :int)) :inline
  "White noise in [-1, 1) from the generator whose state is S[I]."
  (let* ((x (* (aref s i) 16807d0))
         (q (dfloor (/ x 2147483647d0)))
         (r (- x (* q 2147483647d0)))
         (r2 (if (< r 1d0) (+ r 2147483646d0) r)))
    (setf (aref s i) r2)
    (- (* r2 9.313225750491594d-10) 1d0)))

(define-dsp wrap-phase ((x :num)) :inline
  "X, a phase just past either end, back into [0, 1)."
  (- x (dfloor x)))

(define-dsp poly-blep ((ph :num) (dt :num)) :inline
  "The correction that takes the alias out of a jump at phase zero."
  (let* ((a (/ ph dt))
         (c (/ (- ph 1d0) dt)))
    (if (< ph dt)
        (- (- (* 2d0 a) (* a a)) 1d0)
        (if (> ph (- 1d0 dt))
            (+ (+ (* c c) (* 2d0 c)) 1d0)
            0d0))))

(define-dsp osc-wave ((ph :num) (dt :num) (w :num)) :inline
  "One sample of wave W (0 saw, 1 square, 2 triangle, 3 sine) at phase PH."
  (if (< w 0.5d0)
      (- (- (* 2d0 ph) 1d0) (poly-blep ph dt))
      (if (< w 1.5d0)
          (- (+ (if (< ph 0.5d0) 1d0 -1d0) (poly-blep ph dt))
             (poly-blep (wrap-phase (+ ph 0.5d0)) dt))
          (if (< w 2.5d0)
              (- (* 4d0 (abs (- ph 0.5d0))) 1d0)
              (d-sin ph)))))

(define-dsp lfo-wave ((ph :num) (w :num) (held :num)) :inline
  "One value of low-frequency shape W (0 sine, 1 triangle, 2 saw, 3
square, 4 random hold, whose value is HELD)."
  (if (< w 0.5d0)
      (d-sin ph)
      (if (< w 1.5d0)
          (- (* 4d0 (abs (- ph 0.5d0))) 1d0)
          (if (< w 2.5d0)
              (- (* 2d0 ph) 1d0)
              (if (< w 3.5d0)
                  (if (< ph 0.5d0) 1d0 -1d0)
                  held)))))

;;; The state-variable filter (the trapezoidal one): it stays stable
;;; however fast its cutoff moves, which is what filter sweeps do.  Four
;;; coefficients at O: a1 a2 a3 k.

(define-dsp svf-coefs ((s :buf) (o :int) (hz :num) (res :num)) :nil
  "The coefficients at S[O] for cutoff HZ and resonance RES, 0 to 1."
  (let* ((c (max 0.0002d0 (min 0.49d0 (* hz (sg g-isr)))))
         (g (d-tan c))
         (k (- 2d0 (* 1.96d0 (max 0d0 (min 1d0 res)))))
         (a1 (/ 1d0 (+ 1d0 (* g (+ g k)))))
         (a2 (* g a1))
         (a3 (* g a2)))
    (setf (aref s o) a1)
    (setf (aref s (+ o 1)) a2)
    (setf (aref s (+ o 2)) a3)
    (setf (aref s (+ o 3)) k)))

(define-dsp svf-step ((s :buf) (o :int) (c :int) (x :num) (mode :num)) :inline
  "X through the filter with state at S[O] and coefficients at S[C]:
MODE 0 low pass, 1 band pass, 2 high pass."
  (let* ((a1 (aref s c))
         (a2 (aref s (+ c 1)))
         (a3 (aref s (+ c 2)))
         (k (aref s (+ c 3)))
         (ic1 (aref s o))
         (ic2 (aref s (+ o 1)))
         (v3 (- x ic2))
         (v1 (+ (* a1 ic1) (* a2 v3)))
         (v2 (+ ic2 (* a2 ic1) (* a3 v3))))
    (setf (aref s o) (- (* 2d0 v1) ic1))
    (setf (aref s (+ o 1)) (- (* 2d0 v2) ic2))
    (if (< mode 0.5d0)
        v2
        (if (< mode 1.5d0)
            v1
            (- (- x (* k v1)) v2)))))

(define-dsp env-step ((s :buf) (o :int) (a :num) (d :num) (u :num) (r :num)) :nil
  "One frame of the envelope at S[O] (stage, level): stage 1 rises by A,
2 falls toward U by D, 3 falls by R.  Stage 0 rests."
  (let* ((stage (aref s o))
         (level (aref s (+ o 1))))
    (cond ((< stage 0.5d0) nil)
          ((< stage 1.5d0)
           (let* ((up (+ level a)))
             (if (>= up 1d0)
                 (progn (setf (aref s o) 2d0)
                        (setf (aref s (+ o 1)) 1d0))
                 (setf (aref s (+ o 1)) up))))
          ((< stage 2.5d0)
           (setf (aref s (+ o 1)) (+ u (* (- level u) d))))
          (t (setf (aref s (+ o 1)) (* level r))))))

(define-dsp read-tap ((s :buf) (o :int) (size :int) (pos :int) (delay :num)) :inline
  "The delay line of SIZE frames at S[O], read DELAY frames before the
write position POS, between frames by a straight line."
  (let* ((r (- (dfloat pos) delay))
         (w (if (< r 0d0) (+ r (dfloat size)) r))
         (i (ifloor w))
         (f (- w (dfloat i)))
         (j (if (>= (+ i 1) size) 0 (+ i 1)))
         (x (aref s (+ o i))))
    (+ x (* f (- (aref s (+ o j)) x)))))
