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
;; Repairing recorded sound: what a phone's voice memo needs before it
;; sits in a mix.  Each repair works offline, here, on a sample's
;; numbers and makes new ones, kept by content as any sample is; the
;; song then points at them (api.lisp), so undo is the song's own, and
;; the page and the renderer still read the same numbers.  None of this
;; runs on the audio thread, so it may use the Lisp's own sin and cos.
;;
;; Spectral noise reduction is the classic method: learn the noise's
;; spectrum where nobody is speaking -- the gaps between phrases, which
;; sample-info already finds -- and in every short frame of the
;; recording turn each frequency down by how much of it that noise
;; explains.  Hum is mains electricity, 50 or 60 Hz and its harmonics,
;; taken out by narrow notches run forward and back so nothing shifts.
;;

(defun pcm-channels-of (pcm)
  "PCM's channels, each a vector of doubles."
  (let ((frames (pcm-frames pcm))
        (data (pcm-data pcm)))
    (loop for c below (pcm-channels pcm)
          collect (let ((out (double-buffer frames)))
                    (dotimes (i frames out)
                      (setf (aref out i) (float (aref data (+ (* c frames) i)) 1d0)))))))

(defun channels-octets (channels)
  "CHANNELS, vectors of the same length, as the float32 octets a sample is
stored as: one channel after the other."
  (let ((all (make-array (reduce #'+ channels :key #'length) :element-type 'double-float))
        (at 0))
    (dolist (channel channels)
      (replace all channel :start1 at)
      (incf at (length channel)))
    (floats-octets all)))

(defun peak-of-channels (channels)
  (loop for channel in channels
        maximize (loop for x across channel maximize (abs x))))

(defun rms-in (channel ranges sr)
  "The RMS of CHANNEL over RANGES, ((start end)...) in seconds."
  (let ((sum 0d0) (n 0))
    (loop for (a b) in ranges
          do (loop for i from (max 0 (round (* a sr))) below (min (length channel) (round (* b sr)))
                   do (incf sum (* (aref channel i) (aref channel i)))
                      (incf n)))
    (if (plusp n) (sqrt (/ sum n)) 0d0)))

;;; Level.

(defun scale-channels (channels gain)
  (mapcar (lambda (channel) (map '(simple-array double-float (*)) (lambda (x) (* x gain)) channel))
          channels))

(defun normalize-channels (channels peak-db)
  "CHANNELS scaled so their loudest sample is at PEAK-DB."
  (let ((peak (peak-of-channels channels)))
    (when (< peak 1d-9)
      (error "The sample is silent, so there is nothing to normalize."))
    (values (scale-channels channels (/ (expt 10d0 (/ peak-db 20)) peak))
            (list :was (db-of peak) :now peak-db))))

(defun trim-channels (channels sr threshold-db pad)
  "CHANNELS without the quiet before the first sound and after the last,
quiet being under THRESHOLD-DB, PAD seconds left either side: (values
channels report seconds-cut-from-the-front)."
  (let* ((level (expt 10d0 (/ threshold-db 20)))
         (frames (length (first channels)))
         (loud (lambda (i) (some (lambda (c) (> (abs (aref c i)) level)) channels)))
         (first-loud (loop for i below frames when (funcall loud i) return i))
         (last-loud (loop for i downfrom (1- frames) to 0 when (funcall loud i) return i)))
    (unless first-loud
      (error "Nothing in the sample is louder than ~a dB, so there is nothing to keep." threshold-db))
    (let* ((padding (round (* pad sr)))
           (from (max 0 (- first-loud padding)))
           (to (min frames (+ last-loud padding 1))))
      (values (mapcar (lambda (c) (subseq c from to)) channels)
              (list :cut-front (/ (round (* 1000 (/ from sr))) 1000d0)
                    :cut-end (/ (round (* 1000 (/ (- frames to) sr))) 1000d0))
              (/ from (float sr 1d0))))))

;;; Hum.

(defun goertzel-power (channel hz sr &optional (most-frames (* 10 sr)))
  "The power of CHANNEL at HZ, over its first MOST-FRAMES frames."
  (let* ((n (min (length channel) most-frames))
         (w (/ (* 2 pi hz) sr))
         (c (* 2 (cos w)))
         (s1 0d0) (s2 0d0))
    (dotimes (i n)
      (let ((s0 (+ (aref channel i) (* c s1) (- s2))))
        (setf s2 s1 s1 s0)))
    (/ (+ (* s1 s1) (* s2 s2) (- (* c s1 s2))) (max 1 (* n n)))))

(defun hum-score (channel base sr)
  "How far the first three harmonics of BASE stand above their neighbors."
  (loop for k from 1 to 3
        for hz = (* k base)
        sum (/ (goertzel-power channel hz sr)
               (max 1d-30 (* 0.5d0 (+ (goertzel-power channel (- hz 7) sr)
                                      (goertzel-power channel (+ hz 7) sr)))))))

(defun notch! (channel hz sr q)
  "CHANNEL through a notch at HZ, forward then backward, in place."
  (let* ((w0 (/ (* 2 pi hz) sr))
         (alpha (/ (sin w0) (* 2 q)))
         (cosw (cos w0))
         (a0 (+ 1 alpha))
         (b0 (/ 1 a0)) (b1 (/ (* -2 cosw) a0)) (b2 (/ 1 a0))
         (a1 (/ (* -2 cosw) a0)) (a2 (/ (- 1 alpha) a0))
         (n (length channel))
         (x1 0d0) (x2 0d0) (y1 0d0) (y2 0d0))
    (flet ((step! (i)
             (let* ((x (aref channel i))
                    (y (- (+ (* b0 x) (* b1 x1) (* b2 x2)) (* a1 y1) (* a2 y2))))
               (setf x2 x1 x1 x y2 y1 y1 y)
               (setf (aref channel i) y))))
      (loop for i from 0 below n do (step! i))
      (setf x1 0d0 x2 0d0 y1 0d0 y2 0d0)
      (loop for i downfrom (1- n) to 0 do (step! i))
      channel)))

(defun dehum-channels (channels sr frequency harmonics)
  "CHANNELS with the hum at FREQUENCY (50, 60, or :auto to find which)
and its HARMONICS taken out."
  (let ((base (if (eq frequency :auto)
                  (let ((at-50 (hum-score (first channels) 50 sr))
                        (at-60 (hum-score (first channels) 60 sr)))
                    (cond ((and (< at-50 12) (< at-60 12))
                           (error "There is no hum at 50 or 60 Hz to take out: neither stands clearly above its neighbors.  Say :frequency 50 or 60 to take it out anyway."))
                          ((> at-60 at-50) 60)
                          (t 50)))
                  frequency)))
    (unless (member base '(50 60))
      (error "Mains hum is at 50 or 60 Hz, not ~s." base))
    (let ((before (goertzel-power (first channels) base sr))
          (out (mapcar #'copy-seq channels)))
      (dolist (channel out)
        (loop for k from 1 to harmonics
              for hz = (* k base)
              while (< hz (* 0.45 sr))
              ;; a band about 2 Hz wide at every harmonic
              do (notch! channel hz sr (/ hz 2d0))))
      (values out
              (list :frequency base :harmonics harmonics
                    :hum-reduced-by (/ (round (* 10 (* 10 (log (/ before (max 1d-30 (goertzel-power (first out) base sr))) 10)))) 10d0))))))

;;; Noise.

(defparameter *stft-size* 2048)
(defparameter *stft-hop* 512)

(defun noise-ranges (pcm sr)
  "Where PCM has no sound: the gaps between its phrases, as ((start
end)...) in seconds, the ones long enough to learn from."
  (let* ((seconds (/ (pcm-frames pcm) (float sr 1d0)))
         (phrases (pcm-phrases pcm sr))
         (edges (append (list 0d0) (loop for (a b) in phrases collect a collect b) (list seconds))))
    (loop for (a b) on edges by #'cddr
          when (and b (> (- b a) 0.1d0)) collect (list a b))))

(defun stft-frames (length)
  "The start of every analysis frame over LENGTH samples, the first and
last reaching past the ends."
  (loop for p from (- *stft-size*) below length by *stft-hop* collect p))

(defun frame-spectrum! (channel p window re im)
  "The windowed frame of CHANNEL at P into RE and IM, transformed."
  (let ((n *stft-size*) (length (length channel)))
    (dotimes (i n)
      (let ((at (+ p i)))
        (setf (aref re i) (if (< -1 at length) (* (aref channel at) (aref window i)) 0d0)
              (aref im i) 0d0)))
    (fft! re im)))

(defun periodic-hann (n)
  (let ((w (double-buffer n)))
    (dotimes (i n w)
      (setf (aref w i) (* 0.5d0 (- 1 (cos (/ (* 2 pi i) n))))))))

(defun noise-profile (channel frames window sr ranges)
  "The noise's power in each bin: the mean over the frames that lie
wholly inside RANGES, or over the quietest tenth when too few do."
  (let* ((n *stft-size*) (half (1+ (ash n -1)))
         (re (double-buffer n)) (im (double-buffer n))
         (inside (remove-if-not (lambda (p)
                                  (some (lambda (r) (and (>= p (* (first r) sr)) (<= (+ p n) (* (second r) sr))))
                                        ranges))
                                frames))
         (chosen (if (>= (length inside) 3)
                     inside
                     ;; no usable gaps: the quietest tenth of the frames
                     (let ((by-energy (sort (mapcar (lambda (p)
                                                      (cons p (loop for i from (max 0 p) below (min (length channel) (+ p n))
                                                                    sum (expt (aref channel i) 2))))
                                                    (remove-if (lambda (p) (or (< p 0) (> (+ p n) (length channel)))) frames))
                                            #'< :key #'cdr)))
                       (mapcar #'car (subseq by-energy 0 (max 1 (floor (length by-energy) 10)))))))
         (power (double-buffer half)))
    (when (null chosen)
      (error "The sample is too short to learn its noise from."))
    (dolist (p chosen)
      (frame-spectrum! channel p window re im)
      (dotimes (k half)
        (incf (aref power k) (+ (expt (aref re k) 2) (expt (aref im k) 2)))))
    (dotimes (k half power)
      (setf (aref power k) (/ (aref power k) (length chosen))))))

(defun denoise-channel (channel sr ranges reduction sensitivity)
  "CHANNEL with its steady noise turned down, by at most REDUCTION dB."
  (let* ((n *stft-size*) (half (1+ (ash n -1)))
         (window (periodic-hann n))
         (frames (stft-frames (length channel)))
         (noise (noise-profile channel frames window sr ranges))
         (floor (expt 10d0 (/ (- reduction) 20)))
         (over (* 3d0 sensitivity))
         (re (double-buffer n)) (im (double-buffer n))
         (gains (double-buffer half))
         (previous (make-array half :element-type 'double-float :initial-element 1d0))
         (out (double-buffer (length channel))))
    (dolist (p frames)
      (frame-spectrum! channel p window re im)
      ;; how much of each bin the noise explains, smoothed across
      ;; neighboring bins and with the frame before, which keeps the
      ;; leftover noise from twinkling
      (dotimes (k half)
        (let ((power (+ (expt (aref re k) 2) (expt (aref im k) 2))))
          (setf (aref gains k)
                (max floor (sqrt (max 0d0 (- 1 (/ (* over (aref noise k)) (max power 1d-30)))))))))
      (dotimes (k half)
        (let* ((smooth (/ (+ (aref gains (max 0 (1- k))) (* 2 (aref gains k)) (aref gains (min (1- half) (1+ k)))) 4))
               ;; quick to open, so a word's start is not softened; slow
               ;; to close, so what is left of the noise does not twinkle
               (g (max floor (if (> smooth (aref previous k))
                                 smooth
                                 (* 0.5d0 (+ smooth (aref previous k)))))))
          (setf (aref previous k) g)
          (setf (aref re k) (* (aref re k) g)
                (aref im k) (* (aref im k) g))
          (when (< 0 k (ash n -1))
            (setf (aref re (- n k)) (* (aref re (- n k)) g)
                  (aref im (- n k)) (* (aref im (- n k)) g)))))
      ;; back to time: the inverse transform is the forward one conjugated
      (dotimes (i n) (setf (aref im i) (- (aref im i))))
      (fft! re im)
      (dotimes (i n)
        (let ((at (+ p i)))
          (when (< -1 at (length channel))
            ;; Hann in and out at a quarter hop overlaps to 1.5
            (incf (aref out at) (/ (* (aref re i) (aref window i)) (* n 1.5d0)))))))
    out))

(defun denoise-channels (channels pcm sr reduction sensitivity ranges)
  (let* ((ranges (or ranges (noise-ranges pcm sr)))
         (out (mapcar (lambda (c) (denoise-channel c sr ranges reduction sensitivity)) channels))
         (speech (let ((phrases (pcm-phrases pcm sr))) (or phrases (list (list 0 (/ (pcm-frames pcm) sr)))))))
    (values out
            (list :noise-was (db-of (rms-in (first channels) ranges sr))
                  :noise-now (db-of (rms-in (first out) ranges sr))
                  :sound-was (db-of (rms-in (first channels) speech sr))
                  :sound-now (db-of (rms-in (first out) speech sr))
                  :learned-from (if ranges (format nil "~d gaps between phrases" (length ranges)) "the quietest frames")))))
