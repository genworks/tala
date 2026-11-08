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
;; A text model cannot hear, so it is shown: the server renders the
;; song with the very program the browser plays, and turns the frames
;; into a spectrogram and a few measures in words.  What Stratum's look
;; is to a picture, HEAR is to a sound.  The WAV writer lives here too,
;; since it starts from the same frames.
;;

(defstruct (picture (:constructor make-picture (png note width height)))
  "What (hear) answers: PNG octets WIDTH x HEIGHT and NOTE, the words
that go with them."
  png note width height)

(defun png-octets (rgb width height)
  "RGB bytes as a PNG in memory."
  (let ((png (make-instance 'zpng:png :color-type :truecolor :width width :height height
                                      :image-data rgb)))
    (flexi-streams:with-output-to-sequence (s)
      (zpng:write-png-stream png s))))

(defun fft! (re im)
  "The discrete Fourier transform of RE and IM in place; their length a
power of two."
  (declare (type (simple-array double-float (*)) re im))
  (let* ((n (length re))
         (j 0))
    (declare (type fixnum n j))
    (loop for i from 1 below n
          do (let ((bit (ash n -1)))
               (loop while (logtest j bit)
                     do (setf j (logxor j bit)
                              bit (ash bit -1)))
               (setf j (logxor j bit))
               (when (< i j)
                 (rotatef (aref re i) (aref re j))
                 (rotatef (aref im i) (aref im j)))))
    (loop for len = 2 then (* len 2)
          while (<= len n)
          do (let* ((angle (/ (* -2d0 pi) len))
                    (wr (float (cos angle) 1d0))
                    (wi (float (sin angle) 1d0))
                    (half (ash len -1)))
               (loop for i from 0 below n by len
                     do (let ((cr 1d0) (ci 0d0))
                          (declare (type double-float cr ci))
                          (dotimes (k half)
                            (let* ((a (+ i k))
                                   (b (+ a half))
                                   (xr (- (* cr (aref re b)) (* ci (aref im b))))
                                   (xi (+ (* cr (aref im b)) (* ci (aref re b)))))
                              (setf (aref re b) (- (aref re a) xr)
                                    (aref im b) (- (aref im a) xi))
                              (incf (aref re a) xr)
                              (incf (aref im a) xi)
                              (psetf cr (- (* cr wr) (* ci wi))
                                     ci (+ (* cr wi) (* ci wr)))))))))
    (values re im)))

(defparameter *window-frames* 2048 "Frames in one column's analysis.")

(defun hann-window (n)
  (let ((w (double-buffer n)))
    (dotimes (i n w)
      (setf (aref w i) (* 0.5d0 (- 1d0 (cos (/ (* 2d0 pi i) (1- n)))))))))

(defun power-spectrum (left right start window out)
  "The power of the mono mix at frame START into OUT, half a window long."
  (let* ((n (length window))
         (re (double-buffer n))
         (im (double-buffer n))
         (frames (length left)))
    (dotimes (i n)
      (let ((at (+ start i)))
        (when (< -1 at frames)
          (setf (aref re i) (* 0.5d0 (aref window i) (+ (aref left at) (aref right at)))))))
    (fft! re im)
    (dotimes (i (length out) out)
      (setf (aref out i) (+ (* (aref re i) (aref re i)) (* (aref im i) (aref im i)))))))

(defun heat (x)
  "A color for X from 0 to 1: black through violet and orange to white."
  (flet ((ramp (a b) (max 0 (min 255 (round (* 255 (/ (- x a) (- b a))))))))
    (values (ramp 0.25 0.75)
            (ramp 0.55 1.0)
            (let ((up (ramp 0.0 0.3))
                  (down (ramp 0.45 0.6)))
              (max (- up down) (ramp 0.85 1.0))))))

(defparameter *picture-width* 960)
(defparameter *picture-height* 320)
(defparameter *lowest-hz* 30d0)
(defparameter *highest-hz* 18000d0)

(defun row-hz (row height)
  "The frequency at picture row ROW, the top row the highest."
  (* *lowest-hz*
     (expt (/ *highest-hz* *lowest-hz*) (/ (- height 1 row) (float (1- height) 1d0)))))

(defun hz-row (hz height)
  (round (- height 1 (* (1- height) (/ (log (/ hz *lowest-hz*)) (log (/ *highest-hz* *lowest-hz*)))))))

(defun spectrogram-rgb (left right sr bars-at width height)
  "A log-frequency spectrogram of LEFT and RIGHT as RGB bytes, with a
line at each frame of BARS-AT and at 100 Hz, 1 kHz and 10 kHz."
  (let* ((frames (length left))
         (window (hann-window *window-frames*))
         (half (ash *window-frames* -1))
         (power (double-buffer half))
         (rgb (make-array (* 3 width height) :element-type '(unsigned-byte 8) :initial-element 0))
         (bin-of (make-array height :element-type 'double-float)))
    (dotimes (row height)
      (setf (aref bin-of row) (float (/ (* (row-hz row height) *window-frames*) sr) 1d0)))
    (dotimes (x width)
      (power-spectrum left right (- (floor (* x frames) width) half) window power)
      (dotimes (row height)
        (let* ((position (aref bin-of row))
               (a (min (- half 2) (floor position)))
               (top (min (- half 1) (ceiling (aref bin-of (max 0 (1- row))))))
               (value (loop for i from a to (max a top) maximize (aref power i)))
               (db (* 10 (log (max value 1d-12) 10)))
               (level (max 0d0 (min 1d0 (/ (+ db 30d0) 90d0))))
               (o (* 3 (+ x (* row width)))))
          (multiple-value-bind (r g b) (heat level)
            (setf (aref rgb o) r (aref rgb (+ o 1)) g (aref rgb (+ o 2)) b)))))
    (flet ((mark (x row shade)
             (let ((o (* 3 (+ x (* row width)))))
               (dotimes (c 3)
                 (setf (aref rgb (+ o c)) (max (aref rgb (+ o c)) shade))))))
      (dolist (frame bars-at)
        (let ((x (floor (* frame width) frames)))
          (when (< -1 x width)
            (dotimes (row height) (mark x row 90)))))
      (dolist (hz '(100 1000 10000))
        (let ((row (hz-row hz height)))
          (dotimes (x width) (mark x row 70)))))
    rgb))

(defun db-of (x)
  "X, a level, in decibels, rounded to a tenth; -120 for silence."
  (if (<= x 1d-6) -120d0 (/ (round (* 200 (log x 10))) 10d0)))

(defun band-levels (left right sr)
  "The energy of each band, in dB below the loudest, as an alist of
(name . db)."
  (let* ((bands '(("sub (under 60 Hz)" 0 60) ("bass (60-250)" 60 250) ("low mids (250-2k)" 250 2000)
                  ("high mids (2k-8k)" 2000 8000) ("air (over 8k)" 8000 24000)))
         (window (hann-window *window-frames*))
         (half (ash *window-frames* -1))
         (power (double-buffer half))
         (sums (make-list (length bands) :initial-element 0d0))
         (frames (length left))
         (hop (max *window-frames* (floor frames 200))))
    (loop for start from 0 below (max 1 (- frames *window-frames*)) by hop
          do (power-spectrum left right start window power)
             (loop for (nil low high) in bands
                   for cell on sums
                   do (let ((from (floor (* low *window-frames*) sr))
                            (to (min half (ceiling (* high *window-frames*) sr))))
                        (loop for i from from below to
                              do (incf (car cell) (aref power i))))))
    (let ((most (reduce #'max sums)))
      (loop for (name) in bands
            for sum in sums
            collect (cons name (if (plusp most)
                                   (/ (round (* 100 (log (max (/ sum most) 1d-12) 10))) 10d0)
                                   -120d0))))))

(defun level-measures (left right frames-per-bar)
  "(values peak rms per-bar-rms-dB correlation clipped-frames)."
  (let* ((n (length left))
         (peak 0d0) (sum 0d0) (lr 0d0) (ll 0d0) (rr 0d0) (clipped 0)
         (bars (ceiling n frames-per-bar))
         (bar-sums (make-array bars :element-type 'double-float :initial-element 0d0)))
    (dotimes (i n)
      (let* ((l (aref left i))
             (r (aref right i))
             (e (* 0.5d0 (+ (* l l) (* r r)))))
        (when (>= (max (abs l) (abs r)) 1d0) (incf clipped))
        (setf peak (max peak (abs l) (abs r)))
        (incf sum e)
        (incf lr (* l r))
        (incf ll (* l l))
        (incf rr (* r r))
        (incf (aref bar-sums (min (1- bars) (floor i frames-per-bar))) e)))
    (values peak
            (sqrt (/ sum (max 1 n)))
            (loop for s across bar-sums
                  for i from 0
                  for length = (min frames-per-bar (- n (* i frames-per-bar)))
                  collect (db-of (sqrt (/ s (max 1 length)))))
            (if (and (plusp ll) (plusp rr)) (/ lr (sqrt (* ll rr))) 1d0)
            clipped)))

(defparameter *most-heard-seconds* 45 "The longest stretch one (hear) renders.")

(defun song-beats-per-bar (spec)
  (* (getf spec :numerator) (/ 4 (getf spec :denominator))))

(defun scene-script (spec row)
  "What launching scene ROW at the first frame is, track by track: a
slot's clip, or a stop for an empty one."
  (loop for track in (getf spec :tracks)
        for i from 0
        collect (list 0 :launch i
                      (if (find (getf (nth row (getf spec :scenes)) :id) (getf track :clips)
                                :key (lambda (c) (getf c :scene)))
                          row
                          -1))))

(defun hear-song (song &key (from 1) (bars 8) track scene (sr 48000))
  "A picture of BARS bars of SONG from bar FROM (the first bar is 1):
the song, or TRACK's spec-less name alone.  The sound the page plays,
rendered here."
  (let* ((spec (the-object song spec))
         (per-bar (song-beats-per-bar spec))
         (tempo (getf spec :tempo))
         (seconds-per-bar (/ (* 60 per-bar) tempo))
         (bars (max 1 (min bars (floor *most-heard-seconds* seconds-per-bar))))
         (frames-per-bar (round (* seconds-per-bar sr)))
         (frames (* bars frames-per-bar))
         (from-beat (* (1- from) per-bar))
         (listened (if track (make-song (solo-spec spec track)) song)))
    (multiple-value-bind (left right)
        (render-song listened frames :sr sr :from from-beat
                                     :script (and scene (scene-script spec scene)))
      (multiple-value-bind (peak rms per-bar-db correlation clipped)
          (level-measures left right frames-per-bar)
        (let* ((width *picture-width*)
               (height *picture-height*)
               (rgb (spectrogram-rgb left right sr
                                     (loop for b from 0 below bars collect (* b frames-per-bar))
                                     width height)))
          (make-picture
           (png-octets rgb width height)
           (format nil "A spectrogram of ~@[scene ~s of the session, ~]~:[the whole song~;~:*track ~a alone~], bars ~d to ~d at ~a BPM.  ~
Left to right is time, ~,1f pixels a bar, with a faint line at the start of each bar.  ~
Bottom to top is frequency on a logarithmic scale, ~d Hz to ~d Hz, with faint lines at 100 Hz, 1 kHz and 10 kHz.  ~
Brighter is louder: black is under -60 dB, violet quiet, orange strong, white at full level.  ~
Peak ~a dB, RMS ~a dB~:[~*~;, ~d frames at or over full scale (clipping)~], stereo correlation ~,2f (1 mono, 0 wide, negative out of phase).  ~
RMS by bar, dB: ~{~a~^ ~}.  ~
Bands, dB below the strongest: ~{~a~^, ~}."
                   (and scene (getf (nth scene (getf spec :scenes)) :name))
                   track from (+ from bars -1) tempo
                   (/ width (float bars)) (round *lowest-hz*) (round *highest-hz*)
                   (db-of peak) (db-of rms) (plusp clipped) clipped correlation
                   per-bar-db
                   (mapcar (lambda (band) (format nil "~a ~a" (car band) (cdr band)))
                           (band-levels left right sr)))
           width height))))))

(defun solo-spec (spec track)
  "SPEC with only the track named TRACK sounding."
  (let ((copy (copy-tree spec))
        (found nil))
    (dolist (one (getf copy :tracks))
      (let ((me (string-equal (getf one :name) track)))
        (when me (setf found t))
        (put! one :solo me)))
    (unless found
      (error "There is no track ~s." track))
    copy))

;;; WAV.

(defun wav-octets (left right sr)
  "LEFT and RIGHT as a 16-bit stereo WAV file, in memory."
  (let* ((frames (length left))
         (data (* 4 frames))
         (out (make-array (+ 44 data) :element-type '(unsigned-byte 8)))
         (at 0))
    (labels ((bytes (string) (loop for c across string do (setf (aref out at) (char-code c)) (incf at)))
             (u16 (x) (setf (aref out at) (logand x 255)
                            (aref out (1+ at)) (logand (ash x -8) 255))
               (incf at 2))
             (u32 (x) (u16 (logand x #xffff)) (u16 (ash x -16)))
             (sample (x) (u16 (logand #xffff (round (* 32767 (max -1d0 (min 1d0 x))))))))
      (bytes "RIFF") (u32 (+ 36 data)) (bytes "WAVE")
      (bytes "fmt ") (u32 16) (u16 1) (u16 2) (u32 sr) (u32 (* 4 sr)) (u16 4) (u16 16)
      (bytes "data") (u32 data)
      (dotimes (i frames)
        (sample (aref left i))
        (sample (aref right i))))
    out))

(defun float32-octets (left right)
  "LEFT then RIGHT as little-endian float32: what the worklet's output
buffers hold, for the browser to compare."
  (let* ((n (length left))
         (out (make-array (* 8 n) :element-type '(unsigned-byte 8)))
         (at 0))
    (flet ((emit (buffer)
             (dotimes (i n)
               (let ((bits (ldb (byte 32 0) (single-bits (aref buffer i)))))
                 (dotimes (k 4)
                   (setf (aref out at) (ldb (byte 8 (* 8 k)) bits))
                   (incf at))))))
      (emit left)
      (emit right))
    out))

