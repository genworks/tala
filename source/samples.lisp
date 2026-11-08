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
;; Recorded sound.  The browser decodes a file once, at the engine's
;; rate, and sends the samples here as float32; from then on those
;; numbers are the sound, on both sides: the page fetches them back as
;; the POOL, one array the program reads, so a sample changes neither
;; the plan nor the agreement to the last bit.  A float32 is exact in a
;; double, and a sample is read as a double at once.
;;
;; A song names its samples by content (a STAMP), so the spec stays a
;; small s-expression, undo costs nothing, and two songs share one copy.
;; With a directory configured they survive a restart.
;;

(defstruct (pcm (:constructor make-pcm (channels frames data)))
  "CHANNELS (1 or 2) of FRAMES frames: DATA holds the left channel and
then the right, as single-floats."
  channels frames data)

(defun single-bits (x)
  "The bits of the double X rounded to a single, as a signed integer."
  (let ((single (with-audio-floats (coerce x 'single-float))))
    #+sbcl (sb-kernel:single-float-bits single)
    #+ccl (ccl::single-float-bits single)
    #-(or sbcl ccl) (error "No single-float-bits here: ~a" single)))

(defun bits-single (bits)
  "The single-float whose 32 BITS these are."
  #+sbcl (sb-kernel:make-single-float (if (logbitp 31 bits) (- bits (ash 1 32)) bits))
  #+ccl (ccl::host-single-float-from-unsigned-byte-32 bits)
  #-(or sbcl ccl) (error "No bits-single here: ~a" bits))

(defun octets-floats (octets)
  "OCTETS, little-endian float32, as a vector of single-floats."
  (let* ((n (floor (length octets) 4))
         (out (make-array n :element-type 'single-float)))
    (with-audio-floats
      (dotimes (i n out)
        (let ((o (* 4 i)))
          (setf (aref out i)
                (bits-single (logior (aref octets o)
                                     (ash (aref octets (+ o 1)) 8)
                                     (ash (aref octets (+ o 2)) 16)
                                     (ash (aref octets (+ o 3)) 24)))))))))

(defun floats-octets (floats)
  "FLOATS as little-endian float32 octets."
  (let ((out (make-array (* 4 (length floats)) :element-type '(unsigned-byte 8)))
        (at 0))
    (loop for x across floats
          do (let ((bits (ldb (byte 32 0) (single-bits x))))
               (dotimes (k 4)
                 (setf (aref out at) (ldb (byte 8 (* 8 k)) bits))
                 (incf at))))
    out))

(defun content-stamp (octets)
  "A name for OCTETS from what they are: 64-bit FNV-1a and the length."
  (let ((h 14695981039346656037))
    (declare (type (unsigned-byte 64) h))
    (loop for x across octets
          do (setf h (ldb (byte 64 0) (* (logxor h x) 1099511628211))))
    (format nil "~(~16,'0x~)-~d" h (length octets))))

(defvar *pcm-table* (make-hash-table :test 'equal) "Stamp -> pcm, in memory.")
(defvar *pcm-lock* (bt:make-lock "tala samples"))

(defvar *sample-directory* nil
  "Where samples are kept across restarts; nil keeps them in memory only.")

(defparameter *most-sample-bytes* 512000000
  "How much recorded sound the server holds before it refuses more: the
room's heap is shared, and one visitor's uploads must not exhaust it.")

(defun configure-samples! (&key (directory nil directory?) (most-bytes nil most?))
  "Keep samples under DIRECTORY, made if need be, or in memory with nil;
hold at most MOST-BYTES of them."
  (when most? (setf *most-sample-bytes* most-bytes))
  (when (and directory? directory)
    (ensure-directories-exist (uiop:ensure-directory-pathname directory)))
  (when directory?
    (setf *sample-directory* (and directory (uiop:ensure-directory-pathname directory))))
  *sample-directory*)

(defun held-sample-bytes ()
  "The bytes of recorded sound in memory."
  (let ((total 0))
    (maphash (lambda (stamp pcm) (declare (ignore stamp)) (incf total (* 4 (length (pcm-data pcm)))))
             *pcm-table*)
    total))

(defun sample-path (stamp)
  (merge-pathnames (format nil "~a.f32" stamp) *sample-directory*))

(defun store-pcm! (octets channels)
  "OCTETS, the samples of CHANNELS channels, kept: (values stamp frames)."
  (unless (member channels '(1 2))
    (error "A sample has one or two channels, not ~s." channels))
  (let ((frames (floor (length octets) (* 4 channels))))
    (unless (and (plusp frames) (= (length octets) (* 4 channels frames)))
      (error "Those ~d bytes are not whole frames of ~d channels." (length octets) channels))
    (let ((stamp (format nil "~d-~a" channels (content-stamp octets))))
      (bt:with-lock-held (*pcm-lock*)
        (unless (gethash stamp *pcm-table*)
          (when (> (+ (held-sample-bytes) (length octets)) *most-sample-bytes*)
            (error "This server's room for recorded sound is full, so the sample was not kept.  Use shorter samples, or ask whoever runs it to make room."))
          (setf (gethash stamp *pcm-table*) (make-pcm channels frames (octets-floats octets)))
          (when *sample-directory*
            (with-open-file (out (sample-path stamp) :direction :output :if-exists :supersede
                                                     :element-type '(unsigned-byte 8))
              (write-sequence octets out)))))
      (values stamp frames))))

(defun find-pcm (stamp)
  "The samples called STAMP, from memory or the directory; nil if gone."
  (bt:with-lock-held (*pcm-lock*)
    (or (gethash stamp *pcm-table*)
        (when (and *sample-directory* (probe-file (sample-path stamp)))
          (with-open-file (in (sample-path stamp) :element-type '(unsigned-byte 8))
            (let* ((octets (make-array (file-length in) :element-type '(unsigned-byte 8)))
                   (channels (parse-integer stamp :end (position #\- stamp))))
              (read-sequence octets in)
              (setf (gethash stamp *pcm-table*)
                    (make-pcm channels (floor (length octets) (* 4 channels))
                              (octets-floats octets)))))))))

;;; The pool: a song's samples end to end.

(defun pool-layout (samples)
  "Where each of SAMPLES, specs, sits in their pool: (values alist of
(id left right frames) total-length).  A sample whose audio is gone has
no frames, so it is silence and not an error."
  (let ((at 0) (rows '()))
    (dolist (sample samples)
      (let ((pcm (find-pcm (getf sample :stamp))))
        (if pcm
            (let ((frames (pcm-frames pcm)))
              (push (list (getf sample :id) at
                          (if (= 2 (pcm-channels pcm)) (+ at frames) at)
                          frames)
                    rows)
              (incf at (length (pcm-data pcm))))
            (push (list (getf sample :id) 0 0 0) rows))))
    (values (nreverse rows) at)))

(defvar *pools* nil "The last few pools, (key . vector), newest first.")

(defparameter *most-pools* 4 "How many pools are kept.")

(defun pool-key (samples)
  (format nil "~{~a~^ ~}" (mapcar (lambda (s) (getf s :stamp)) samples)))

(defun pool-for (samples)
  "The pool of SAMPLES.  An edit that leaves the samples alone costs a
look-up, not a copy."
  (let ((key (pool-key samples)))
    (bt:with-lock-held (*pcm-lock*)
      (let ((kept (assoc key *pools* :test #'string=)))
        (when kept (return-from pool-for (cdr kept)))))
    (multiple-value-bind (rows total) (pool-layout samples)
      (declare (ignore rows))
      (let ((pool (make-array (max 1 total) :element-type 'single-float :initial-element 0f0))
            (at 0))
        (dolist (sample samples)
          (let ((pcm (find-pcm (getf sample :stamp))))
            (when pcm
              (replace pool (pcm-data pcm) :start1 at)
              (incf at (length (pcm-data pcm))))))
        (bt:with-lock-held (*pcm-lock*)
          (push (cons key pool) *pools*)
          (when (> (length *pools*) *most-pools*)
            (setf *pools* (subseq *pools* 0 *most-pools*))))
        pool))))

(defparameter *phrase-gap* 0.15d0
  "A silence this many seconds long separates two phrases.")

(defun pcm-phrases (pcm sr)
  "Where the sound in PCM is, as ((start end)...) in seconds: the
places to cut speech.  Loudness in 20 ms steps against the loudest."
  (let* ((data (pcm-data pcm))
         (frames (pcm-frames pcm))
         (step (round (* sr 0.02d0)))
         (steps (ceiling frames step))
         (levels (make-array steps :element-type 'double-float :initial-element 0d0)))
    (dotimes (i steps)
      (let ((sum 0d0)
            (end (min frames (* (1+ i) step))))
        (loop for k from (* i step) below end
              do (let ((x (float (aref data k) 1d0)))
                   (incf sum (* x x))))
        (setf (aref levels i) (sqrt (/ sum (max 1 (- end (* i step))))))))
    ;; sound is what stands clearly above the recording's own floor (its
    ;; quietest tenth), and not merely above silence: a phone in a noisy
    ;; room never reaches silence
    (let* ((floor (let ((sorted (sort (copy-seq levels) #'<)))
                    (if (plusp steps) (aref sorted (floor steps 10)) 0d0)))
           (threshold (max (* 3 floor) (* 0.01d0 (reduce #'max levels :initial-value 0d0))))
           (gap (ceiling (/ *phrase-gap* 0.02d0)))
           (phrases '())
           (start nil)
           (last-loud nil))
      (when (plusp threshold)
        (dotimes (i steps)
          (when (> (aref levels i) threshold)
            (when (and start (> (- i last-loud) gap))
              (push (list start (1+ last-loud)) phrases)
              (setf start nil))
            (unless start (setf start i))
            (setf last-loud i)))
        (when start (push (list start (1+ last-loud)) phrases)))
      (mapcar (lambda (p)
                (list (/ (round (* (first p) 2)) 100d0)
                      (/ (round (* (min (second p) steps) 2)) 100d0)))
              (nreverse phrases)))))
