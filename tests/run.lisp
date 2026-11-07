;;;; -*- Mode: Lisp; Package: tala -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;; The Lisp half of Tala's regression suite.  (load "tests/run.lisp"),
;;;; then (tala::run-tests): a list of the checks that failed and their
;;;; count.  tests/browser.sh is the other half: the worklet against
;;;; this renderer, sample for sample.

(in-package :tala)

(defvar *checks* nil)

(defmacro check (name form)
  "FORM must be true; NAME says what it proves."
  `(let ((value (handler-case ,form
                  (serious-condition (e)
                    (let ((*print-readably* nil))
                      (format nil "signaled: ~a" e))))))
     (push (cons ,name (and value (not (stringp value)))) *checks*)
     (when (stringp value)
       (format t "~&~a: ~a~%" ,name value))))

(defun forms (song text)
  "TEXT's forms run on SONG: the results, every one, passed or not."
  (let ((*song* song))
    (with-double-floats
      (with-audio-floats
        (sexp-agent:run-forms *agent* text :stop-on-error nil)))))

(defun forms-pass? (song text)
  (let ((results (forms song text)))
    (or (every (lambda (r) (getf r :ok)) results)
        (error "~{~a~^; ~}" (mapcar (lambda (r) (getf r :error))
                                    (remove-if (lambda (r) (getf r :ok)) results))))))

(defun form-error (song text)
  "The error of the one form in TEXT."
  (getf (first (forms song text)) :error))

(defun sample-song ()
  "A song that reaches every device."
  (let ((song (make-song)))
    (forms-pass? song
"(set-tempo 145)
(add-track \"Drums\" :instrument :drums)
(add-clip \"Drums\" :start 0 :length 16 :loop 4)
(add-steps \"Drums\" nil 36 \"x...x...x...x...\")
(add-steps \"Drums\" nil 39 \"..x...x...x...x.\")
(add-track \"Bass\" :values (:cutoff 300 :resonance 0.5 :filter-env 36 :filter-decay 0.08 :sustain 0.5 :voices 1))
(add-clip \"Bass\" :start 0 :length 16 :loop 1 :notes ((\"E1\" 0.25 0.2) (\"E1\" 0.5 0.2) (\"E1\" 0.75 0.2)))
(add-device \"Bass\" :compressor :threshold -30 :ratio 8 :release 120)
(set-sidechain \"Bass\" :compressor \"Drums\")
(add-return \"Space\")
(add-device \"Space\" :reverb :mix 1 :decay 3)
(add-track \"Lead\" :values (:osc2-level 0.7 :osc2-detune 12 :cutoff 1500 :resonance 0.4 :fm 0.1 :noise-level 0.05 :lfo-pitch 0.2))
(add-device \"Lead\" :delay :mix 0.3)
(add-device \"Lead\" :chorus)
(add-device \"Lead\" :phaser)
(add-device \"Lead\" :eq :low -12)
(add-device \"Lead\" :drive :amount 4)
(add-device \"Lead\" :filter :cutoff 8000)
(add-device \"Lead\" :utility :width 1.2)
(add-clip \"Lead\" :start 0 :length 16 :notes ((\"E4\" 0 1) (\"G4\" 1 1) (\"B4\" 2 2) (\"E5\" 2 2)))
(set-track \"Lead\" :sends (-12))
(set-automation \"Lead\" :instrument :cutoff ((0 400) (16 6000)))
(set-automation \"Lead\" nil :gain ((0 -6) (8 0)))")
    song))

(defun peak-of (buffer)
  (reduce #'max buffer :key #'abs :initial-value 0d0))

(defun checksum (left right)
  "A number for the frames, the same on every implementation: the bits
of each sample as a float32, mixed."
  (let ((h 2166136261))
    (flet ((mix (buffer)
             (loop for x across buffer
                   do (setf h (logand #xffffffff
                                      (* 16777619 (logxor h (logand #xffffffff (single-bits x)))))))))
      (mix left)
      (mix right))
    h))

(defun test-arithmetic ()
  "Every function of math.lisp against the Lisp it stands for."
  (flet ((near? (a b tolerance) (<= (abs (- a b)) tolerance)))
    (check "sine"
      (loop for i from -200 to 200
            for x = (* i 0.0137d0)
            always (near? (d-sin x) (sin (* 2 pi x)) 1d-7)))
    (check "tangent"
      (loop for x from 0.001d0 to 0.48d0 by 0.01d0
            always (near? (/ (d-tan x) (tan (* pi x))) 1d0 1d-6)))
    (check "powers of two"
      (loop for x from -20d0 to 20d0 by 0.37d0
            always (near? (/ (d-exp2 x) (expt 2d0 x)) 1d0 1d-6)))
    (check "logarithm"
      (loop for x in '(0.001d0 0.1d0 0.5d0 1d0 3d0 1000d0)
            always (near? (d-log2 x) (log x 2d0) 1d-5)))
    (check "decibels both ways"
      (near? (gain-db (db-gain -12d0)) -12d0 1d-4))
    (check "pitch" (near? (pitch-hz 69d0) 440d0 1d-6))
    (check "soft clip holds the limit"
      (and (near? (d-soft 3d0) 1d0 1d-9) (near? (d-soft 100d0) 1d0 1d-9) (near? (d-soft 0d0) 0d0 1d-12)))
    (check "noise stays in range and moves"
      (let ((s (double-buffer 1)))
        (setf (aref s 0) 12345d0)
        (let ((values (loop repeat 2000 collect (next-noise s 0))))
          (and (every (lambda (x) (<= -1d0 x 1d0)) values)
               (> (length (remove-duplicates values)) 1900)))))
    (check "right-nested arithmetic is flattened, as Parenscript prints it"
      (equal (normalize-dsp '(* a (* b c) (+ d (+ e f))))
             '(* a b c (+ d e f))))))

(defun test-model ()
  (let ((song (make-song)))
    (check "an empty song has only the master" (= 1 (length (the-object song plan))))
    (check "add-track" (forms-pass? song "(add-track \"Bass\" :values (:cutoff 800))"))
    (check "a control equal to its default is not stored"
      (progn
        (forms-pass? song "(set-device \"Bass\" :instrument :resonance 0.2)")
        (null (getf (getf (first (getf (the-object song spec) :tracks)) :instrument) :values))
        t))
    (check "a value past the range is held at the end"
      (progn
        (forms-pass? song "(set-device 0 :instrument :cutoff 99999)")
        (= 20000 (device-value (getf (first (getf (the-object song spec) :tracks)) :instrument) :cutoff))))
    (check "an unknown control says what the controls are"
      (search ":cutoff" (form-error song "(set-device 0 :instrument :cutof 3)")))
    (check "an unknown track says what the tracks are"
      (search "Bass" (form-error song "(set-track \"Bas\" :gain 0)")))
    (check "an unknown device kind says what the kinds are"
      (search ":delay" (form-error song "(add-device 0 :dealy)")))
    (check "a bad choice says what the choices are"
      (search ":saw" (form-error song "(set-device 0 :instrument :osc1-wave :sawtooth)")))
    (check "note names, C3 = 60"
      (and (= 60 (pitch-number "C3")) (= 40 (pitch-number "E1")) (= 66 (pitch-number "F#3"))
           (= 36 (pitch-number "C1")) (= 58 (pitch-number "Bb2"))))
    (check "a clip repeats within its length"
      (progn
        (forms-pass? song "(add-clip \"Bass\" :start 4 :length 8 :loop 2 :notes ((40 0 1) (41 1 1)))")
        (equal (mapcar #'first (the-object (the-object song (tracks 0)) events))
               '(4 5 6 7 8 9 10 11))))
    (check "a note is cut at its clip's end"
      (let ((song (make-song)))
        (forms-pass? song "(add-track \"A\") (add-clip \"A\" :length 2 :notes ((60 1 4)))")
        (equal (first (the-object (the-object song (tracks 0)) events)) '(1 1 60 100))))
    (check "step patterns"
      (let ((song (make-song)))
        (forms-pass? song "(add-track \"A\") (add-clip \"A\" :length 4) (add-steps \"A\" nil 36 \"x..Xx\")")
        (let ((notes (getf (first (getf (first (getf (the-object song spec) :tracks)) :clips)) :notes)))
          (and (= 3 (length notes)) (= 0.75 (second (second notes))) (= 127 (fourth (second notes)))))))
    (check "a return gives every track a send"
      (let ((song (make-song)))
        (forms-pass? song "(add-track \"A\") (add-return \"R\") (add-track \"B\") (add-return \"S\")")
        (and (equal '(-60 -60) (getf (first (getf (the-object song spec) :tracks)) :sends))
             (equal '(-60 -60) (getf (second (getf (the-object song spec) :tracks)) :sends)))))
    (check "deleting a return drops its send"
      (let ((song (make-song)))
        (forms-pass? song "(add-track \"A\") (add-return \"R\") (add-return \"S\") (set-track \"A\" :sends (-1 -2)) (delete-track \"R\")")
        (equal '(-2) (getf (first (getf (the-object song spec) :tracks)) :sends))))
    (check "a sidechain key follows its track, and goes with it"
      (let ((song (make-song)))
        (forms-pass? song "(add-track \"K\") (add-track \"B\") (add-device \"B\" :compressor) (set-sidechain \"B\" 0 \"K\") (move-track \"K\" 1)")
        (let* ((spec (the-object song spec))
               (compressor (first (getf (first (getf spec :tracks)) :devices))))
          (and (= 1 (getf compressor :key))
               (progn (forms-pass? song "(delete-track \"K\")")
                      (null (getf (first (getf (first (getf (the-object song spec) :tracks)) :devices)) :key)))))))
    (check "automation, by device and by the track's fader"
      (let ((song (make-song)))
        (forms-pass? song "(add-track \"A\") (add-device \"A\" :filter) (set-automation \"A\" :filter :cutoff ((8 100) (0 200))) (set-automation \"A\" nil :pan ((0 -1)))")
        (let ((lanes (getf (first (getf (the-object song spec) :tracks)) :automation)))
          (and (= 2 (length lanes)) (equal '((0 200) (8 100)) (getf (first lanes) :points))))))
    (check "undo and redo"
      (let ((song (make-song)))
        (forms-pass? song "(set-tempo 100) (set-tempo 110)")
        (forms-pass? song "(undo)")
        (and (= 100 (the-object song tempo))
             (progn (forms-pass? song "(redo)") (= 110 (the-object song tempo))))))
    (check "a failed edit leaves the song and the undo list as they were"
      (let ((song (make-song)))
        (forms-pass? song "(add-track \"A\")")
        (let ((before (the-object song spec))
              (depth (length (the-object song undo-entries))))
          (forms song "(add-device \"A\" :delay :nonsense 1)")
          (and (eq before (the-object song spec))
               (= depth (length (the-object song undo-entries)))))))
    (check "a song survives its file"
      (let* ((song (sample-song))
             (text (song-text song))
             (spec (read-song-text text)))
        (equal spec (the-object song spec))))
    (check "the file's reader runs nothing"
      (handler-case (progn (read-song-text "#.(error \"ran\")") nil)
        (error (e) (not (search "ran" (princ-to-string e))))))))

(defun test-sandbox ()
  (let ((song (make-song)))
    (check "no form outside the operations"
      (search "not an operation" (form-error song "(delete-file \"/etc/passwd\")")))
    (check "no variables in the arguments"
      (search "literal" (form-error song "(set-tempo *print-base*)")))
    (check "no read-time evaluation"
      (not (getf (first (forms song "(set-tempo #.(+ 100 20))")) :ok)))))

(defun test-plan-and-parameters ()
  (clrhash *lisp-engines*)
  (clrhash *js-engines*)
  (let* ((song (sample-song))
         (plan (the-object song plan)))
    (render-song song 4800)
    (worklet-js plan *sample-rate*)
    (let ((builds (copy-list *builds*)))
      (forms-pass? song "(set-device \"Bass\" :instrument :cutoff 500)
(set-tempo 150)
(set-track \"Lead\" :gain -3 :mute t)
(add-notes \"Bass\" nil ((\"G1\" 0.5 0.1)))
(set-automation \"Lead\" :delay :feedback ((0 0.2) (8 0.8)))
(add-clip \"Lead\" :start 16 :length 4)
(set-device \"Lead\" :delay :on nil)
(set-device \"Bass\" :instrument :osc1-wave :square)")
      (render-song song 4800)
      (worklet-js (the-object song plan) *sample-rate*)
      (check "knobs, notes, clips and automation leave the plan alone"
        (equal plan (the-object song plan)))
      (check "and rebuild neither program" (equal builds *builds*))
      (forms-pass? song "(add-device \"Bass\" :delay)")
      (render-song song 4800)
      (check "a new device is a new plan and a new program"
        (= (getf *builds* :lisp) (1+ (getf builds :lisp))))
      (forms-pass? song "(delete-device \"Bass\" :delay)")
      (render-song song 4800)
      (check "a structure the song had before costs nothing"
        (= (getf *builds* :lisp) (1+ (getf builds :lisp)))))))

(defun test-render ()
  (let* ((song (sample-song))
         (frames 48000))
    (multiple-value-bind (left right) (render-song song frames)
      (check "the sample song sounds" (> (peak-of left) 0.1d0))
      (check "and stays finite"
        (every (lambda (x) (and (= x x) (< (abs x) 4d0))) left))
      (check "the limiter holds the ceiling"
        (< (peak-of right) (* 1.0001d0 (expt 10 (/ -0.3d0 20)))))
      (multiple-value-bind (l2 r2) (render-song song frames :block-size 128)
        (check "the frames do not depend on the block size"
          (and (equalp left l2) (equalp right r2))))
      (multiple-value-bind (l3 r3) (render-song song frames)
        (check "a render is the same every time"
          (and (equalp left l3) (equalp right r3)))))
    (check "the worklet module has nothing that would stop it"
      (let ((js (worklet-js (the-object song plan) *sample-rate*)))
        (and (not (search "__PS_MV_REG" js))
             (not (search "function ()" js))
             (search "registerProcessor" js))))
    (check "a sidechain ducks"
      (let* ((quiet (sample-song))
             (loud (sample-song)))
        (forms-pass? loud "(set-sidechain \"Bass\" :compressor nil) (set-device \"Bass\" :compressor :threshold 0 :ratio 1)")
        (flet ((rms (song)
                 (let ((left (render-song (make-song (solo-spec (the-object song spec) "Bass")) 48000)))
                   (sqrt (/ (reduce #'+ (map 'vector (lambda (x) (* x x)) left)) 48000)))))
          ;; soloing the bass silences the kick's fader, not its bus
          (< (rms quiet) (rms loud)))))
    (check "muting a track takes it out"
      (let ((silent (make-song)))
        (forms-pass? silent "(add-track \"A\") (add-clip \"A\" :length 4 :notes ((60 0 2))) (set-track \"A\" :mute t)")
        (< (peak-of (render-song silent 24000)) 1d-6)))
    (check "automation moves the sound"
      (flet ((level (spec-forms)
               (let ((song (make-song)))
                 (forms-pass? song "(add-track \"A\") (add-clip \"A\" :length 16 :notes ((60 0 16)))")
                 (forms-pass? song spec-forms)
                 (peak-of (subseq (render-song song 96000) 72000)))))
        (< (level "(set-automation \"A\" nil :gain ((0 0) (2 -60)))")
           (* 0.1 (level "(set-track \"A\" :gain 0)")))))
    (check "a loop plays its first note again"
      (let ((song (make-song)))
        (forms-pass? song "(set-tempo 120) (set-loop 0 1) (add-track \"A\" :instrument :drums)
(add-clip \"A\" :length 1 :notes ((36 0 0.1)))
(set-device \"A\" :drums :pad1-decay 0.01 :pad1-sweep 0)")
        (let ((left (render-song song 48000)))
          ;; one beat is 24000 frames at this tempo: the second hit at 24000
          (and (> (peak-of (subseq left 0 2000)) 0.05d0)
               (< (peak-of (subseq left 12000 23000)) 0.001d0)
               (> (peak-of (subseq left 24000 26000)) 0.05d0)))))
    (flet ((tail (forms)
             ;; a short note into the effect: the RMS of each second after it,
             ;; and the mean of the last
             (let ((song (make-song)))
               (forms-pass? song "(add-return \"R\") (add-track \"A\") (add-clip \"A\" :length 1 :notes ((64 0 0.25))) (set-track \"A\" :sends (-6))")
               (forms-pass? song forms)
               (let ((left (render-song song (* 4 48000))))
                 (values (loop for i from 0 below 4
                               collect (let ((part (subseq left (* i 48000) (* (1+ i) 48000))))
                                         (sqrt (/ (reduce #'+ (map 'vector (lambda (x) (* x x)) part)) 48000))))
                         (abs (/ (reduce #'+ (subseq left (* 3 48000))) 48000)))))))
      ;; parity cannot find these: both programs would be wrong alike
      (dolist (device '("(add-device \"R\" :reverb :mix 1 :decay 1)"
                        "(add-device \"R\" :delay :mix 1 :feedback 0.3)"
                        "(add-device \"R\" :chorus :mix 1 :feedback 0.5)"
                        "(add-device \"R\" :phaser :mix 1)"))
        (multiple-value-bind (seconds mean) (tail device)
          (check (format nil "a tail dies away and leaves no offset: ~a" device)
            (and (> (first seconds) 0.001d0)
                 (< (fourth seconds) (* 0.01d0 (first seconds)))
                 (< mean 0.0001d0))))))
    (check "a slot cannot be called size"
      (handler-case (progn (macroexpand-1 '(define-slots xx size)) nil)
        (error () t)))
    (check "a WAV's header and length"
      (let ((wav (wav-octets (double-vector '(0d0 1d0 -1d0)) (double-vector '(0d0 0d0 0d0)) 48000)))
        (and (= (length wav) (+ 44 12)) (= (aref wav 0) 82)
             (= (aref wav 48) 255) (= (aref wav 49) 127))))))

(defun test-hearing ()
  (let* ((song (sample-song))
         (picture (let ((*song* song))
                    (with-audio-floats (tala-api:hear :bars 2)))))
    (check "hear makes a picture" (and (picture-p picture) (> (length (picture-png picture)) 1000)))
    (check "its caption gives the scale and the measures"
      (let ((note (picture-note picture)))
        (and (search "bars 1 to 2" note) (search "RMS" note) (search "sub (under 60 Hz)" note))))
    (check "a track can be heard alone"
      (search "track Bass alone"
              (picture-note (let ((*song* song))
                              (with-audio-floats (tala-api:hear :bars 1 :track "Bass"))))))
    (check "the fast Fourier transform finds a tone"
      (let* ((n 1024)
             (re (double-buffer n))
             (im (double-buffer n)))
        (dotimes (i n) (setf (aref re i) (sin (/ (* 2 pi 64 i) n))))
        (fft! re im)
        (= 64 (loop with best = 0 with most = 0d0
                    for k from 0 below 512
                    for power = (+ (expt (aref re k) 2) (expt (aref im k) 2))
                    do (when (> power most) (setf most power best k))
                    finally (return best)))))))

(defun test-json ()
  (check "a double reaches the page exactly"
    (let ((text (json-text (list 0.1d0 1d-7 145 (/ 1d0 3d0)))))
      (equal (coerce (yason:parse text) 'list)
             (list 0.1d0 1d-7 145 (/ 1d0 3d0)))))
  (check "strings are escaped"
    (equal (gethash "a\"b" (yason:parse (json-text (obj "a\"b" (format nil "c~%d")))))
           (format nil "c~%d")))
  (check "the song's state is JSON"
    (hash-table-p (yason:parse (json-text (song-json (sample-song)))))))

(defun test-reference ()
  (let ((reference (model-reference)))
    (check "every operation is in the reference"
      (let ((missing (remove-if (lambda (op)
                                  (search (format nil "(~(~a~)" op) reference))
                                (operators-for-prompt))))
        (or (null missing)
            (format nil "not documented: ~{~(~a~)~^ ~}" missing))))
    (check "every control is in the reference, with what it does"
      (every (lambda (kind)
               (or (eq (device-kind-key kind) :drums)
                   (every (lambda (p)
                            (and (search (format nil "`:~(~a~)`" (param-key p)) reference)
                                 (plusp (length (param-doc p)))))
                          (device-params (device-kind-key kind)))))
             *device-kinds*))
    (check "a pad's controls are said once"
      (and (search ":pad1-tune" reference) (search "`:sweep-time`" reference)))))

(defun call-with-stub-model (replies fn)
  "FN with *AGENT* answering REPLIES, texts, in turn: the requests it
had go to a function FN is called with."
  (let* ((requests '())
         (saved (fdefinition 'sexp-agent::call)))
    (setf (fdefinition 'sexp-agent::call)
          (lambda (agent messages &key session-id)
            (declare (ignore agent session-id))
            (push messages requests)
            (let ((table (make-hash-table :test 'equal))
                  (block (make-hash-table :test 'equal)))
              (setf (gethash "type" block) "text"
                    (gethash "text" block) (or (pop replies) (format nil "```lisp~%```"))
                    (gethash "content" table) (list block)
                    (gethash "stop_reason" table) "end_turn")
              table)))
    (unwind-protect (funcall fn (lambda () (reverse requests)))
      (setf (fdefinition 'sexp-agent::call) saved))))

(defun test-prompt ()
  (let* ((session (make-session "test" (make-song)))
         (song (session-song session)))
    (call-with-stub-model
     (list (format nil "Setting the tempo and a bass.~%```lisp~%(set-tempo 145)~%(add-track \"Bass\")~%(add-clip \"Bass\" :length 4 :notes ((40 0 1)))~%(hear :bars 1)~%```")
           (format nil "That sounds right.~%```lisp~%(point-at \"device:Bass:Synth:cutoff\" \"This is the brightness.\")~%(show :devices \"Bass\")~%```"))
     (lambda (requests)
       (let ((result (prompt-song session "Set up a bass at 145")))
         (check "the forms ran" (= 145 (the-object song tempo)))
         (check "the model heard it, and went on"
           (and (= 1 (length (getf result :heard))) (= 2 (length (funcall requests)))))
         (check "the page is told what to light up"
           (= 2 (length (getf result :ui))))
         (check "the reply is both rounds'"
           (and (search "Setting" (getf result :reply)) (search "right" (getf result :reply))))
         (check "the song in words goes with the request"
           (let ((first-message (first (first (funcall requests)))))
             (search "Song" (let ((content (gethash "content" first-message)))
                              (if (stringp content)
                                  content
                                  (gethash "text" (first content))))))))))
    (check "a prompt is one undo"
      (progn
        (forms-pass? song "(undo)")
        (and (= 120 (the-object song tempo)) (null (getf (the-object song spec) :tracks)))))
    (call-with-stub-model
     (list (format nil "Trying.~%```lisp~%(set-tempo 133)~%(add-device \"Nope\" :delay)~%```")
           (format nil "Done.~%```lisp~%```"))
     (lambda (requests)
       (prompt-song session "Go on")
       (check "a failure goes back to the model once"
         (= 2 (length (funcall requests))))
       (check "what ran before it stands"
         (= 133 (the-object song tempo)))))
    (check "the operators the model is admitted are the operations"
      (and (member 'tala-api::hear (sexp-agent:agent-operators *agent*))
           (not (member 'delete-file (sexp-agent:agent-operators *agent*)))))))

(defparameter *golden-checksum* nil
  "The sample song's first second, recorded when the sound means to
change: (record-golden).")

(defparameter *golden-file*
  (merge-pathnames "tests/golden.sexp" (asdf:system-source-directory :tala)))

(defun golden-now ()
  (multiple-value-bind (left right) (render-song (sample-song) 48000)
    (checksum left right)))

(defun record-golden ()
  "Move the golden number: only in a commit that means to change the sound."
  (with-open-file (out *golden-file* :direction :output :if-exists :supersede)
    (format out ";; The sample song's first second; (tala::record-golden) moves it.~%~d~%" (golden-now)))
  (golden-now))

(defun test-golden ()
  (check "the sample song's frames are the recorded ones, on this implementation too"
    (let ((recorded (with-open-file (in *golden-file*) (read in)))
          (now (golden-now)))
      (or (= recorded now)
          (format nil "recorded ~d, now ~d" recorded now)))))

(defun run-tests ()
  "Every check: (values failed-names count)."
  (setf *checks* nil)
  (test-arithmetic)
  (test-model)
  (test-sandbox)
  (test-plan-and-parameters)
  (test-render)
  (test-hearing)
  (test-json)
  (test-reference)
  (test-prompt)
  (test-golden)
  (let ((failed (remove-if #'cdr *checks*)))
    (values (reverse (mapcar #'car failed)) (length *checks*))))
