# Tala

A digital audio workstation in Common Lisp and the browser, with a
prompt tile from the first commit.  A *tala* is a rhythmic cycle.

Tracks, clips, a piano roll, automation, device chains, returns,
sidechain ducking, imported audio, a clip-launch grid of scenes and a
mixer, laid out as anyone who has used a modern DAW will expect.  In the prompt tile a language model works the same
controls: ask for a style, a sound or an explanation, and it reads what
each control does, sets them, checks the result and points at what it
talked about.  There are no presets.  A sound is a set of values, and
what a style calls for is worked out each time.

## Running it

In a Gendl image:

```lisp
(load "/projects/gw/tala/dev.lisp")
(tala:configure-agent! :messages-url "http://transporter-room/llm/messages")
```

then browse `/tala/` on that image's HTTP port.  The audio engine is an
AudioWorklet, which browsers offer only in a secure context, so use
`localhost` or `https`.

Space plays, F9 records what the armed tracks are played (an armed
audio track records its input, picked on its device panel), Tab switches
between the arrangement and the session, M turns
the computer's keys into a keyboard, and a MIDI keyboard is picked up if
the browser offers one.  Double-click a lane for a clip and a clip for
its notes.  Drop an audio file on a track, or use Import audio, for
spoken word or anything else the browser can read.  Point at any control
and the Info box says what it does.

## How it is built

**A song is one s-expression**, its spec: the file, what undo goes back
to, what the page and the model are shown.  Worked out from it is a
Gendl `define-object` tree of song, tracks, devices and clips, whose
computed slots are the plan, the parameters and the score, each made
only when something reads it (`source/song.lisp`).

**Plan and parameters.**  The plan is the song's structure alone: which
tracks, which instruments, which devices in what order, what ducks
what.  From the plan comes one program.  Everything else, every knob,
note and automation point, is data that program reads.  A knob never
rebuilds anything; the tests count builds to hold it to that
(`source/engine.lisp`).

**One program, two runtimes.**  The DSP is written once, in a subset
that is both Lisp and Parenscript (`source/dual.lisp`).  The plan's
program goes through Parenscript to become the AudioWorklet the browser
plays, and through `compile` to become the renderer the server bounces
with.  A JavaScript number and a double-float are the same IEEE double,
and everything is built from `+ - * /` and `floor`, which IEEE defines
exactly (`source/math.lisp` has its own sine, tangent and exponential
for that reason).  So the two agree **to the last bit**, not to a
tolerance: `tests/browser.sh` compares every sample.  Nothing allocates
on the audio thread, and Lisp is never on it.

**Recorded sound is data too.**  The browser decodes a file once, at
the engine's rate, and sends the samples to the server, which keeps them
by what they are (`source/samples.lisp`).  Those numbers are then the
sound on both sides: the page fetches them back as one pool the program
reads, and a clip is a row of the score, so importing, moving or
retuning audio rebuilds nothing and the two runtimes still agree to the
last bit.  A song names its samples, so its file stays small and does
not carry the audio.

**Recordings can be repaired**, as a phone's voice memo usually needs:
spectral noise reduction learned from the gaps between phrases, mains
hum taken out at 50 or 60 Hz and its harmonics, silence trimmed, the
level normalized (`source/repair.lisp`).  Each repair makes new sound for
the sample offline, on the server; the song points at it, so undo is the
song's own and both runtimes still read the same numbers.

**The session keeps its own time.**  Launching a clip is a performance,
not an edit: it goes straight to the audio thread, which starts the clip
on the next boundary of the launch quantize by a clock that never jumps
back with the arrangement's loop.  Session clips are rows of the score
like any other, so the grid, too, rebuilds nothing.  The Lisp renderer
and the worklet both take a script of launches at given frames, which is
how a performance is held to the same frames on both sides.

**Undo takes back one edit, not the song.**  The person and the
prompt's model can edit at once, so an undo entry records the edit, the
song before and after it, and undoing it is a three-way merge that takes
back that edit's own changes and leaves everything done since.  A prompt
is one entry of its own edits only.  Versions share whatever they have
in common, so an entry costs only the path to what it changed, and two
hundred of them cost about one song.

**One door.**  The page drives the song by sending forms of the Lisp API
(`source/api.lisp`, `docs/api.md`), so nothing can be done with the
mouse that a program or the model cannot do.  What the model may run is
held to those operations with literal arguments by Gendl's `sexp-agent`.

**The model cannot hear, so it is shown.**  `(hear)` renders the song
with the program the page plays and answers a spectrogram with measures
in words: peak, RMS, clipping, stereo correlation, RMS by bar and the
balance of five bands (`source/hear.lisp`).  It goes back to the model
with its next turn, so it checks what it made.  `(point-at)` and
`(show)` reach the page, so it can teach.

**The controls are their own documentation.**  Each parameter carries
its range and what it does, in words.  That one table feeds the knobs,
the Info box and the model's reference, so a control is explained once
(`source/devices.lisp`).

## Devices

Synth (two oscillators with FM, sub, noise, a resonant 12 or 24 dB
filter, two envelopes, an LFO, glide), Drums (eight synthesized pads),
Audio (a track of recorded clips), Auto Filter, EQ Three, Saturator, Compressor with sidechain, Delay,
Chorus, Phaser, Reverb, Utility and Limiter.

## Tests

- `(load "tests/run.lisp")`, then `(tala::run-tests)`: the Lisp half, in
  a Gendl room and in CI on cold SBCL and CCL images.  A golden checksum
  holds both implementations to the same frames; it moves only in a
  commit that means to change the sound, by `(tala::record-golden)`.
- `tests/browser.sh`: the worklet against the server's frames in
  headless Chromium, then the page driven as a person drives it.
- `tests/parity.js BASE SESSION [FRAMES]`: the same comparison in Node,
  with no browser, and the engine's speed against real time.

Parity proves the two programs agree, not that they are right: a bug in
the shared source is in both.  The checks on what each device does are
the other half.

## Not built yet

Recording into a session slot (takes go into the arrangement); takes
stacked as comps; follow actions; tempo
per scene; stretching audio to the tempo; samples on drum
pads; saved songs that carry their audio; tempo changes along the song;
plugin hosting.

## License

AGPL-3.0-or-later; see LICENSE.
