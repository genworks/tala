# The Tala Lisp API

Everything the page does is done by evaluating s-expressions: the page
itself sends these forms, a program can, and in the prompt tile a
language model writes them for what a person asks in words.  The
operations live in the package `tala-api`.  Each is a function of
literal arguments on the current song and answers a plist or text.

```lisp
(set-tempo 138)
(add-track "Bass" :instrument :synth :values (:cutoff 400 :resonance 0.5 :voices 1))
(add-clip "Bass" :start 0 :length 16 :loop 1 :notes (("E1" 0.25 0.2) ("E1" 0.5 0.2) ("E1" 0.75 0.2)))
(add-device "Bass" :compressor :threshold -30 :ratio 8)
(describe-song)
```

A form's arguments are literals only: numbers, strings, keywords, `t`,
`nil` and lists of those (a list may be quoted or not).

Time is in **beats** from the start of the song; a bar of 4/4 is four
beats, a sixteenth is 0.25.  Pitches are MIDI numbers or names, with
**C3 = 60** (so "E1" is 40, a low bass E, and "C2" 48).  Velocity runs 1 to 127.  Levels are
in dB.  Tracks go by name, or instrument tracks by number from 0.  A
device goes by `:instrument`, by its number in the effect chain from
0, by kind (`:delay`) or by label (`"Delay"`).  A clip goes by the id
`add-clip` answers or by name; `nil` means a track's only clip.

## How a song is built

A **track** holds an instrument (`:synth` or `:drums`), a chain of
effects, clips, and automation.  A **return track** holds effects only,
and every track has a send to it: put one reverb and one delay on
returns and send to them, rather than a copy on each track.  The
**master** track holds what all of it passes through last; a new song
has a limiter there.

A **clip** sits on a track's timeline, `:start` and `:length` in beats.
Its notes repeat every `:loop` beats, so a one-bar pattern over sixteen
bars is one clip of length 64 with `:loop 4`.  Keep notes inside the
first `:loop` beats.  A section that changes is a new clip after it.

A sound is a set of control values.  There are no presets: read what
each control does under "Devices and their controls" below and work out
the values for the sound that is wanted.

## Reading the song

- `(describe-song)` — the song in words: tempo, loop, each track with
  its instrument, effects, clips and automation, values that differ
  from the defaults.  One comes with every request.
- `(song-info)` — the same in brief, as a plist.
- `(device-kinds)` — every device with what it is for.
- `(device-controls kind)` — a device's controls: key, label, default,
  range or choices, what it does.
- `(device-values track device)` — a device's values that differ from
  the defaults.
- `(clip-notes track &optional clip)` — a clip's notes, each `(pitch
  start length velocity)`.

## Hearing it

You cannot hear the song, so look at it.

- `(hear &key from bars track)` — a spectrogram of BARS bars (8 by
  default) from bar FROM (1 by default), of the whole song or of TRACK
  alone, by name.  It is rendered by the same program the page plays,
  and comes with measures in words: peak, RMS, clipping, stereo
  correlation, RMS by bar and the balance of five frequency bands.

End a block with `(hear)` to check what you made: the forms before it
run, the picture and measures come back, and you answer again.  What to
look for:

- Silence or nothing in a band means something is not sounding.  Hear a
  track alone to find out which.
- A kick is a bright vertical stripe at the bottom on each beat; a
  rolling bass fills the gaps between them, and a bright band over the
  whole width under 100 Hz means they are fighting.
- Peak near 0 dB with many clipped frames is too hot: lower the track
  gains, not the master.  A mix peaking around -3 to -6 dB before the
  limiter has room.  RMS around -14 to -9 dB is loud for dance music.
- The sub and bass bands should lead dance music; air more than about
  20 dB under them sounds dull, and high mids near the bass sound harsh.
- Correlation near 1 is nearly mono; under 0 will vanish on mono
  speakers.  Keep kick and bass in the center.

## The song

- `(set-tempo bpm)` — 20 to 300.
- `(set-time-signature numerator denominator)`.
- `(set-loop start end &key on)` — the loop, in beats; `:on nil` plays
  straight through.
- `(rename-song name)`.
- `(clear-song)` — an empty song.  Undo brings it back.

## Tracks

- `(add-track name &key instrument values)` — `:instrument` is `:synth`
  (the default) or `:drums`; `:values` a plist of its controls.
- `(add-return name)` — a return track.  Its effects usually want
  `:mix 1`, since the track's send sets how much is heard.
- `(delete-track track)`, `(rename-track track name)`,
  `(move-track track index)`.
- `(set-track track &key gain pan mute solo sends color)` — the mixer.
  `:gain` is -60 to 12 dB, `:pan` -1 to 1, `:sends` a list of dB, one
  for each return in order, -60 being off, `:color` 0 to 11.

## Devices

- `(set-device track device &rest values)` — controls by key: `(set-device
  "Bass" :instrument :cutoff 600 :resonance 0.7)`.  A number past a
  control's range is held at the end of it.  `:on nil` bypasses the
  device.
- `(add-device track kind &rest values)` — an effect at the end of the
  chain, or at `:index n`.
- `(delete-device track device)`, `(move-device track device index)`,
  `(reset-device track device)`.
- `(set-instrument track kind &rest values)` — a new instrument for a
  track.
- `(set-sidechain track device source)` — the compressor DEVICE on
  TRACK listens to the instrument track SOURCE, so TRACK ducks when
  SOURCE plays; `nil` listens to its own track.  This is how a bass
  makes room for a kick: a low threshold, a high ratio, a release timed
  to the beat.

## Clips and notes

- `(add-clip track &key name start length loop notes)` — answers the
  clip's id.  `:notes` as below.
- `(delete-clip track clip)`, `(duplicate-clip track clip start)`.
- `(set-clip track clip &key name start length loop)`.
- `(add-notes track clip notes)` — each note `(pitch start length)` or
  `(pitch start length velocity)`, times in beats from the clip's
  start.
- `(set-notes track clip notes)` — replaces them.
- `(add-steps track clip pitch steps &key step velocity accent gate
  start)` — a step pattern, the short way to write a rhythm: `steps` is
  a string, `x` a note, `X` an accent, anything else a rest, each
  character `:step` beats long, by default 0.25.

  ```lisp
  (add-clip "Drums" :length 64 :loop 4)
  (add-steps "Drums" nil 36 "x...x...x...x...")
  (add-steps "Drums" nil 39 "..x...x...x...x.")
  ```

  The drum kit's pads are pitches 36 to 43, in the order under
  `:drums` below.
- `(transpose-notes track clip semitones)`.

## Automation

- `(set-automation track device param points)` — the control follows
  POINTS, each `(beat value)`, in straight lines, over the song's time.
  DEVICE `nil` means the track's own `:gain` or `:pan`.  A lane
  overrides the control while the song plays.
- `(clear-automation track device param)`.

```lisp
(set-automation "Lead" :instrument :cutoff ((0 300) (32 300) (64 8000)))
```

A build-up is often this: a filter opening over eight or sixteen bars,
with noise rising, then everything landing on a bar line.

## Undo

- `(undo)`, `(redo)` — one change.  A prompt's forms are one undo, so
  "undo that" is `(undo)` once.

## Teaching

People use the prompt to learn as much as to make.  When a request is a
question, or when something you did would teach them something, show
them where:

- `(point-at target &optional say)` — lights up a control with SAY in a
  note beside it, a short sentence.  Targets: `"play"`, `"loop"`,
  `"tempo"`, `"browser"`, `"view:arrangement"`, `"view:mixer"`,
  `"add-track"`, `"track:NAME"`, `"track:NAME:gain"`, `"track:NAME:mute"`,
  `"track:NAME:solo"`, `"device:TRACK:LABEL"`,
  `"device:TRACK:LABEL:control"` and `"clip:ID"`.  LABEL is a device's
  label: `"device:Bass:Synth:cutoff"`, `"device:Lead:Delay:feedback"`.
- `(show view &optional track clip)` — opens a view: `:arrangement`,
  `:mixer`, `:devices` for TRACK, or `:notes` for CLIP of TRACK (the
  piano roll).

Say what a control does to the sound and what to listen for, then point
at it.  Where it helps, set an example so they can hear the difference,
and say how to undo it.

## What producers mean

People describe sound in words that are not literal.  Read them as:

- **warm**: less top end, a little saturation, no harshness; lower a
  filter or high band, add a touch of `:drive`.
- **dark**: little above a few kHz.  **Bright**, **airy**: the opposite.
- **fat**, **thick**: detuned oscillators, a sub, chorus; more low mids.
- **punchy**: a fast transient and a short body; a compressor with a
  slow attack lets the hit through, a tight decay leaves space.
- **tight**: short decays and releases, nothing ringing on.
- **wide**: stereo difference from chorus, delay or `:width`; keep the
  low end centered.
- **muddy**: too much around 200 to 500 Hz; cut it, mostly on what is
  not kick or bass.
- **harsh**, **fizzy**: too much around 3 to 8 kHz; lower it, or lower
  the top of a saturator's `:tone`.
- **wet**, **dry**: more or less reverb and delay.
- **pumping**: the sidechain's duck, heard as the mix breathing with the
  kick.
- **squelchy**, **acid**: a resonant low-pass moved quickly by an
  envelope, a monophonic line that slides.
- **a build**, **a riser**: tension over bars, from opening filters and
  rising noise; **a drop**: everything landing together after it.

A style is a set of such choices, from what you know of the music: its
tempo range, its drum pattern, how the bass sits against the kick, what
the leads do, how it is arranged in eight- and sixteen-bar phrases.
Build it from the operations here, make the parts belong together, and
use `hear` to check.
