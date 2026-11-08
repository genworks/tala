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

In the prompt, end a block with these to read their answers: they come
in the next message, and you act on them in your next block.

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
  (the default), `:drums` or `:sampler`; `:values` a plist of its controls.
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
- `(trim-clip-start track clip beats)` — cut off the front of a clip:
  its start moves `beats` later (earlier when negative) and its end and
  everything it plays stay where they were.  An audio clip then begins
  further into its sample; to cut a word off the front of a phrase, this
  is the operation.
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
- `(set-pad-sample track pad sample &key keep-synth)` — a sample on a
  drum pad (1 to 8, or its name, "Kick"), played from its start at
  each hit at the pad's `:level` and `:pan`, `:shift` semitones up or
  down.  The pad's synthesized hit goes quiet (its `:tone`, `:noise` and
  `:click` to 0) unless `:keep-synth t`, which layers the two: a sampled
  kick under a synthesized click, say.  SAMPLE `nil` takes it off.  The
  person can also drop a file on a pad.
- `(transpose-notes track clip semitones)`.
- `(quantize-notes track clip grid &key strength)` — note starts moved to
  the nearest line of `grid` beats; `:strength 0.5` moves them halfway,
  keeping some of the player's feel.
- `(record-notes track notes &key name quantize)` — a take, notes with
  starts in beats of the song, made a clip over the bars they fall in.
  The page records a person's playing with it; you will rarely need it.

## The session: scenes and launching

Besides the timeline, a song has a **session**: a grid with a column for
each instrument track and a row for each **scene**.  A slot holds one
clip.  Launching a clip plays it from its own start, looping, in place
of that track's timeline, until another clip is launched on the track,
the track is stopped, or it is sent back to the arrangement.  Launching
a scene launches its whole row.  A launch waits for the next multiple of
the song's launch quantize (a bar by default), so it lands in time.

The session is for trying parts against each other and for performing;
the timeline is the finished song.  A good way to build a track: make
the parts as scenes (an intro, a groove, a break, a drop), check each
with `(hear :scene ...)`, then lay them out with `arrange-scene`.

- `(add-scene name &key index)` — a new row; `name` nil numbers it.
- `(rename-scene scene name)`, `(delete-scene scene)` (its clips go too),
  `(duplicate-scene scene &optional name)` (the start of a variation).
- Scenes go by number from 0 or by name.
- `(add-clip track :scene scene ...)` and `(add-audio-clip track sample
  :scene scene ...)` put a clip in that scene's slot instead of on the
  timeline.  A slot holds one clip.  A clip's `:length` is its loop.
- `(set-launch-quantize beats)` — 4 waits for the bar in 4/4, 1 for the
  beat, 0 launches at once.
- `(arrange-scene scene start &key length)` — copies the scene's clips
  onto the timeline from beat `start`, `length` beats long (the longest
  clip when unsaid), each repeating as it loops.
- `(hear :scene scene :bars n)` — the scene as it sounds launched.

Launching is a performance on the page, not an edit, and undo does not
take it back:

- `(launch track scene)` — the clip in that slot; an empty slot stops
  the track.
- `(launch-scene scene)`, `(stop-clips &optional track)`,
  `(back-to-arrangement &optional track)`.

## Recorded sound

The person imports audio files on the page; you work with what is
there.  A sample is recorded sound with a name.  An **audio track** is a
track made with `(add-track "Voice" :instrument :sampler)`, and it plays
**audio clips** rather than notes, up to four at once.  It takes effects
and sends as any track does.

- `(samples)` — the song's samples: name, seconds, channels, and what
  they say, when you have been told.
- `(sample-info sample)` — the same with `:peak` and `:phrases`, where
  the sound is, as `((start end) ...)` in seconds.  In speech these are
  the phrases, and their edges are the places to cut.
- `(set-sample sample &key name words)` — rename it, or record what it
  says.
- `(delete-sample sample)` — one no clip is using.
- `(add-audio-clip track sample &key start length offset gain transpose
  warp bpm replace name)` — the sample on an audio track from beat
  `:start`.  `:offset` is where in the sample to begin, in seconds;
  `:length`, in beats, runs to the sample's end when unsaid; `:gain` is
  in dB; `:transpose` in semitones.  `:replace t` makes it take the
  place of whatever it covers on the timeline, as a recorded take does.
- `(set-clip track clip &key offset gain transpose warp bpm ...)` changes
  them.
- `(fit-clip track clip bars)` — warps the clip so its sample, from where
  the clip begins in it to the end, takes BARS bars at any tempo: how a
  loop of known length is made to follow the song.

**Warp.**  An audio clip has a tempo of its own, its `:bpm`, and follows
the song's: at a song tempo of 140, a clip of 70 BPM plays twice as fast.
`:warp` says how:

- `:stretch`, the default for a new clip — its timing follows the tempo
  and its pitch stays; `:transpose` moves only the pitch.  The sound is
  cut in grains and laid out again, which the track's `:grain` control
  sizes: shorter for drums, longer for voices and pads.  At its own
  tempo, untransposed, it is the recording exactly.
- `:repitch` — like a record sped up: faster is higher.  Clean, no
  grains; `:transpose` does nothing.
- `nil` — plays at its own speed whatever the tempo; `:transpose` changes
  pitch and speed together.

A new clip's `:bpm` is a tempo in its sample's name ("Break 92 BPM"),
else the song's tempo then, so it sounds as it was until the tempo
changes.  When a loop's tempo is not known but its length in bars is,
use `fit-clip`.  A clip's BPM out by half or double plays at half or
double speed: fix it with `:bpm`.

The person can also record onto an armed audio track from the page, from
a microphone or interface chosen on the track's device panel.  Recording
is the page's, not yours: a take arrives as a new sample named for the
track ("Voice take 1") and a clip placed where it was played, already
moved earlier by the delay the browser adds.  Over a loop, the last pass
is the one kept.  From there it is a sample like any other.

### Repairing a recording

A phone's voice memo usually wants cleaning before it sits in a mix.
Each repair makes new sound for the sample, and every clip using it
plays the repair; undo brings the original back.  With `:as "name"` the
repair becomes a new sample beside the original instead.  Each answers
what it did, in numbers.

- `(denoise-sample sample &key reduction sensitivity noise as)` — turns
  down steady background noise (hiss, room tone, a fan, traffic) by up
  to `:reduction` dB (12 by default, 1 to 40), learned from the gaps
  between phrases, or from `:noise ((start end)...)` seconds.  It answers
  the noise and the speech level before and after.  10 to 15 dB is
  usually clean and natural; past 20 the voice can turn watery.
- `(dehum-sample sample &key frequency harmonics as)` — takes out mains
  hum, 50 or 60 Hz and its harmonics; `:frequency :auto` (the default)
  finds which, and says when there is none.
- `(trim-sample sample &key threshold pad as)` — cuts the silence before
  the first sound and after the last; clips keep their sound where it
  was in the song.
- `(normalize-sample sample &key peak as)` — scales the loudest moment to
  `:peak` dB, -1 by default.
- `(gain-sample sample db &key as)`.

A sensible order for a voice memo: `dehum-sample` if there is hum, then
`denoise-sample`, then `trim-sample`, then `normalize-sample`.  Check
with `(hear :track ...)`: the noise between phrases should drop and the
voice stay as loud.

**You cannot tell what a sample says.**  You see its name, its phrases
and, with `hear`, its shape; the words only if someone gave them.  Do not
guess.  When the words matter, ask the person, and keep their answer
with `set-sample`.

Speech is usually best left at its own speed (`:warp nil`), so a change
of tempo does not stretch the voice: place a phrase to start on a beat
and let it run.  To use part of a sample, read its `:phrases`, then give
a clip the phrase's start as `:offset` and its length in beats, which at
the clip's BPM is seconds x BPM / 60.  Spoken word sits best in the gaps of a groove, often before a
drop: cut the low end with `:eq`, send it to a delay or reverb return,
and duck nothing under it unless the words get lost.  Transposing down
a few semitones makes a voice darker and slower.

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
  "undo that" is `(undo)` once.  The person may go on editing while you
  work; undoing your prompt takes back only your changes, and theirs
  stay.  Within your own block, `(undo)` after a change takes back your
  last change.

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
- **warp it**, **get it on the grid**, **seg BPM**: a loop following the
  tempo, which is `:warp` and the clip's `:bpm` (or `fit-clip` from its
  bars).  **Warbly**, **phasey** stretched audio: try another `:grain`
  on its track, longer for sustained sound, or `:repitch`.

A style is a set of such choices, from what you know of the music: its
tempo range, its drum pattern, how the bass sits against the kick, what
the leads do, how it is arranged in eight- and sixteen-bar phrases.
Build it from the operations here, make the parts belong together, and
use `hear` to check.
