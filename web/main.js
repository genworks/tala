// Copyright © 2026 Genworks International
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.  Distributed WITHOUT
// ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

// Start-up, the browser panel, the file buttons, the keys, and the
// page's own test.

'use strict';

function renderBrowser() {
  const list = $('#browser-list');
  const kinds = (type) => T.kinds.filter((k) => k.type === type);
  list.replaceChildren(...[
    el('h3', {}, 'Instruments'),
    kinds('instrument').map((k) => el('button', {
      class: 'kind', 'data-title': k.label, 'data-doc': k.doc,
      onclick: () => {
        const name = uniqueName(k.label);
        T.send(`(add-track ${lit(name)} :instrument :${k.key})`);
        T.setSheet(null);
      },
    }, k.label)),
    el('button', { class: 'kind', 'data-title': 'Return track',
                   'data-doc': 'A return track: effects every track can send to.',
                   onclick: () => { addReturn(); T.setSheet(null); } }, 'Return track'),
    el('h3', {}, 'Effects'),
    kinds('effect').map((k) => el('button', {
      class: 'kind', 'data-title': k.label, 'data-doc': `${k.doc} Click to add it to the selected track.`,
      onclick: () => {
        T.detail = 'devices';
        T.send(`(add-device ${lit(selectedTrack().name)} :${k.key})`);
        T.showDetail();
      },
    }, k.label)),
  ].flat());
}

function chooseAudio() {
  const input = el('input', { type: 'file', accept: 'audio/*', multiple: true, onchange: async () => {
    for (const file of input.files) await T.importFile(file);
  } });
  input.click();
}

function renderSamples() {
  const song = T.song;
  $('#sample-list').replaceChildren(...[
    el('h3', {}, 'Samples'),
    el('button', { class: 'kind', 'data-ctl': 'import', 'data-title': 'Import audio',
                   'data-doc': 'WAV, MP3 and whatever else this browser can read. You can also drop files on a track. The audio stays with this server; a saved song only names it.',
                   onclick: chooseAudio }, '+ Import audio'),
    song.samples.map((s) => el('button', {
      class: 'kind', 'data-ctl': `sample:${s.name}`, 'data-title': s.name,
      'data-doc': `${s.seconds.toFixed(2)} s${s.words ? `: "${s.words}"` : ''}. Click to place it at the marker.${s.missing ? ' Its audio is not on this server; import it again.' : ''}`,
      onclick: async () => {
        const target = await audioTrack(null);
        T.send(form('add-audio-clip', target, s.name, kw('start'), T.startBeat));
        T.setSheet(null);
      },
    }, s.missing ? `⚠ ${s.name}` : s.name)),
  ].flat());
}

// A file dropped anywhere else starts at the marker.
document.addEventListener('dragover', (e) => e.preventDefault());
document.addEventListener('drop', (e) => {
  e.preventDefault();
  for (const file of e.dataTransfer.files) T.importFile(file);
});

function renderFileButtons() {
  const song = T.song;
  $('#file-buttons').replaceChildren(
    el('button', { 'data-title': 'New', 'data-doc': 'Clear the song. Undo brings it back.',
                   onclick: () => T.send(form('clear-song')) }, 'New'),
    el('button', { 'data-title': 'Open', 'data-doc': 'Open a .tala file.', onclick: openFile }, 'Open'),
    el('a', { href: `/tala/api/save?s=${encodeURIComponent(T.session)}`, download: `${song.name}.tala` },
      el('button', { 'data-title': 'Save', 'data-doc': 'Download the song as a .tala file.' }, 'Save')),
    el('a', { href: `/tala/api/bounce.wav?s=${encodeURIComponent(T.session)}`, download: `${song.name}.wav` },
      el('button', { 'data-title': 'Export audio', 'data-doc': 'Render the whole song on the server to a WAV file, frame for frame what you hear here.' }, 'Export WAV')));
  $('#undo-buttons').replaceChildren(
    el('button', { disabled: !song.undo, 'data-title': 'Undo', 'data-doc': 'Ctrl+Z. A prompt is one undo.',
                   onclick: () => T.send(form('undo')) }, '↶'),
    el('button', { disabled: !song.redo, 'data-title': 'Redo', 'data-doc': 'Ctrl+Y.',
                   onclick: () => T.send(form('redo')) }, '↷'));
}

function openFile() {
  const input = el('input', { type: 'file', accept: '.tala', onchange: async () => {
    const text = await input.files[0].text();
    await loadText(text);
  } });
  input.click();
}

async function loadText(text) {
  try {
    const { song } = await post('load', { text });
    applySong(song);
  } catch (e) {
    status(e.message);
  }
}

// The computer's keys as a keyboard, M to turn it on: the home row,
// Z and X for the octave.
const KEY_NOTES = { a: 0, w: 1, s: 2, e: 3, d: 4, f: 5, t: 6, g: 7, y: 8, h: 9, u: 10, j: 11, k: 12 };
let keyboardOn = false;
let keyboardOctave = 60;
const keysDown = new Map();

function typingInField(e) {
  return /^(INPUT|TEXTAREA|SELECT)$/.test(e.target.tagName) && e.target.type !== 'range';
}

document.addEventListener('keydown', (e) => {
  if (typingInField(e)) {
    if (e.key === 'Enter' && (e.ctrlKey || e.metaKey) && e.target.id === 'prompt-text') {
      $('#prompt-form').requestSubmit();
    }
    return;
  }
  const ctrl = e.ctrlKey || e.metaKey;
  const key = e.key.toLowerCase();
  if (e.key === 'F9') { e.preventDefault(); T.record(); return; }
  if (ctrl) {
    if (key === 'z') { e.preventDefault(); T.send(form(e.shiftKey ? 'redo' : 'undo')); }
    else if (key === 'y') { e.preventDefault(); T.send(form('redo')); }
    else if (key === 'd') { e.preventDefault(); duplicateClip(); }
    else if (key === 't') { e.preventDefault(); addTrack(':synth'); }
    return;
  }
  if (e.key === ' ') { e.preventDefault(); if (T.playing) T.stop(); else T.play(); return; }
  if (e.key === 'Tab') {
    e.preventDefault();
    if (e.shiftKey) { T.detail = T.detail === 'notes' ? 'devices' : 'notes'; renderDetail(); }
    else T.setView(T.view === 'arrangement' ? 'session' : 'arrangement');
    return;
  }
  if (e.key === 'Delete' || e.key === 'Backspace') { e.preventDefault(); deleteSelection(); return; }
  if (key === 'm') { keyboardOn = !keyboardOn; status(`Computer keyboard ${keyboardOn ? 'on: A to K play notes, Z and X change octave.' : 'off.'}`); return; }
  if (keyboardOn) {
    if (key === 'z') { keyboardOctave = Math.max(24, keyboardOctave - 12); return; }
    if (key === 'x') { keyboardOctave = Math.min(96, keyboardOctave + 12); return; }
    if (key in KEY_NOTES && !keysDown.has(key)) {
      const pitch = keyboardOctave + KEY_NOTES[key];
      keysDown.set(key, pitch);
      T.noteOn(pitch);
    }
    return;
  }
  if (key === 'l') {
    T.send(form('set-loop', T.song.loopStart, T.song.loopEnd, kw('on'), !T.song.loopOn));
  }
});

document.addEventListener('keyup', (e) => {
  const key = e.key.toLowerCase();
  if (keysDown.has(key)) {
    T.noteOff(keysDown.get(key));
    keysDown.delete(key);
  }
});

// A keyboard or pads, when the browser offers them, and the outputs
// external instruments and the clock go to.
async function connectMidi() {
  if (!navigator.requestMIDIAccess) return;
  try {
    const access = await navigator.requestMIDIAccess();
    T.midi = access;
    const listen = (input) => {
      input.onmidimessage = ({ data }) => {
        const [command, pitch, velocity] = data;
        const kind = command & 0xf0;
        if (kind === 0x90 && velocity > 0) T.noteOn(pitch, velocity);
        else if (kind === 0x80 || kind === 0x90) T.noteOff(pitch);
      };
    };
    access.inputs.forEach(listen);
    access.onstatechange = (e) => {
      if (e.port.type === 'input' && e.port.state === 'connected') listen(e.port);
      renderTransport();
      renderDetail();
    };
    renderTransport();
  } catch (e) { /* none, or refused */ }
}

$('#prompt-form').addEventListener('submit', (e) => {
  e.preventDefault();
  const box = $('#prompt-text');
  const text = box.value.trim();
  if (text) {
    box.value = '';
    T.prompt(text);
  }
});

// The page's half of the regression suite: the worklet, in a real
// AudioContext's own offline form, against the server's frames.
T.selfTest = async function selfTest(frames = 48000, script = []) {
  await T.synced;
  const song = T.song;
  const context = new OfflineAudioContext(2, frames, 48000);
  await loadModule(context, song.processor);
  const node = new AudioWorkletNode(context, song.processor, {
    numberOfInputs: 0, outputChannelCount: [2],
    processorOptions: { params: engineParams(song), score: song.score, pool: T.pool, playing: true, beat: 0, script },
  });
  node.connect(context.destination);
  const rendered = await context.startRendering();
  const scripted = (script.length ? `&script=${encodeURIComponent(JSON.stringify(script))}` : '')
    + (T.metronome ? '&metronome=1' : '');
  const buffer = await (await fetch(`/tala/api/render.f32?s=${encodeURIComponent(T.session)}&frames=${frames}${scripted}`)).arrayBuffer();
  const server = new Float32Array(buffer);
  const left = rendered.getChannelData(0);
  const right = rendered.getChannelData(1);
  let different = 0;
  let peak = 0;
  for (let i = 0; i < frames; i++) {
    if (left[i] !== server[i] || right[i] !== server[frames + i]) different++;
    peak = Math.max(peak, Math.abs(left[i]), Math.abs(right[i]));
  }
  return { ok: different === 0 && peak > 0, frames, different, peak, processor: song.processor };
};

// The page driven as a person drives it, for tests/browser.sh: it needs
// a song with a track called Bass.
T.uiTest = async function uiTest() {
  const wait = (ms) => new Promise((r) => setTimeout(r, ms));
  const checks = {};
  // wait for what the engine reports, not for a time: a loaded machine is slow
  const until = async (test, ms = 8000) => {
    for (let waited = 0; waited < ms && !test(); waited += 50) await wait(50);
    return test();
  };
  await T.play(0);
  checks.playheadMoved = await until(() => T.beat > 1);
  checks.metersMoved = await until(() => Boolean(T.peaks) && Math.max(...T.peaks) > 0.001);
  const processor = engine.processor;
  // a knob: live, then the form, and the same program goes on playing
  const bass = T.song.tracks.find((t) => t.name === 'Bass');
  const cutoff = bass.instrument.param + controlIndex(bass.instrument, 'cutoff');
  await T.send(form('set-device', 'Bass', kw('instrument'), kw('cutoff'), 333));
  checks.knobReachedParams = T.song.params[cutoff] === 333;
  checks.noRebuildOnKnob = engine.processor === processor;
  // a device: a new program, playing on from where it was
  const before = T.beat;
  await T.send(form('add-device', 'Bass', kw('phaser')));
  checks.newProgram = await until(() => engine.processor !== processor && engine.processor === T.song.processor);
  checks.keptPlaying = await until(() => T.playing && T.beat !== before);
  await T.send(form('delete-device', 'Bass', kw('phaser')));
  await T.send(form('set-device', 'Bass', kw('instrument'), kw('cutoff'), 170));
  T.stop();
  // teaching
  await T.send(form('point-at', 'device:Bass:Synth:cutoff', 'This is the brightness.'));
  checks.pointed = Boolean($('.pointed')) && Boolean($('.callout'));
  clearCallouts();
  // the views
  T.setView('mixer');
  checks.mixer = !$('#mixer').hidden && document.querySelectorAll('.strip').length === allTracks().length;
  T.setView('arrangement');
  T.trackId = bass.id;
  T.clipId = bass.clips[0].id;
  T.detail = 'notes';
  renderDetail();
  checks.pianoRoll = Boolean($('#roll'));
  T.detail = 'devices';
  renderDetail();
  checks.knobs = document.querySelectorAll('.knob').length > 20;
  checks.synthGroupsComplete = kindOf('synth').params.every((p) =>
    SYNTH_GROUPS.some(([, keys]) => keys.includes(p.key)));
  const failed = Object.entries(checks).filter(([, v]) => !v).map(([k]) => k);
  return { ok: failed.length === 0, failed, checks };
};

// A WAV file made here, three bursts of tone, for T.sampleTest.
function testWav() {
  const rate = 44100;
  const frames = rate * 2;
  const view = new DataView(new ArrayBuffer(44 + frames * 2));
  const text = (at, s) => [...s].forEach((c, i) => view.setUint8(at + i, c.charCodeAt(0)));
  text(0, 'RIFF'); view.setUint32(4, 36 + frames * 2, true); text(8, 'WAVEfmt ');
  view.setUint32(16, 16, true); view.setUint16(20, 1, true); view.setUint16(22, 1, true);
  view.setUint32(24, rate, true); view.setUint32(28, rate * 2, true);
  view.setUint16(32, 2, true); view.setUint16(34, 16, true);
  text(36, 'data'); view.setUint32(40, frames * 2, true);
  for (let i = 0; i < frames; i++) {
    const t = i / rate;
    const on = (t > 0.1 && t < 0.5) || (t > 0.9 && t < 1.2) || (t > 1.6 && t < 1.9);
    view.setInt16(44 + i * 2, on ? 16000 * Math.sin(2 * Math.PI * 220 * t) : 0, true);
  }
  return new File([view.buffer], 'Test voice.wav', { type: 'audio/wav' });
}

// A file imported as a person imports one, at a rate the engine does
// not run at, then the worklet against the server with the sample in
// the song.  Run it on a session of its own: it clears the song.
T.sampleTest = async function sampleTest() {
  const checks = {};
  await T.send(form('clear-song'));
  await T.send(form('set-tempo', 120));
  await T.importFile(testWav());
  await T.synced;
  const sample = T.song.samples[0];
  checks.imported = Boolean(sample) && sample.name === 'Test voice';
  checks.resampled = sample && Math.abs(sample.seconds - 2) < 0.01;
  const track = T.song.tracks.find((t) => t.instrument.kind === 'sampler');
  checks.onATrack = Boolean(track) && track.clips.length === 1;
  checks.poolArrived = T.pool.length >= sample.frames;
  const processor = T.song.processor;
  await T.send(form('add-audio-clip', track.name, 'Test voice', kw('start'), 8, kw('transpose'), -5, kw('offset'), 0.05));
  await T.send(form('add-track', 'Bed'), form('add-clip', 'Bed', kw('length'), 16, kw('notes'), [[48, 0, 8]]));
  await T.send(form('add-device', track.name, kw('reverb'), kw('mix'), 0.4));
  await T.synced;
  checks.clipsMakeNoProgram = T.song.processor !== processor; // a track and a device do
  const parity = await T.selfTest(48000 * 8);
  checks.parity = parity.ok;
  checks.heard = parity.peak > 0.1;
  // stretched: the song's tempo away from the clips' own, and repitched
  await T.send(form('set-tempo', 97));
  await T.synced;
  checks.parityStretched = (await T.selfTest(48000 * 8)).ok;
  await T.send(form('set-clip', track.name, 'Test voice', kw('warp'), kw('repitch')), form('set-tempo', 151));
  await T.synced;
  checks.parityRepitched = (await T.selfTest(48000 * 8)).ok;
  await T.send(form('set-tempo', 120));
  await T.synced;
  // a file dropped on a drum pad plays from that pad
  await T.send(form('add-track', 'Kit', kw('instrument'), kw('drums')),
               form('add-clip', 'Kit', kw('length'), 16, kw('notes'), [[37, 0, 0.25], [36, 2, 0.25], [37, 9, 0.25]]));
  await T.synced;
  T.trackId = T.song.tracks.find((t) => t.name === 'Kit').id;
  T.detail = 'devices';
  renderDetail();
  const padButton = document.querySelector('[data-ctl="pad:2"]');
  const dropped = new DataTransfer();
  dropped.items.add(testWav());
  padButton.dispatchEvent(new DragEvent('drop', { dataTransfer: dropped, bubbles: true, cancelable: true }));
  const kitPads = () => T.song.tracks.find((t) => t.name === 'Kit').instrument.pads;
  for (let i = 0; i < 200 && !kitPads()[1]; i++) await new Promise((r) => setTimeout(r, 25));
  await T.synced;
  checks.padDrop = Boolean(kitPads()[1]);
  checks.parityPads = (await T.selfTest(48000 * 6)).ok;
  await T.play(0);
  await new Promise((r) => setTimeout(r, 700));
  checks.plays = T.peaks && Math.max(...T.peaks) > 0.01;
  T.stop();
  // the front of a clip cut off by its left edge, as a mouse drags it
  T.setView('arrangement');
  const drag = async (target, x0, y0, dx) => {
    const at = (x) => ({ clientX: x, clientY: y0, bubbles: true, pointerId: 1 });
    target.dispatchEvent(new PointerEvent('pointerdown', at(x0)));
    window.dispatchEvent(new PointerEvent('pointermove', at(x0 + dx)));
    window.dispatchEvent(new PointerEvent('pointerup', at(x0 + dx)));
    await chain;
  };
  const first = T.song.tracks.find((t) => t.id === track.id).clips.find((c) => c.scene === null && c.start === 0);
  const edge = document.querySelector(`[data-ctl="clip:${first.id}"] .clip-edge.left`);
  const box = edge.getBoundingClientRect();
  await drag(edge, box.left + 2, box.top + 5, T.ppb * 0.5);
  const trimmed = T.song.tracks.find((t) => t.id === track.id).clips.find((c) => c.id === first.id);
  checks.leftEdgeTrims = trimmed.start === 0.5 && trimmed.length === first.length - 0.5 && trimmed.offset === 0.25;
  // and the end of what it plays, by the waveform's right handle
  T.clipId = first.id;
  T.trackId = track.id;
  T.detail = 'notes';
  renderDetail();
  const view = $('#detail-body canvas');
  checks.clipView = Boolean(view);
  checks.warpControl = Boolean(document.querySelector('[data-ctl="warp"]'));
  const vbox = view.getBoundingClientRect();
  const sampleSeconds = T.song.samples[0].seconds;
  const toX = vbox.left + (0.25 + trimmed.length * 0.5) / sampleSeconds * view.width;
  await drag(view, toX, vbox.top + 20, -view.width * 0.25 / sampleSeconds);
  const shortened = T.song.tracks.find((t) => t.id === track.id).clips.find((c) => c.id === first.id);
  checks.waveformHandleTrims = Math.abs(shortened.length - (trimmed.length - 0.5)) < 0.02;
  const after = await T.selfTest(48000 * 4);
  checks.parityAfterTrims = after.ok;
  // a repair from its button: the sample's sound changes, the clip plays it
  const wait = (ms) => new Promise((r) => setTimeout(r, ms));
  const stamp = T.song.poolStamp;
  $('[data-ctl="repair:normalize-sample"]').click();
  for (let i = 0; i < 100 && T.song.poolStamp === stamp; i++) await wait(50);
  await T.synced;
  checks.repairButton = T.song.poolStamp !== stamp && /:now -1/.test($('.repair-row .turn-note').textContent);
  checks.parityAfterRepair = (await T.selfTest(48000 * 4)).ok;
  renderDetail();
  checks.listed = Boolean(document.querySelector('[data-ctl="sample:Test voice"]'));
  const info = await T.send(form('sample-info', 'Test voice'));
  checks.phrases = /:phrases \(\(.*\) \(.*\) \(.*\)\)/.test(info.results[0].value);
  const failed = Object.entries(checks).filter(([, v]) => !v).map(([k]) => k);
  return { ok: failed.length === 0, failed, checks, frames: parity.frames, different: parity.different, info: info.results[0].value };
};

// The session as a person plays it, on a session of its own (it clears
// the song): a performance held to the server's render, then the grid
// driven by its buttons.
T.sessionTest = async function sessionTest() {
  const checks = {};
  const wait = (ms) => new Promise((r) => setTimeout(r, ms));
  await T.send(form('clear-song'));
  await T.send(form('set-tempo', 120), form('set-loop', 0, 2),
               form('add-track', 'Kick', kw('instrument'), kw('drums')),
               form('add-clip', 'Kick', kw('length'), 4, kw('notes'), [[36, 0, 0.1], [36, 1, 0.1], [36, 2, 0.1], [36, 3, 0.1]]),
               form('add-clip', 'Kick', kw('scene'), 1, kw('length'), 2, kw('notes'), [[36, 0, 0.1], [38, 1, 0.1]]),
               form('add-track', 'Lead', kw('values'), [kw('sustain'), 0.3]),
               form('add-clip', 'Lead', kw('scene'), 0, kw('length'), 4, kw('notes'), [[60, 0, 1], [63, 1, 1], [67, 2, 2]]),
               form('add-clip', 'Lead', kw('scene'), 1, kw('length'), 1, kw('loop'), 1, kw('notes'), [[72, 0, 0.25]]),
               form('rename-scene', 0, 'Groove'), form('rename-scene', 1, 'Drop'));
  await T.synced;
  T.setView('session');
  renderSession();
  checks.grid = document.querySelectorAll('.slot').length === 2 * T.song.scenes.length
    && document.querySelectorAll('.slot.full').length === 3;
  // a performance: scene 0 at once, the drop launched mid-bar so it
  // waits for the bar, then everything back to the timeline
  const script = [[0, 'launch', 0, -1], [0, 'launch', 1, 0], [30000, 'launch', 0, 1], [30000, 'launch', 1, 1],
                  [200000, 'arrange', -1]];
  const parity = await T.selfTest(48000 * 6, script);
  checks.parity = parity.ok;
  checks.parityHeard = parity.peak > 0.05;
  // the buttons; the engine reports what it is doing a few times a
  // second, so wait for the report rather than for a time
  const until = async (test, ms = 5000) => {
    for (let waited = 0; waited < ms && !test(); waited += 50) await wait(50);
    return test();
  };
  await T.launchScene(0);
  const lead = T.song.tracks.findIndex((t) => t.name === 'Lead');
  checks.launched = await until(() => T.sessionState && T.sessionState[lead][0] > 0.5 && T.sessionState[lead][1] === 1);
  checks.playingShown = await until(() => Boolean(document.querySelector('.slot.playing')));
  checks.arrangeButton = await until(() => !$('#back-to-arrangement').hidden);
  T.backToArrangement();
  checks.backToTimeline = await until(() => T.sessionState && T.sessionState.every((s) => s[0] < 0.5));
  T.stop();
  await T.send(form('arrange-scene', 'Drop', 8, kw('length'), 8));
  checks.arranged = T.song.tracks.every((t) => t.clips.some((c) => c.scene === null && c.start === 8));
  const failed = Object.entries(checks).filter(([, v]) => !v).map(([k]) => k);
  return { ok: failed.length === 0, failed, checks, frames: parity.frames, different: parity.different };
};

// A take played as a person plays one, through the same doors a MIDI
// keyboard uses, with the metronome on.  It clears the song.
T.recordTest = async function recordTest() {
  const checks = {};
  const wait = (ms) => new Promise((r) => setTimeout(r, ms));
  const until = async (test, ms = 8000) => {
    for (let waited = 0; waited < ms && !test(); waited += 10) await wait(10);
    return test();
  };
  await T.send(form('clear-song'));
  await T.send(form('set-tempo', 120), form('set-loop', 0, 16), form('add-track', 'Keys'));
  await T.synced;
  T.trackId = T.song.tracks[0].id;
  T.armed.clear();
  T.toggleMetronome();
  const parity = await T.selfTest(48000 * 3);
  checks.metronomeParity = parity.ok && parity.peak > 0.1;
  T.startBeat = 0;
  await T.record();
  checks.recording = T.recording && T.playing && T.armed.size === 1;
  const play = async (pitch, at, until_) => {
    await until(() => T.beat >= at);
    T.noteOn(pitch, 100);
    await until(() => T.beat >= until_);
    T.noteOff(pitch);
  };
  await play(60, 0.5, 1.0);
  await play(64, 1.5, 2.25);
  await until(() => T.beat >= 2.5);
  await T.record();
  await chain;
  T.stop();
  T.toggleMetronome();
  const clip = T.song.tracks[0].clips[0];
  checks.take = Boolean(clip) && clip.notes.length === 2 && clip.start === 0 && clip.length === 4;
  // the page's clock is read every few milliseconds, so a note goes a
  // little after the beat asked for; it lands earlier by the output's
  // latency, which a player hears the beat late by
  const ctx = engine.ctx;
  const late = ((ctx.outputLatency || 0) + (ctx.baseLatency || 0)) * T.song.tempo / 60;
  const near = (start, beat) => start >= beat - late - 0.02 && start < beat - late + 0.25;
  checks.landedWhere = Boolean(clip) && near(clip.notes[0][1], 0.5) && near(clip.notes[1][1], 1.5);
  checks.latencyCompensated = late >= 0;
  checks.lengths = Boolean(clip) && clip.notes[0][2] > 0.3 && clip.notes[0][2] < 0.75;
  await T.send(form('quantize-notes', 'Keys', clip.id, 0.5));
  const tight = T.song.tracks[0].clips[0];
  checks.quantized = tight.notes[0][1] === 0.5 && tight.notes[1][1] === 1.5;
  checks.oneUndo = (await T.send(form('undo'), form('undo'))).ok && T.song.tracks[0].clips.length === 0;
  const failed = Object.entries(checks).filter(([, v]) => !v).map(([k]) => k);
  return { ok: failed.length === 0, failed, checks, notes: clip && clip.notes };
};

// An audio take from Chromium's pretend microphone: kept as a sample and
// placed; then a take over a loop, of which the last pass is kept.  It
// clears the song.
T.audioRecordTest = async function audioRecordTest() {
  const checks = {};
  const wait = (ms) => new Promise((r) => setTimeout(r, ms));
  const until = async (test, ms = 15000) => {
    for (let waited = 0; waited < ms && !test(); waited += 10) await wait(10);
    return test();
  };
  await T.send(form('clear-song'));
  await T.send(form('set-tempo', 120), form('set-loop', 0, 16), form('add-track', 'Voice', kw('instrument'), kw('sampler')));
  await T.synced;
  const voice = T.song.tracks[0];
  T.trackId = voice.id;
  T.armed.clear();
  T.armed.add(voice.id);
  T.detail = 'devices';
  renderDetail();
  checks.inputCard = Boolean(document.querySelector(`[data-ctl="input:Voice"]`));
  await T.openInput();
  checks.inputOpen = Boolean(T.input.source) && T.input.devices.length > 0;
  T.startBeat = 0;
  await T.record();
  await until(() => T.beat >= 4);
  await T.record();
  T.stop();
  await chain;
  const sample = T.song.samples[0];
  const clip = T.song.tracks[0].clips[0];
  checks.kept = Boolean(sample) && sample.seconds > 1.7 && sample.seconds < 2.6;
  checks.placed = Boolean(clip) && clip.sample === sample.id && clip.start >= 0 && clip.start < 0.25;
  await T.synced;
  let peak = 0;
  for (let i = 0; i < sample.frames; i++) peak = Math.max(peak, Math.abs(T.pool[sample.left + i]));
  checks.heardSomething = peak > 0.01;
  // over a loop of two beats, a little more than two passes: the last is kept
  await T.send(form('set-loop', 0, 2));
  await T.record();
  const began = T.clock;
  await until(() => T.clock >= began + 5);
  await T.record();
  T.stop();
  await chain;
  const second = T.song.samples[1];
  const secondClip = T.song.tracks[0].clips.find((c) => c.sample === (second && second.id));
  checks.lastPassKept = Boolean(second) && second.seconds < 1.05 && second.seconds > 0.2;
  checks.lastPassAtLoopStart = Boolean(secondClip) && secondClip.start < 0.25;
  checks.parity = (await T.selfTest(48000 * 2)).ok;
  const failed = Object.entries(checks).filter(([, v]) => !v).map(([k]) => k);
  return { ok: failed.length === 0, failed, checks, first: sample && sample.seconds, second: second && second.seconds,
           clip: clip && [clip.start, clip.offset], latency: T.input.latency };
};

// An external instrument's notes and the clock, sent to a stand-in MIDI
// output that keeps what it is sent and when it is due.  It clears the
// song.
T.midiOutTest = async function midiOutTest() {
  const checks = {};
  const wait = (ms) => new Promise((r) => setTimeout(r, ms));
  const sent = [];
  const fake = { name: 'Fake synth', send: (data, at) => sent.push({ data: [...data], at: at === undefined ? performance.now() : at }) };
  const realMidi = T.midi;
  T.midi = { outputs: new Map([['fake', fake]]), inputs: new Map() };
  await T.send(form('clear-song'));
  await T.send(form('set-tempo', 120), form('set-loop', 0, 16, kw('on'), null),
               form('add-track', 'Moog', kw('instrument'), kw('external'), kw('values'), [kw('channel'), 3]),
               form('add-clip', 'Moog', kw('length'), 4, kw('notes'), [[48, 0, 1], [50, 1, 0.5], [52, 2, 0.25]]));
  await T.synced;
  const moog = T.song.tracks[0];
  T.trackId = moog.id;
  T.detail = 'devices';
  renderDetail();
  checks.portListed = Boolean(document.querySelector('[data-ctl="midi-port"] option, select[data-ctl="midi-port"]'));
  // the output's clock is off for its first moments (headless Chromium:
  // about 20 ms on the first Play), which this test is not about
  await T.play(0);
  await wait(400);
  T.stop();
  await wait(300);
  sent.length = 0;
  T.setClockPort('Fake synth');
  renderTransport();
  checks.clockControl = Boolean(document.querySelector('[data-ctl="clock-out"]'));
  T.startBeat = 0;
  await T.play(0);
  for (let i = 0; i < 600 && T.beat < 3.2; i++) await wait(10);
  T.stop();
  await wait(400);
  const ons = sent.filter((m) => m.data[0] === 0x92);
  const offs = sent.filter((m) => m.data[0] === 0x82);
  checks.notesOnChannel3 = ons.map((m) => m.data[1]).join() === '48,50,52';
  const gaps = [ons[1].at - ons[0].at, ons[2].at - ons[1].at];
  checks.onTheBeat = gaps.every((g) => Math.abs(g - 500) < 15);
  const offOf = (pitch, after) => offs.find((m) => m.data[1] === pitch && m.at >= after.at);
  checks.heldTheirLength = Math.abs(offOf(48, ons[0]).at - ons[0].at - 500) < 5
    && Math.abs(offOf(50, ons[1]).at - ons[1].at - 250) < 5;
  checks.clockStarts = sent.length > 1 && sent[0].data[0] === 0xf2 && sent[1].data[0] === 0xfa;
  const ticks = sent.filter((m) => m.data[0] === 0xf8 && m.at >= ons[0].at - 1 && m.at < ons[0].at + 999);
  checks.clock24 = Math.abs(ticks.length - 48) <= 1;
  const tickGaps = ticks.slice(1).map((m, i) => m.at - ticks[i].at);
  checks.clockEven = tickGaps.every((g) => Math.abs(g - 500 / 24) < 2);
  checks.clockStops = sent.some((m) => m.data[0] === 0xfc);
  checks.allNotesOff = sent.some((m) => m.data[0] === 0xb2 && m.data[1] === 123);
  // played live on the armed track: through to the hardware at once
  T.armed.clear();
  T.armed.add(moog.id);
  const before = sent.length;
  await T.noteOn(60, 90);
  T.noteOff(60);
  const live = sent.slice(before);
  checks.liveThrough = live.some((m) => m.data.join() === '146,60,90') && live.some((m) => m.data[0] === 0x82 && m.data[1] === 60);
  // an external track is silent here, on both sides
  checks.parity = (await T.selfTest(48000 * 2)).different === 0;
  T.setClockPort('');
  T.armed.clear();
  T.midi = realMidi;
  const failed = Object.entries(checks).filter(([, v]) => !v).map(([k]) => k);
  return { ok: failed.length === 0, failed, checks, gaps, ticks: ticks.length,
           worstTickMs: Math.max(...tickGaps.map((g) => Math.abs(g - 500 / 24))),
           latencyMs: Math.round(((engine.ctx.outputLatency || 0) + (engine.ctx.baseLatency || 0)) * 1000) };
};

// A finger for the tests.  STEPS are [x, y] points it goes down on and
// moves through before it lifts, or a number of milliseconds to hold
// still.  tests/probe.js sends it down the browser's own touch path;
// without the probe, the events one would cause are made here.
let fingers = 0;
const lifting = new Map();

T.touched = function touched(serial) {
  const done = lifting.get(serial);
  lifting.delete(serial);
  if (done) done();
};

async function finger(...steps) {
  if (window.probeTouch) {
    await new Promise((done) => {
      lifting.set(++fingers, done);
      window.probeTouch(JSON.stringify({ serial: fingers, steps }));
    });
    return;
  }
  const event = (type, [x, y]) => new PointerEvent(type, {
    bubbles: true, cancelable: true, pointerId: 7, pointerType: 'touch', clientX: x, clientY: y,
  });
  const points = steps.filter(Array.isArray);
  const first = points[0];
  const last = points[points.length - 1];
  const target = document.elementFromPoint(...first);
  let held = 0;
  target.dispatchEvent(event('pointerdown', first));
  for (const step of steps.slice(1)) {
    if (Array.isArray(step)) window.dispatchEvent(event('pointermove', step));
    else {
      held += step;
      await new Promise((r) => setTimeout(r, step));
    }
  }
  window.dispatchEvent(event('pointerup', last));
  if (held < 500 && Math.hypot(last[0] - first[0], last[1] - first[1]) < 10) {
    target.dispatchEvent(new MouseEvent('click', { bubbles: true, cancelable: true, clientX: first[0], clientY: first[1] }));
  }
}

// The page as a phone shows it and a finger drives it, on a session of
// its own (it clears the song).  tests/browser.sh runs it upright and on
// its side; in a wide window it holds the phone's layout while it runs.
T.phoneTest = async function phoneTest() {
  const checks = {};
  const wait = (ms) => new Promise((r) => setTimeout(r, ms));
  const until = async (test, ms = 5000) => {
    for (let waited = 0; waited < ms && !test(); waited += 50) await wait(50);
    return Boolean(test());
  };
  const real = Boolean(window.probeTouch);
  if (!real) T.setPhone(true);
  const shown = (node) => Boolean(node) && node.getClientRects().length > 0;
  const middle = (node) => {
    const box = node.getBoundingClientRect();
    return [Math.round(box.left + box.width / 2), Math.round(box.top + box.height / 2)];
  };
  const tap = (node) => finger(middle(node));
  const bass = () => T.song.tracks.find((t) => t.name === 'Bass');
  await T.send(form('clear-song'));
  await T.send(form('set-tempo', 120), form('add-track', 'Bass'),
               form('add-clip', 'Bass', kw('start'), 0, kw('length'), 4, kw('notes'), [[36, 0, 1], [43, 2, 1]]),
               form('add-clip', 'Bass', kw('start'), 60, kw('length'), 4),
               form('add-track', 'Kick', kw('instrument'), kw('drums')));
  await T.synced;
  T.setView('arrangement');
  // the layout: one view, the tabs, nothing wider or taller than the window
  const fits = () => document.documentElement.scrollWidth <= innerWidth
    && Math.round($('#tabs').getBoundingClientRect().bottom) <= innerHeight
    && Math.round($('#top').getBoundingClientRect().right) <= innerWidth;
  checks.phone = T.phone && T.landscape === (real && innerWidth > innerHeight);
  checks.fits = fits();
  checks.oneView = shown($('#arrangement')) && shown($('#tabs')) && !shown($('#view-tabs'))
    && !shown($('#browser')) && !shown($('#prompt')) && !shown($('#detail'));
  // the tabs
  await tap(findControl('view:session'));
  checks.sessionTab = await until(() => T.view === 'session' && shown($('#session')) && !shown($('#arrangement')));
  await tap(findControl('view:mixer'));
  checks.mixerTab = await until(() => shown($('#mixer')) && document.querySelectorAll('.strip').length === allTracks().length);
  await tap(findControl('view:arrangement'));
  await until(() => T.view === 'arrangement');
  // a sheet: up, the whole screen, half again, away
  await tap(findControl('sheet:prompt'));
  checks.sheetUp = await until(() => shown($('#prompt')) && shown($('#stage')));
  checks.sheetFits = fits();
  await tap(findControl('sheet:full'));
  checks.sheetFull = await until(() => shown($('#prompt')) && !shown($('#stage')));
  await tap(findControl('sheet:full'));
  await tap(findControl('sheet:close'));
  checks.sheetAway = await until(() => !shown($('#prompt')) && shown($('#stage')));
  // the browser adds a track and gets out of the way
  await tap(findControl('add-track'));
  checks.browserUp = await until(() => shown($('#browser')));
  await tap([...document.querySelectorAll('#browser-list .kind')].find((n) => n.textContent === 'Return track'));
  checks.addedFromBrowser = await until(() => T.song.returns.length === 1 && !shown($('#browser')));
  // the menu
  await tap($('#menu-button'));
  checks.menuOpens = await until(() => shown($('#file-buttons')) && shown($('#tempo')));
  await finger(middle($('#arrangement')));
  checks.menuCloses = await until(() => !shown($('#file-buttons')));
  // a finger held on a control reads its help and does not press it
  const play = middle($('#play'));
  await finger(play, 750);
  checks.heldReadsHelp = !$('#toast').hidden && /play/i.test($('#toast').textContent) && !T.playing;
  $('#toast').hidden = true;
  // zoom
  const ppb = T.ppb;
  await tap(findControl('zoom-in'));
  checks.zoom = await until(() => T.ppb > ppb);
  await tap(findControl('zoom-out'));
  await until(() => T.ppb === ppb);
  // a swipe along a lane scrolls the song, clip or no clip; the browser
  // does that, so only its own finger shows it
  const clipNode = () => document.querySelector(`[data-ctl="clip:${bass().clips[0].id}"]`);
  const [cx, cy] = middle(clipNode());
  if (real) {
    await finger([cx + 60, cy], [cx + 20, cy], [cx - 40, cy], [cx - 80, cy]);
    checks.swipeScrolls = await until(() => $('#arrangement').scrollLeft > 20, 2000);
    checks.swipeLeavesClip = bass().clips[0].start === 0;
    $('#arrangement').scrollLeft = 0;
    await wait(100);
  }
  // a tap chooses the clip; then it moves, and the strip has its verbs
  await tap(clipNode());
  checks.tapSelects = await until(() => T.clipId === bass().clips[0].id && shown(findControl('action:edit')));
  const [sx, sy] = middle(clipNode());
  await finger([sx, sy], [sx + T.ppb, sy], [sx + 3 * T.ppb, sy], [sx + 4 * T.ppb, sy]);
  checks.dragMoves = await until(() => bass().clips[0].start === 4);
  await tap(findControl('action:duplicate'));
  checks.duplicate = await until(() => bass().clips.length === 3);
  await T.send(form('undo'));
  // an automation lane: a tap makes a point, a tap on it takes it away
  await tap(findControl('track:Bass:automation'));
  await until(() => Boolean($('.auto-lane')));
  const lane = $('.auto-lane').getBoundingClientRect();
  const spot = [Math.round(lane.left + 5 * T.ppb), Math.round(lane.top + lane.height / 2)];
  await finger(spot);
  checks.autoPoint = await until(() => document.querySelectorAll('.auto-lane .ring').length === 1, 3000);
  await wait(450);
  if (checks.autoPoint) await finger(middle($('.auto-lane .ring')));
  checks.autoPointGone = await until(() => Boolean($('.auto-lane')) && !$('.auto-lane .ring'), 3000);
  await tap(findControl('track:Bass:automation'));
  // the clip's notes, in the sheet: Draw makes one, Delete takes it away
  await tap(clipNode());
  await until(() => shown(findControl('action:edit')));
  await tap(findControl('action:edit'));
  checks.editOpensRoll = await until(() => T.sheet === 'detail' && shown($('#roll')));
  checks.rollFits = fits();
  const notes = () => bass().clips[0].notes.length;
  await tap($('#draw-notes'));
  await until(() => T.drawing);
  const grid = $('.roll-wrap').getBoundingClientRect();
  const cell = [Math.round(grid.left + grid.width * 0.7), Math.round(grid.top + grid.height * 0.3)];
  await finger(cell);
  checks.drawAdds = await until(() => notes() === 3, 3000);
  await tap($('#draw-notes'));
  await until(() => !T.drawing);
  await finger(cell);
  checks.tapChoosesNote = await until(() => !$('#delete-note').disabled, 2000);
  await tap($('#delete-note'));
  checks.deleteNote = await until(() => notes() === 2, 3000);
  // a knob follows a finger up
  T.detail = 'devices';
  renderDetail();
  const params = T.song.params.join();
  const [kx, ky] = middle($('#detail .knob'));
  await finger([kx, ky], [kx, ky - 15], [kx, ky - 30], [kx, ky - 45]);
  checks.knobTurns = await until(() => T.song.params.join() !== params, 3000);
  await tap(findControl('sheet:close'));
  // two taps are a double-click: an empty slot gets a clip and its notes
  await tap(findControl('view:session'));
  await until(() => T.view === 'session');
  const slot = middle(findControl('slot:Kick:0'));
  await finger(slot);
  await finger(slot);
  checks.doubleTap = await until(() => T.song.tracks.find((t) => t.name === 'Kick').clips.length === 1
    && T.sheet === 'detail' && shown($('#roll')));
  T.setSheet(null);
  T.setView('arrangement');
  checks.fitsAfter = fits();
  if (!real) {
    T.setPhone(null);
    checks.deskAgain = !T.phone && shown($('#browser')) && shown($('#prompt')) && !shown($('#tabs'));
  }
  const failed = Object.entries(checks).filter(([, v]) => !v).map(([k]) => k);
  return { ok: failed.length === 0, failed, checks, width: innerWidth, height: innerHeight, real };
};

async function start() {
  const query = new URLSearchParams(location.search);
  T.session = query.get('s') || (() => {
    let id = null;
    try { id = localStorage.getItem('tala-session'); } catch (e) { /* private window */ }
    if (!id) {
      id = Math.random().toString(36).slice(2, 12);
      try { localStorage.setItem('tala-session', id); } catch (e) { /* private window */ }
    }
    return id;
  })();
  T.renderers.push(renderTransport, renderFileButtons, renderSamples, renderArrangement, renderSession, renderMixer, renderTabs, renderDetail);
  try {
    const [kinds, song] = await Promise.all([
      fetch('/tala/api/kinds').then((r) => r.json()),
      fetch(`/tala/api/state?s=${encodeURIComponent(T.session)}`).then((r) => r.json()),
    ]);
    T.kinds = kinds;
    renderBrowser();
    applySong(song);
    let kept = null;
    try { kept = localStorage.getItem('tala-song'); } catch (e) { /* private window */ }
    if (!song.tracks.length && kept && !query.get('s')) await loadText(kept);
    requestAnimationFrame(animate);
    connectMidi();
    T.ready = true;
  } catch (e) {
    T.error = e.message;
    status(e.message);
  }
}

start();
