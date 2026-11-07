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
      },
    }, k.label)),
    el('h3', {}, 'Effects'),
    kinds('effect').map((k) => el('button', {
      class: 'kind', 'data-title': k.label, 'data-doc': `${k.doc} Click to add it to the selected track.`,
      onclick: () => {
        T.detail = 'devices';
        T.send(`(add-device ${lit(selectedTrack().name)} :${k.key})`);
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
      el('button', { 'data-title': 'Export audio', 'data-doc': 'Render the whole song on the server to a WAV file, frame for frame what you hear here.' }, 'Export WAV')),
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
    else T.setView(T.view === 'mixer' ? 'arrangement' : 'mixer');
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

// A keyboard or pads, when the browser offers them.
async function connectMidi() {
  if (!navigator.requestMIDIAccess) return;
  try {
    const access = await navigator.requestMIDIAccess();
    const listen = (input) => {
      input.onmidimessage = ({ data }) => {
        const [command, pitch, velocity] = data;
        const kind = command & 0xf0;
        if (kind === 0x90 && velocity > 0) T.noteOn(pitch, velocity);
        else if (kind === 0x80 || kind === 0x90) T.noteOff(pitch);
      };
    };
    access.inputs.forEach(listen);
    access.onstatechange = (e) => { if (e.port.type === 'input' && e.port.state === 'connected') listen(e.port); };
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
T.selfTest = async function selfTest(frames = 48000) {
  await T.synced;
  const song = T.song;
  const context = new OfflineAudioContext(2, frames, 48000);
  await loadModule(context, song.processor);
  const node = new AudioWorkletNode(context, song.processor, {
    numberOfInputs: 0, outputChannelCount: [2],
    processorOptions: { params: song.params, score: song.score, pool: T.pool, playing: true, beat: 0 },
  });
  node.connect(context.destination);
  const rendered = await context.startRendering();
  const buffer = await (await fetch(`/tala/api/render.f32?s=${encodeURIComponent(T.session)}&frames=${frames}`)).arrayBuffer();
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
  await T.play(0);
  await wait(1500);
  checks.playheadMoved = T.beat > 1;
  checks.metersMoved = Boolean(T.peaks) && Math.max(...T.peaks) > 0.001;
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
  await wait(600);
  checks.newProgram = engine.processor !== processor && engine.processor === T.song.processor;
  checks.keptPlaying = T.playing && T.beat !== before;
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
  await T.play(0);
  await new Promise((r) => setTimeout(r, 700));
  checks.plays = T.peaks && Math.max(...T.peaks) > 0.01;
  T.stop();
  T.clipId = track.clips[0].id;
  T.trackId = track.id;
  T.detail = 'notes';
  renderDetail();
  checks.clipView = Boolean($('#detail-body canvas'));
  checks.listed = Boolean(document.querySelector('[data-ctl="sample:Test voice"]'));
  const info = await T.send(form('sample-info', 'Test voice'));
  checks.phrases = /:phrases \(\(.*\) \(.*\) \(.*\)\)/.test(info.results[0].value);
  const failed = Object.entries(checks).filter(([, v]) => !v).map(([k]) => k);
  return { ok: failed.length === 0, failed, checks, frames: parity.frames, different: parity.different, info: info.results[0].value };
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
  T.renderers.push(renderTransport, renderFileButtons, renderSamples, renderArrangement, renderMixer, renderTabs, renderDetail);
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
