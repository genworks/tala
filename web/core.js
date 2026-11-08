// Copyright © 2026 Genworks International
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.  Distributed WITHOUT
// ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

// The page holds no song of its own.  It shows what the server answers
// and sends back forms of the Lisp API, the same operations the prompt's
// model runs, so everything done here can be done there.  While a knob
// moves its value goes straight to the audio thread; the form goes when
// it lets go.

'use strict';

const T = window.TALA = {
  ready: false, error: null,
  song: null, kinds: [],
  session: null,
  trackId: null, clipId: null,
  view: 'arrangement', detail: 'devices',
  ppb: 22, beat: 0, startBeat: 0, playing: false,
  pool: new Float32Array(1), poolStamp: null,
  autoOpen: {},
  renderers: [],
};

const $ = (selector, root = document) => root.querySelector(selector);

function el(tag, attrs = {}, ...children) {
  const node = document.createElement(tag);
  for (const [key, value] of Object.entries(attrs)) {
    if (value === null || value === undefined || value === false) continue;
    if (key.startsWith('on')) node.addEventListener(key.slice(2), value);
    else if (key === 'class') node.className = value;
    else if (key === 'style') node.style.cssText = value;
    else if (key in node && key !== 'list') node[key] = value;
    else node.setAttribute(key, value);
  }
  for (const child of children.flat()) {
    if (child === null || child === undefined || child === false) continue;
    node.append(child.nodeType ? child : String(child));
  }
  return node;
}

const COLORS = ['#f0a63c', '#38c3b4', '#e2604f', '#8f7ef2', '#7cc05a', '#e47fb5',
                '#5aa7e8', '#d6c24a', '#c9835a', '#62d08f', '#b488e0', '#9aa4b8'];

// Forms: a JavaScript value as a Lisp literal.
const kw = (name) => ({ kw: name });

function lit(x) {
  if (x === null || x === undefined || x === false) return 'nil';
  if (x === true) return 't';
  if (typeof x === 'number') return String(+x.toFixed(6));
  if (typeof x === 'string') return JSON.stringify(x);
  if (Array.isArray(x)) return '(' + x.map(lit).join(' ') + ')';
  if (x.kw) return ':' + x.kw;
  throw new Error('No literal for ' + x);
}

const form = (op, ...args) => '(' + [op, ...args.map(lit)].join(' ') + ')';

// One request at a time, in order, so edits arrive as they were made.
let chain = Promise.resolve();

function post(path, body) {
  return fetch(`/tala/api/${path}?s=${encodeURIComponent(T.session)}`, {
    method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body),
  }).then(async (r) => {
    if (!r.ok) throw new Error(await r.text());
    return r.json();
  });
}

T.send = function send(...forms) {
  const text = forms.join('\n');
  chain = chain.then(async () => {
    try {
      const answer = await post('forms', { forms: text });
      applySong(answer.song);
      const bad = answer.results.find((r) => !r.ok);
      if (bad) status(bad.error);
      if (answer.ui.length) T.doUi(answer.ui);
      return answer;
    } catch (e) {
      status(e.message);
      return { ok: false, results: [], error: e.message };
    }
  });
  return chain;
};

function status(text) {
  const info = $('#info');
  info.replaceChildren(el('b', {}, 'Tala'), el('p', {}, text));
}

function applySong(song) {
  T.song = song;
  const tracks = allTracks();
  if (!tracks.some((t) => t.id === T.trackId)) {
    T.trackId = (song.tracks[0] || song.master).id;
  }
  if (!selectedClip()) T.clipId = null;
  T.synced = fetchPool().then(syncEngine);
  renderAll();
  saveLocal();
}

// The song's recorded sound, the same numbers the server renders from.
// A song names its samples, so this comes only when they change.
async function fetchPool() {
  const stamp = T.song.poolStamp;
  if (stamp === T.poolStamp) return;
  const r = await fetch(`/tala/api/pool?s=${encodeURIComponent(T.session)}`);
  T.pool = new Float32Array(await r.arrayBuffer());
  T.poolStamp = stamp;
  if (engine.node) engine.node.port.postMessage({ t: 'pool', pool: T.pool });
  renderArrangement();
  renderDetail();
}

// A file becomes float32 at the engine's rate here, once.  The browser
// resamples as it decodes, and what it made is the sound from then on.
T.decodeFile = async function decodeFile(file) {
  const context = new OfflineAudioContext(1, 1, 48000);
  const audio = await context.decodeAudioData(await file.arrayBuffer());
  const channels = Math.min(2, audio.numberOfChannels);
  const data = new Float32Array(channels * audio.length);
  for (let c = 0; c < channels; c++) data.set(audio.getChannelData(c), c * audio.length);
  return { channels, data };
};

T.uploadSample = async function uploadSample(name, channels, data) {
  const query = `s=${encodeURIComponent(T.session)}&name=${encodeURIComponent(name)}&channels=${channels}`;
  const r = await fetch(`/tala/api/sample?${query}`, {
    method: 'POST', headers: { 'Content-Type': 'application/octet-stream' }, body: data,
  });
  if (!r.ok) throw new Error(await r.text());
  const answer = await r.json();
  applySong(answer.song);
  return answer.name;
};

// An audio track to put a clip on: the given one, the selected one, or
// a new one.
async function audioTrack(track) {
  const isAudio = (t) => t && t.instrument && t.instrument.kind === 'sampler';
  if (isAudio(track)) return track.name;
  const selected = selectedTrack();
  if (isAudio(selected)) return selected.name;
  const name = uniqueName('Audio');
  await T.send(`(add-track ${lit(name)} :instrument :sampler)`);
  return name;
}

T.importFile = async function importFile(file, track, beat = T.startBeat) {
  status(`Reading ${file.name}...`);
  try {
    const { channels, data } = await T.decodeFile(file);
    const name = await T.uploadSample(file.name.replace(/\.[^.]*$/, ''), channels, data);
    const target = await audioTrack(track);
    await T.send(form('add-audio-clip', target, name, kw('start'), beat));
    status(`${name} is on ${target}. Tell the prompt what it says with (set-sample), or ask it to.`);
  } catch (e) {
    status(`Could not import ${file.name}: ${e.message}`);
  }
};

function renderAll() {
  for (const render of T.renderers) render();
}

const allTracks = () => [...T.song.tracks, ...T.song.returns, T.song.master];
const selectedTrack = () => allTracks().find((t) => t.id === T.trackId);

function selectedClip() {
  const track = selectedTrack();
  return track && track.clips.find((c) => c.id === T.clipId);
}

const kindOf = (key) => T.kinds.find((k) => k.key === key);
const beatsPerBar = () => T.song.numerator * 4 / T.song.denominator;

// A device's control, by its parameter table.
function controlIndex(device, key) {
  return kindOf(device.kind).params.findIndex((p) => p.key === key);
}

function controlValue(device, key) {
  return T.song.params[device.param + controlIndex(device, key)];
}

// Which device a form calls a device by.
function deviceRef(track, device) {
  return device === track.instrument ? kw('instrument') : track.devices.indexOf(device);
}

function setLive(index, value) {
  T.song.params[index] = value;
  if (engine.node) engine.node.port.postMessage({ t: 'param', i: index, v: value });
}

// Audio.  The context runs at the server's rate, so the worklet and the
// bounce are the same frames.

const engine = { ctx: null, node: null, out: null, processor: null, loaded: new Set(), ticket: 0 };

async function startAudio() {
  if (!engine.ctx) {
    engine.ctx = new AudioContext({ sampleRate: 48000, latencyHint: 'interactive' });
    engine.out = engine.ctx.createGain();
    engine.out.connect(engine.ctx.destination);
  }
  await engine.ctx.resume();
  await syncEngine();
}

async function loadModule(context, name) {
  await context.audioWorklet.addModule(`/tala/api/engine.js?name=${name}`);
}

// One at a time, in order: a sync that gave way to a later one would
// leave whoever awaited it (Play, a key) with no engine yet.  Each brings
// the engine to the song as it is when its turn comes.
function syncEngine() {
  engine.syncing = (engine.syncing || Promise.resolve())
    .then(syncOnce)
    .catch((e) => status(`The audio engine did not start: ${e.message}`));
  return engine.syncing;
}

async function syncOnce() {
  if (!engine.ctx) return;
  const song = T.song;
  if (song.processor === engine.processor && engine.node) {
    engine.node.port.postMessage({ t: 'params', params: engineParams(song) });
    engine.node.port.postMessage({ t: 'score', score: song.score });
    return;
  }
  // a new structure is a new program: the old one stops, the new one
  // takes up at the same place
  if (!engine.loaded.has(song.processor)) {
    await loadModule(engine.ctx, song.processor);
    engine.loaded.add(song.processor);
  }
  const node = new AudioWorkletNode(engine.ctx, song.processor, {
    numberOfInputs: 1, channelCount: 2, channelCountMode: 'explicit', outputChannelCount: [2],
    processorOptions: { params: engineParams(song), score: song.score, pool: T.pool, playing: T.playing, beat: T.beat },
  });
  node.port.onmessage = (e) => {
    if (node === engine.node) heardFromEngine(e.data);
  };
  node.connect(engine.out);
  if (engine.node) {
    engine.node.port.onmessage = null;
    engine.node.disconnect();
  }
  engine.node = node;
  engine.processor = song.processor;
  // the input follows the engine to its new program
  connectInput();
  if (T.monitor) node.port.postMessage({ t: 'monitor', on: true });
}

// Audio input: one device for the page, a channel of it for each armed
// audio track.  Browsers show device names only once one has been allowed.
T.input = { stream: null, source: null, deviceId: null, devices: [], latency: 0 };
T.inputChannel = {};
T.monitor = false;

T.openInput = async function openInput(deviceId) {
  await startAudio();
  if (T.input.stream) T.input.stream.getTracks().forEach((t) => t.stop());
  const audio = { echoCancellation: false, noiseSuppression: false, autoGainControl: false, channelCount: 2 };
  if (deviceId) audio.deviceId = { exact: deviceId };
  const stream = await navigator.mediaDevices.getUserMedia({ audio });
  const settings = stream.getAudioTracks()[0].getSettings();
  T.input.stream = stream;
  T.input.deviceId = settings.deviceId;
  T.input.latency = settings.latency || 0;
  T.input.devices = (await navigator.mediaDevices.enumerateDevices()).filter((d) => d.kind === 'audioinput');
  if (T.input.source) T.input.source.disconnect();
  T.input.source = engine.ctx.createMediaStreamSource(stream);
  connectInput();
  renderDetail();
};

function connectInput() {
  if (!T.input.source || !engine.node) return;
  try { T.input.source.disconnect(); } catch (e) { /* not yet connected */ }
  T.input.source.connect(engine.node);
}

T.setMonitor = function setMonitor(on) {
  T.monitor = on;
  if (engine.node) engine.node.port.postMessage({ t: 'monitor', on });
};

function heardFromEngine(message) {
  if (message.rec) { takeNote(message); return; }
  if (message.cap) { takeAudio(message); return; }
  if (T.playing) T.beat = message.beat;
  T.peaks = message.peaks;
  T.clock = message.clock;
  T.sessionState = message.session;
}

T.play = async function play(from = T.startBeat) {
  await startAudio();
  T.beat = from;
  T.playing = true;
  engine.node.port.postMessage({ t: 'play', beat: from });
  renderTransport();
};

T.stop = function stop() {
  if (T.recording) finishTake();
  T.playing = false;
  if (engine.node) engine.node.port.postMessage({ t: 'stop' });
  T.beat = T.startBeat;
  renderTransport();
};

// The metronome is the page's, not the song's: the song's parameters
// always say off, and every set of them sent to the engine is patched.
T.metronome = false;

function engineParams(song) {
  const params = song.params.slice();
  if (T.metronome) params[song.metronomeParam] = 1;
  return params;
}

T.toggleMetronome = function toggleMetronome() {
  T.metronome = !T.metronome;
  if (engine.node) engine.node.port.postMessage({ t: 'param', i: T.song.metronomeParam, v: T.metronome ? 1 : 0 });
  renderTransport();
};

// Recording.  The engine says at which beat each played note sounded,
// so a take lands where it was heard, not where the page's clock thought
// it was; a take becomes a clip when recording stops.
T.armed = new Set();
T.recording = false;
let take = null;

const playable = (track) => track && track.instrument && track.instrument.kind !== 'sampler';
const audible = (track) => track && track.instrument && track.instrument.kind === 'sampler';
const armedTracks = () => T.song.tracks.filter((t) => T.armed.has(t.id) && t.instrument);

T.record = async function record() {
  if (T.recording) { await finishTake(); renderTransport(); renderArrangement(); return; }
  if (!armedTracks().length) {
    const selected = T.song.tracks.find((t) => t.id === T.trackId);
    if (!selected || !selected.instrument) {
      status('Arm a track first: its ● button.');
      return;
    }
    T.armed.add(selected.id);
  }
  const audioTracks = armedTracks().filter(audible);
  if (audioTracks.length && !T.input.source) {
    try { await T.openInput(T.input.deviceId); } catch (e) {
      status(`No input to record from: ${e.message}`);
      return;
    }
  }
  take = { held: new Map(), notes: new Map(), audio: audioTracks.length ? { chunks: [], tracks: audioTracks.map((t) => t.id) } : null };
  T.recording = true;
  if (!T.playing) await T.play();
  if (take.audio) engine.node.port.postMessage({ t: 'capture', on: true });
  renderTransport();
  renderArrangement();
};

// The input's batches, as the engine sends them; the last says so.
let captureDone = null;

function takeAudio(m) {
  if (take && take.audio) take.audio.chunks.push(m);
  else if (finishing) finishing.chunks.push(m);
  if (m.last && captureDone) { captureDone(); captureDone = null; }
}

let finishing = null;

// A player plays to what they hear, and they hear each beat the output's
// latency after the engine reached it: a take is moved earlier by that
// much, as a recording studio's software compensates.
function heardBeat(beat) {
  const ctx = engine.ctx;
  const latency = ctx ? (ctx.outputLatency || 0) + (ctx.baseLatency || 0) : 0;
  return Math.max(0, beat - latency * T.song.tempo / 60);
}

function takeNote(m) {
  if (!take) return;
  const key = `${m.track}:${m.pitch}`;
  if (m.rec === 'on') {
    take.held.set(key, { start: heardBeat(m.beat), vel: m.vel });
    return;
  }
  const held = take.held.get(key);
  if (!held) return;
  take.held.delete(key);
  keepNote(m.track, m.pitch, held, heardBeat(m.beat));
}

function keepNote(track, pitch, held, end) {
  const song = T.song;
  // a note held across the loop's wrap ends after its start
  const length = end >= held.start ? end - held.start
    : (song.loopEnd - held.start) + (end - song.loopStart);
  if (!take.notes.has(track)) take.notes.set(track, []);
  take.notes.get(track).push([pitch, +held.start.toFixed(4), Math.max(0.01, +length.toFixed(4)), held.vel]);
}

async function finishTake() {
  if (!take) return;
  for (const [key, held] of take.held) {
    const [track, pitch] = key.split(':').map(Number);
    keepNote(track, pitch, held, T.beat);
  }
  const done = take;
  take = null;
  T.recording = false;
  for (const [track, notes] of done.notes) {
    const name = T.song.tracks[track] && T.song.tracks[track].name;
    if (name && notes.length) T.send(form('record-notes', name, notes));
  }
  if (done.audio && engine.node) {
    // the engine's last batch comes after this; wait for it
    finishing = done.audio;
    const last = new Promise((resolve) => { captureDone = resolve; });
    engine.node.port.postMessage({ t: 'capture', on: false });
    await Promise.race([last, new Promise((r) => setTimeout(r, 2000))]);
    finishing = null;
    await keepAudio(done.audio);
  }
}

let takes = 0;

// A recorded take: the captured frames, split where the loop wrapped, the
// last pass kept, moved earlier by what the player heard late, uploaded as
// a sample and put on each armed audio track.
async function keepAudio(audio) {
  const chunks = audio.chunks;
  const total = chunks.reduce((n, c) => n + c.left.length, 0);
  if (!total) return;
  const left = new Float32Array(total);
  const right = new Float32Array(total);
  const beats = [];
  let at = 0;
  for (const c of chunks) {
    left.set(c.left, at);
    right.set(c.right, at);
    at += c.left.length;
    for (const b of c.beats) beats.push(b);
  }
  const song = T.song;
  const Q = 128;
  const bpf = song.tempo / 60 / 48000;
  // where each pass begins: the frame at which the loop's end was reached
  let from = 0;
  let start = beats[0];
  for (let q = 0; q + 1 < beats.length; q++) {
    if (beats[q + 1] < beats[q] - 1e-9) {
      from = q * Q + Math.min(Q, Math.ceil((song.loopEnd - beats[q]) / bpf));
      start = song.loopStart;
    }
  }
  const ctx = engine.ctx;
  const late = ((ctx.outputLatency || 0) + (ctx.baseLatency || 0) + (T.input.latency || 0)) * song.tempo / 60;
  let beat = start - late;
  let offset = 0;
  if (beat < 0) { offset = -beat * 60 / song.tempo; beat = 0; }
  takes += 1;
  for (const id of audio.tracks) {
    const track = T.song.tracks.find((t) => t.id === id);
    if (!track) continue;
    const which = T.inputChannel[id] || 'left';
    const frames = total - from;
    const channels = which === 'stereo' ? 2 : 1;
    const data = new Float32Array(frames * channels);
    if (which === 'right') data.set(right.subarray(from));
    else data.set(left.subarray(from));
    if (channels === 2) data.set(right.subarray(from), frames);
    try {
      const name = await T.uploadSample(`${track.name} take ${takes}`, channels, data);
      // played at the song's tempo, and in place of what it covers
      await T.send(form('add-audio-clip', track.name, name, kw('start'), +beat.toFixed(4), kw('offset'), +offset.toFixed(4),
                        kw('bpm'), song.tempo, kw('replace'), true));
    } catch (e) {
      status(`The take could not be kept: ${e.message}`);
    }
  }
}

// Notes played live go to the armed tracks, or with none armed to the
// selected one; an armed track records them while recording.
function liveTargets() {
  const tracks = T.song.tracks;
  const armed = tracks.filter((t) => T.armed.has(t.id) && playable(t));
  if (armed.length) return armed.map((t) => tracks.indexOf(t));
  const i = tracks.findIndex((t) => t.id === T.trackId);
  return i >= 0 && playable(tracks[i]) ? [i] : [];
}

const sounding = new Map();

T.locate = function locate(beat) {
  T.startBeat = Math.max(0, beat);
  if (T.playing) {
    T.beat = T.startBeat;
    engine.node.port.postMessage({ t: 'seek', beat: T.startBeat });
  } else {
    T.beat = T.startBeat;
  }
  renderAll();
};

// Notes played live, from the keys, a MIDI device or the piano roll.
T.noteOn = async function noteOn(pitch, velocity = 100) {
  const targets = liveTargets();
  if (!targets.length) return;
  await startAudio();
  sounding.set(pitch, targets);
  for (const track of targets) {
    const rec = T.recording && T.armed.has(T.song.tracks[track].id);
    engine.node.port.postMessage({ t: 'note', track, pitch, vel: velocity, rec });
  }
};

T.noteOff = function noteOff(pitch) {
  const targets = sounding.get(pitch) || liveTargets();
  sounding.delete(pitch);
  if (!engine.node) return;
  for (const track of targets) {
    engine.node.port.postMessage({ t: 'noteoff', track, pitch, rec: T.recording });
  }
};

// The song kept in this browser too, so a restarted server loses nothing.
let saveTimer = null;

function saveLocal() {
  clearTimeout(saveTimer);
  saveTimer = setTimeout(async () => {
    try {
      const text = await (await fetch(`/tala/api/save?s=${encodeURIComponent(T.session)}`)).text();
      localStorage.setItem('tala-song', text);
    } catch (e) { /* a private window keeps nothing */ }
  }, 1500);
}

// The info box: what the thing under the pointer does.
document.addEventListener('mouseover', (e) => {
  const target = e.target.closest('[data-doc]');
  if (target) {
    $('#info').replaceChildren(el('b', {}, target.dataset.title || ''), el('p', {}, target.dataset.doc));
  }
});

function renderTransport() {
  const song = T.song;
  const box = $('#transport');
  box.replaceChildren(
    el('button', { id: 'play', class: T.playing ? 'on' : '', 'data-ctl': 'play', 'data-title': 'Play',
                   'data-doc': 'Play from the marker, or stop. Space does the same.',
                   onclick: () => (T.playing ? T.stop() : T.play()) }, T.playing ? '■ Stop' : '▶ Play'),
    el('button', { class: song.loopOn ? 'on' : '', 'data-ctl': 'loop', 'data-title': 'Loop',
                   'data-doc': 'Repeat the stretch under the bracket in the ruler. L does the same.',
                   onclick: () => T.send(form('set-loop', song.loopStart, song.loopEnd, kw('on'), !song.loopOn)) }, 'Loop'),
    el('button', { id: 'record', class: T.recording ? 'on rec' : 'rec', 'data-ctl': 'record', 'data-title': 'Record',
                   'data-doc': 'Record what the armed tracks are played, from a MIDI keyboard or the computer keys (M), into a new clip. F9 does the same; press again or Stop to keep the take.',
                   onclick: () => T.record() }, '● Rec'),
    el('button', { class: T.metronome ? 'on' : '', 'data-ctl': 'metronome', 'data-title': 'Metronome',
                   'data-doc': 'A click on every beat while the song plays, higher on the first of the bar. It is not in an export.',
                   onclick: () => T.toggleMetronome() }, 'Click'),
    el('span', { class: 'readout', id: 'position' }, '1.1.1'),
    el('button', { id: 'back-to-arrangement', hidden: true, 'data-ctl': 'back-to-arrangement',
                   'data-title': 'Back to Arrangement',
                   'data-doc': 'Tracks playing session clips go back to the timeline, from where the song is now.',
                   onclick: () => T.backToArrangement() }, '↩ Arrangement'),
    el('input', { id: 'tempo', type: 'number', min: 20, max: 300, step: 0.1, value: song.tempo,
                  'data-ctl': 'tempo', 'data-title': 'Tempo', 'data-doc': 'Beats a minute.',
                  onchange: (e) => T.send(form('set-tempo', +e.target.value)) }),
    el('span', { class: 'dim' }, 'BPM'),
    el('select', { 'data-ctl': 'time-signature', 'data-title': 'Time signature', 'data-doc': 'Beats in a bar over the beat note.',
                   onchange: (e) => { const [n, d] = e.target.value.split('/'); T.send(form('set-time-signature', +n, +d)); } },
       ['4/4', '3/4', '6/8', '7/8', '5/4'].map((v) =>
         el('option', { value: v, selected: v === `${song.numerator}/${song.denominator}` }, v))),
  );
}

function positionText(beat) {
  const per = beatsPerBar();
  const bar = Math.floor(beat / per) + 1;
  const within = beat - (bar - 1) * per;
  const b = Math.floor(within) + 1;
  const sixteenth = Math.floor((within - Math.floor(within)) * 4) + 1;
  return `${bar}.${b}.${sixteenth}`;
}
