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

async function syncEngine() {
  if (!engine.ctx) return;
  const song = T.song;
  if (song.processor === engine.processor && engine.node) {
    engine.node.port.postMessage({ t: 'params', params: song.params });
    engine.node.port.postMessage({ t: 'score', score: song.score });
    return;
  }
  // a new structure is a new program: the old one stops, the new one
  // takes up at the same place
  const ticket = ++engine.ticket;
  if (!engine.loaded.has(song.processor)) {
    await loadModule(engine.ctx, song.processor);
    engine.loaded.add(song.processor);
  }
  if (ticket !== engine.ticket) return;
  const node = new AudioWorkletNode(engine.ctx, song.processor, {
    numberOfInputs: 0, outputChannelCount: [2],
    processorOptions: { params: song.params, score: song.score, pool: T.pool, playing: T.playing, beat: T.beat },
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
}

function heardFromEngine(message) {
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
  T.playing = false;
  if (engine.node) engine.node.port.postMessage({ t: 'stop' });
  T.beat = T.startBeat;
  renderTransport();
};

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

// Notes played live: on the selected track, from the keys or a device.
T.noteOn = async function noteOn(pitch, velocity = 100) {
  const index = T.song.tracks.findIndex((t) => t.id === T.trackId);
  if (index < 0) return;
  await startAudio();
  engine.node.port.postMessage({ t: 'note', track: index, pitch, vel: velocity });
};

T.noteOff = function noteOff(pitch) {
  const index = T.song.tracks.findIndex((t) => t.id === T.trackId);
  if (index >= 0 && engine.node) engine.node.port.postMessage({ t: 'noteoff', track: index, pitch });
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
