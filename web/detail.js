// Copyright © 2026 Genworks International
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.  Distributed WITHOUT
// ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

// The bottom of the page: a track's device chain, or a clip's piano
// roll.  Every control comes from the table the server keeps, the same
// one the prompt's model reads, with its words on the info box.

'use strict';

// A control's value on a 0-1 scale, and back.
function norm(param, value) {
  if (param.log) return Math.log(value / param.min) / Math.log(param.max / param.min);
  return (value - param.min) / (param.max - param.min);
}

function denorm(param, x) {
  const clipped = Math.max(0, Math.min(1, x));
  if (param.log) return param.min * Math.pow(param.max / param.min, clipped);
  return param.min + clipped * (param.max - param.min);
}

const WHOLE = /octave|semitones|voices/;

function formatValue(param, value) {
  if (param.unit === 'Hz' && value >= 1000) return `${(value / 1000).toFixed(2)} kHz`;
  const size = Math.abs(value);
  const text = size >= 100 ? value.toFixed(0) : size >= 10 ? value.toFixed(1)
    : size > 0 && size < 0.1 ? value.toFixed(3) : value.toFixed(2);
  const unit = param.unit === 'semitones' ? 'st' : param.unit === 'octaves' ? 'oct' : param.unit;
  return `${text}${unit ? ' ' + unit : ''}`;
}

function arcPath(x) {
  const start = Math.PI * 0.75;
  const end = start + x * Math.PI * 1.5;
  const r = 13;
  const p = (a) => `${16 + r * Math.cos(a)} ${16 + r * Math.sin(a)}`;
  return `M ${p(start)} A ${r} ${r} 0 ${x > 2 / 3 ? 1 : 0} 1 ${p(end)}`;
}

function knob(param, value, ctl, onLive, onCommit, automated) {
  const arc = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  arc.setAttribute('fill', 'none');
  arc.setAttribute('stroke', '#f0a63c');
  arc.setAttribute('stroke-width', '3');
  const track = document.createElementNS(arc.namespaceURI, 'path');
  track.setAttribute('fill', 'none');
  track.setAttribute('stroke', '#3a3e4a');
  track.setAttribute('stroke-width', '3');
  track.setAttribute('d', arcPath(1));
  const svg = document.createElementNS(arc.namespaceURI, 'svg');
  svg.setAttribute('viewBox', '0 0 32 32');
  svg.append(track, arc);
  const text = el('div', { class: 'value' });
  let current = value;
  const show = () => {
    const x = Math.max(0.0001, Math.min(0.9999, norm(param, current)));
    arc.setAttribute('d', arcPath(x));
    text.textContent = formatValue(param, current);
  };
  show();
  const set = (v) => {
    current = WHOLE.test(param.key) ? Math.round(v) : v;
    show();
    onLive(current);
  };
  const node = el('div', {
    class: 'knob' + (automated ? ' automated' : ''), 'data-ctl': ctl,
    'data-title': param.label, 'data-doc': param.doc + (param.unit ? ` (${param.unit})` : ''),
    onpointerdown: (e) => {
      const x0 = norm(param, current);
      const fine = e.shiftKey ? 0.001 : 0.005;
      dragging(e, (dx, dy) => set(denorm(param, x0 - dy * fine)), () => onCommit(current));
    },
    ondblclick: () => { set(param.default); onCommit(current); },
    onwheel: (e) => {
      e.preventDefault();
      set(denorm(param, norm(param, current) - Math.sign(e.deltaY) * 0.01));
      clearTimeout(node.timer);
      node.timer = setTimeout(() => onCommit(current), 300);
    },
  }, svg, el('div', { class: 'label' }, param.label), text);
  return node;
}

function choiceControl(param, index, ctl, onCommit) {
  return el('label', { class: 'choice', 'data-ctl': ctl, 'data-title': param.label, 'data-doc': param.doc },
    el('span', { class: 'label' }, param.label),
    el('select', { onchange: (e) => onCommit(param.choices[+e.target.value]) },
      param.choices.map((c, i) => el('option', { value: i, selected: i === Math.round(index) }, c))));
}

// How the synth's many controls are grouped on its panel.
const SYNTH_GROUPS = [
  ['Oscillator 1', ['osc1-wave', 'osc1-octave', 'osc1-level', 'fm']],
  ['Oscillator 2', ['osc2-wave', 'osc2-octave', 'osc2-semitones', 'osc2-detune', 'osc2-level']],
  ['Sub, noise', ['sub-level', 'noise-level', 'drive']],
  ['Filter', ['filter-type', 'slope', 'cutoff', 'resonance', 'filter-env', 'key-track']],
  ['Filter envelope', ['filter-attack', 'filter-decay', 'filter-sustain', 'filter-release']],
  ['Amp envelope', ['attack', 'decay', 'sustain', 'release']],
  ['LFO', ['lfo-wave', 'lfo-rate', 'lfo-sync', 'lfo-pitch', 'lfo-cutoff', 'lfo-level']],
  ['Voice', ['voices', 'glide', 'velocity', 'level']],
];

function deviceLabel(device) {
  return kindOf(device.kind).label;
}

function controlFor(track, device, param, padName) {
  const index = device.param + kindOf(device.kind).params.indexOf(param);
  const value = T.song.params[index];
  const label = deviceLabel(device);
  const ctl = `device:${track.name}:${label}:${param.key}`;
  const ref = deviceRef(track, device);
  const commit = (v) => T.send(`(set-device ${lit(track.name)} ${lit(ref)} :${param.key} ${
    typeof v === 'string' ? ':' + v : lit(v)})`);
  if (param.choices.length) return choiceControl(param, value, ctl, commit);
  const automated = track.automation.some((l) => l.device === device.id && l.param === param.key);
  // on a pad's panel the pad's name need not be on every knob
  const shown = padName ? { ...param, label: param.label.slice(padName.length + 1) } : param;
  return knob(shown, value, ctl, (v) => setLive(index, v), commit, automated);
}

function group(title, controls) {
  return el('div', { class: 'control-group' }, title ? el('h4', {}, title) : null,
    el('div', { class: 'control-rows' }, controls));
}

function drumBody(track, device) {
  const kind = kindOf(device.kind);
  const pad = T.drumPad || 1;
  const names = kind.params.filter((p) => p.key.endsWith('-tune')).map((p) => p.label.replace(/ tune$/, ''));
  const prefix = `pad${pad}-`;
  const params = kind.params.filter((p) => p.key.startsWith(prefix));
  return [
    el('div', { class: 'pads' }, names.map((name, i) => el('button', {
      class: i + 1 === pad ? 'on' : '',
      'data-title': name, 'data-doc': `Pad ${i + 1}, played by pitch ${36 + i}.`,
      onclick: () => { T.drumPad = i + 1; renderDetail(); },
      onpointerdown: () => T.noteOn(36 + i),
      onpointerup: () => T.noteOff(36 + i),
    }, name))),
    group(names[pad - 1], params.map((p) => controlFor(track, device, p, names[pad - 1]))),
  ];
}

function deviceBody(track, device) {
  const kind = kindOf(device.kind);
  if (device.kind === 'drums') return drumBody(track, device);
  if (device.kind === 'synth') {
    return SYNTH_GROUPS.map(([title, keys]) => group(title, keys.map((key) =>
      controlFor(track, device, kind.params.find((p) => p.key === key)))));
  }
  const extra = device.kind === 'compressor' ? sidechainChoice(track, device) : null;
  return [extra, group(null, kind.params.map((p) => controlFor(track, device, p)))];
}

function sidechainChoice(track, device) {
  const ref = track.devices.indexOf(device);
  const others = T.song.tracks.filter((t) => t !== track);
  return el('label', { class: 'choice', 'data-title': 'Sidechain',
                       'data-doc': 'Listen to another track, so this one ducks when that one plays: the pumping of a bass under a kick.' },
    el('span', { class: 'label' }, 'Sidechain'),
    el('select', { onchange: (e) => T.send(form('set-sidechain', track.name, ref, e.target.value || null)) },
      el('option', { value: '' }, 'Own signal'),
      others.map((t) => el('option', { value: t.name, selected: T.song.tracks.indexOf(t) === device.key }, t.name))));
}

function deviceCard(track, device) {
  const isInstrument = device === track.instrument;
  const label = deviceLabel(device);
  const ref = deviceRef(track, device);
  const index = track.devices.indexOf(device);
  return el('div', {
    class: 'device' + (device.on ? '' : ' off'), 'data-ctl': `device:${track.name}:${label}`,
  },
  el('div', { class: 'device-head', 'data-title': label, 'data-doc': kindOf(device.kind).doc },
    isInstrument ? null : el('button', {
      class: device.on ? 'on' : '', 'data-title': 'Device on', 'data-doc': 'Turn this device off to hear the track without it.',
      onclick: () => T.send(form('set-device', track.name, ref, kw('on'), !device.on)),
    }, '⏻'),
    el('span', { class: 'name' }, label),
    isInstrument ? null : el('button', {
      disabled: index === 0, onclick: () => T.send(form('move-device', track.name, ref, index - 1)),
    }, '◀'),
    isInstrument ? null : el('button', {
      disabled: index === track.devices.length - 1, onclick: () => T.send(form('move-device', track.name, ref, index + 1)),
    }, '▶'),
    el('button', { 'data-title': 'Reset', 'data-doc': 'Every control back to its default.',
                   onclick: () => T.send(form('reset-device', track.name, ref)) }, '↺'),
    isInstrument ? null : el('button', { onclick: () => T.send(form('delete-device', track.name, ref)) }, '✕')),
  el('div', { class: 'device-body' }, deviceBody(track, device)));
}

function renderDevices(track) {
  const devices = [track.instrument, ...track.devices].filter(Boolean);
  const body = $('#detail-body');
  const left = body.scrollLeft;
  body.replaceChildren(el('div', { class: 'chain' },
    devices.map((d) => deviceCard(track, d)),
    el('div', { class: 'hint' }, track.devices.length ? '' : 'Click an effect in the browser to add it here.')));
  body.scrollLeft = left;
}

function renderDetail() {
  const box = $('#detail');
  const track = selectedTrack();
  if (!track) return;
  const clip = selectedClip();
  const notes = T.detail === 'notes' && clip;
  const tabs = el('div', { class: 'track-buttons' },
    el('button', { class: notes ? '' : 'on', onclick: () => { T.detail = 'devices'; renderDetail(); } }, 'Devices'),
    el('button', { class: notes ? 'on' : '', disabled: !clip, 'data-ctl': 'notes',
                   onclick: () => { T.detail = 'notes'; renderDetail(); } }, 'Notes'));
  const bar = el('div', { id: 'detail-bar' }, tabs, el('span', { class: 'title' }, track.name));
  const field = (label, value, key, doc, step = 0.25) => el('label', { 'data-title': label, 'data-doc': doc }, label,
    el('input', { type: 'number', step, value,
                  onchange: (e) => T.send(form('set-clip', track.name, clip.id, kw(key), +e.target.value)) }));
  if (notes && clip.sample) {
    bar.append(
      field('Length', clip.length, 'length', 'How long the clip runs on the timeline, in beats.'),
      field('Start in sample', clip.offset, 'offset', 'Seconds into the sample where the clip begins.', 0.01),
      field('Gain', clip.gain, 'gain', 'This clip\'s level, in dB.', 0.5),
      field('Transpose', clip.transpose, 'transpose', 'Semitones. Pitch and speed change together: down is slower and deeper.', 1));
    box.replaceChildren(bar, el('div', { id: 'detail-body' }));
    renderAudioClip(clip);
    return;
  }
  if (notes) {
    bar.append(
      field('Length', clip.length, 'length', 'How long the clip runs on the timeline, in beats.'),
      field('Repeat', clip.loop || clip.length, 'loop', 'The notes repeat every this many beats.'),
      el('span', { class: 'turn-note' }, 'Double-click: add or remove. Drag: move. Right edge: length. Alt-drag: velocity.'));
  }
  box.replaceChildren(bar, el('div', { id: 'detail-body' }));
  if (notes) renderRoll(track, clip);
  else renderDevices(track);
}

// An audio clip's whole sample, the part the clip plays lit, with a
// handle at each end of it: the left one cuts or brings back the front,
// the right one the end.
function renderAudioClip(clip) {
  const body = $('#detail-body');
  const track = selectedTrack();
  const sample = T.song.samples.find((s) => s.id === clip.sample);
  const width = Math.max(300, body.clientWidth - 16);
  const height = Math.max(90, body.clientHeight - 58);
  const canvas = el('canvas', { width, height, class: 'sample-view' });
  const info = el('div', { class: 'hint' },
    sample ? `${sample.name}: ${sample.seconds.toFixed(2)} s, ${sample.channels === 2 ? 'stereo' : 'mono'}` : 'The sample is gone.',
    sample && sample.missing ? ' — its audio is not on this server; import it again.' : '',
    sample && sample.words ? ` — "${sample.words}"` : '',
    sample ? ' — drag the bright part\'s edges to choose what the clip plays.' : '');
  body.replaceChildren(info, canvas);
  if (!sample || sample.missing || T.pool.length < sample.left + sample.frames) return;
  const rate = Math.pow(2, clip.transpose / 12);
  const secondsPerBeat = 60 / T.song.tempo;
  const x = (seconds) => seconds / sample.seconds * width;
  let from = clip.offset;
  let to = Math.min(sample.seconds, clip.offset + clip.length * secondsPerBeat * rate);
  const peaks = new Float32Array(width);
  for (let px = 0; px < width; px++) {
    const a = sample.left + Math.floor(px / width * sample.frames);
    const b = sample.left + Math.floor((px + 1) / width * sample.frames);
    let peak = 0;
    for (let i = a; i < b; i++) peak = Math.max(peak, Math.abs(T.pool[i]));
    peaks[px] = peak;
  }
  const draw = () => {
    const g = canvas.getContext('2d');
    g.clearRect(0, 0, width, height);
    const middle = height / 2;
    for (let px = 0; px < width; px++) {
      const inside = px >= x(from) && px <= x(to);
      g.fillStyle = inside ? '#38c3b4' : '#3a3e4a';
      const h = Math.min(1, peaks[px]) * middle;
      g.fillRect(px, middle - h, 1, Math.max(1, h * 2));
    }
    g.fillStyle = '#f0a63c';
    g.fillRect(x(from) - 1, 0, 3, height);
    g.fillRect(x(to) - 1, 0, 3, height);
  };
  draw();
  canvas.addEventListener('pointerdown', (e) => {
    const box = canvas.getBoundingClientRect();
    const px = e.clientX - box.left;
    const which = Math.abs(px - x(from)) < 10 ? 'from' : Math.abs(px - x(to)) < 10 ? 'to' : null;
    if (!which) return;
    const from0 = from;
    const to0 = to;
    const minimum = 0.0625 * secondsPerBeat * rate;
    dragging(e, (dx) => {
      const seconds = dx / width * sample.seconds;
      if (which === 'from') from = Math.max(0, Math.min(to0 - minimum, from0 + seconds));
      else to = Math.max(from0 + minimum, Math.min(sample.seconds, to0 + seconds));
      draw();
    }, () => {
      if (which === 'from' && from !== from0) {
        T.send(form('trim-clip-start', track.name, clip.id, (from - from0) / secondsPerBeat / rate));
      } else if (which === 'to' && to !== to0) {
        T.send(form('set-clip', track.name, clip.id, kw('length'), (to - from0) / secondsPerBeat / rate));
      }
    });
  });
  canvas.addEventListener('pointermove', (e) => {
    const px = e.clientX - canvas.getBoundingClientRect().left;
    canvas.style.cursor = Math.abs(px - x(from)) < 10 || Math.abs(px - x(to)) < 10 ? 'ew-resize' : 'default';
  });
}

// The piano roll.

const KEY_WIDTH = 46;
const NOTE_NAMES = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'];
const pitchName = (p) => `${NOTE_NAMES[p % 12]}${Math.floor(p / 12) - 2}`;

function renderRoll(track, clip) {
  const drums = track.instrument && track.instrument.kind === 'drums';
  const low = drums ? 36 : 24;
  const high = drums ? 43 : 96;
  const row = drums ? 22 : 11;
  const span = clip.loop || clip.length;
  const body = $('#detail-body');
  const beat = Math.max(28, Math.min(160, (body.clientWidth - KEY_WIDTH - 20) / span));
  const width = KEY_WIDTH + span * beat;
  const height = (high - low + 1) * row;
  const canvas = el('canvas', { id: 'roll', width, height });
  const wrap = el('div', { class: 'roll-wrap' }, canvas);
  body.replaceChildren(wrap);
  const notes = clip.notes.map((n) => [...n]);
  const kit = drums ? kindOf('drums').params.filter((p) => p.key.endsWith('-tune')).map((p) => p.label.replace(/ tune$/, '')) : null;
  const color = COLORS[track.color % 12];
  let selected = null;
  const yOf = (pitch) => (high - pitch) * row;
  const pitchAt = (y) => Math.max(low, Math.min(high, high - Math.floor(y / row)));
  const beatAt = (x) => (x - KEY_WIDTH) / beat;
  const grid = 0.25;
  const commit = () => T.send(form('set-notes', track.name, clip.id, notes));
  const draw = () => {
    const g = canvas.getContext('2d');
    g.fillStyle = '#1b1c21';
    g.fillRect(0, 0, width, height);
    for (let p = low; p <= high; p++) {
      const black = [1, 3, 6, 8, 10].includes(p % 12) && !drums;
      g.fillStyle = black ? '#17181c' : '#1f2127';
      g.fillRect(KEY_WIDTH, yOf(p), width, row - 1);
      g.fillStyle = black ? '#2a2d36' : '#4a4e5c';
      g.fillRect(0, yOf(p), KEY_WIDTH - 1, row - 1);
      if (drums || p % 12 === 0) {
        g.fillStyle = '#dfe2ea';
        g.font = '10px system-ui';
        g.fillText(drums ? kit[p - 36].slice(0, 8) : pitchName(p), 3, yOf(p) + row - 3);
      }
    }
    for (let b = 0; b <= span; b += grid) {
      const whole = Number.isInteger(b);
      g.fillStyle = whole ? '#3a3e4a' : '#26282f';
      g.fillRect(KEY_WIDTH + b * beat, 0, 1, height);
    }
    for (const note of notes) {
      const [pitch, start, length, velocity] = note;
      if (pitch < low || pitch > high) continue;
      g.globalAlpha = 0.45 + 0.55 * velocity / 127;
      g.fillStyle = note === selected ? '#dfe2ea' : color;
      g.fillRect(KEY_WIDTH + start * beat + 1, yOf(pitch) + 1, Math.max(3, length * beat - 2), row - 3);
      g.globalAlpha = 1;
    }
  };
  const noteAt = (x, y) => {
    const pitch = pitchAt(y);
    const b = beatAt(x);
    return notes.find((n) => n[0] === pitch && b >= n[1] && b <= n[1] + n[2]);
  };
  const local = (e) => {
    const box = canvas.getBoundingClientRect();
    return [e.clientX - box.left, e.clientY - box.top];
  };
  canvas.addEventListener('pointerdown', (e) => {
    const [x, y] = local(e);
    if (x < KEY_WIDTH) {
      const pitch = pitchAt(y);
      T.noteOn(pitch);
      window.addEventListener('pointerup', () => T.noteOff(pitch), { once: true });
      return;
    }
    const note = noteAt(x, y);
    if (!note) { selected = null; draw(); return; }
    selected = note;
    draw();
    const [p0, s0, l0, v0] = note;
    const edge = KEY_WIDTH + (s0 + l0) * beat - x < 6;
    dragging(e, (dx, dy, ev) => {
      if (ev.altKey) note[3] = Math.max(1, Math.min(127, Math.round(v0 - dy)));
      else if (edge) note[2] = Math.max(grid / 2, Math.round((l0 + dx / beat) / grid) * grid || grid / 2);
      else {
        note[0] = Math.max(low, Math.min(high, p0 - Math.round(dy / row)));
        note[1] = Math.max(0, Math.round((s0 + dx / beat) / grid) * grid);
      }
      draw();
    }, () => {
      if (note[0] !== p0 || note[1] !== s0 || note[2] !== l0 || note[3] !== v0) commit();
    });
  });
  canvas.addEventListener('dblclick', (e) => {
    const [x, y] = local(e);
    if (x < KEY_WIDTH) return;
    const note = noteAt(x, y);
    if (note) notes.splice(notes.indexOf(note), 1);
    else {
      const pitch = pitchAt(y);
      notes.push([pitch, Math.floor(beatAt(x) / grid) * grid, grid, 100]);
      T.noteOn(pitch);
      setTimeout(() => T.noteOff(pitch), 200);
    }
    commit();
  });
  draw();
  if (!drums) {
    const pitches = notes.map((n) => n[0]);
    const middle = pitches.length ? (Math.min(...pitches) + Math.max(...pitches)) / 2 : 60;
    wrap.scrollTop = yOf(middle) - wrap.clientHeight / 2;
  }
}
