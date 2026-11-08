// Copyright © 2026 Genworks International
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.  Distributed WITHOUT
// ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

// The arrangement: a ruler with the loop bracket, a row a track, clips
// on the lanes, and an automation lane under a track when it is open.
// A drag moves the element; the form goes when it lets go.

'use strict';

function songBeats() {
  const song = T.song;
  let last = song.loopEnd;
  for (const track of song.tracks) {
    for (const clip of track.clips) last = Math.max(last, clip.start + clip.length);
  }
  return Math.ceil((last + 32) / 4) * 4;
}

const snap = (beat, step = 1) => Math.max(0, Math.round(beat / step) * step);

function dragging(event, onMove, onUp) {
  event.preventDefault();
  event.stopPropagation();
  const x0 = event.clientX;
  const y0 = event.clientY;
  const move = (e) => onMove(e.clientX - x0, e.clientY - y0, e);
  const up = (e) => {
    window.removeEventListener('pointermove', move);
    window.removeEventListener('pointerup', up);
    if (onUp) onUp(e.clientX - x0, e.clientY - y0, e);
  };
  window.addEventListener('pointermove', move);
  window.addEventListener('pointerup', up);
}

function drawClipNotes(canvas, clip, color) {
  const notes = clip.notes;
  const g = canvas.getContext('2d');
  g.fillStyle = 'rgba(0,0,0,.65)';
  if (!notes.length) return;
  const pitches = notes.map((n) => n[0]);
  const low = Math.min(...pitches);
  const high = Math.max(...pitches);
  const span = Math.max(1, high - low);
  const repeat = clip.loop || clip.length;
  const h = canvas.height;
  for (let offset = 0; offset < clip.length; offset += repeat) {
    for (const [pitch, start, length] of notes) {
      if (start >= repeat || offset + start >= clip.length) continue;
      const y = (h - 3) * (1 - (pitch - low) / span);
      g.fillRect((offset + start) * T.ppb, y, Math.max(1, length * T.ppb - 1), 2);
    }
  }
}

// The peaks of an audio clip across a canvas.
function drawWaveform(canvas, clip, fill = 'rgba(0,0,0,.65)') {
  const sample = T.song.samples.find((s) => s.id === clip.sample);
  if (!sample || sample.missing || T.pool.length < sample.left + sample.frames) return;
  const g = canvas.getContext('2d');
  g.fillStyle = fill;
  const framesPerBeat = 48000 * clip.spb;
  const first = clip.offset * 48000;
  const shown = Math.min(sample.frames - first, clip.length * framesPerBeat);
  const width = Math.min(canvas.width, Math.ceil(shown / (clip.length * framesPerBeat) * canvas.width));
  const middle = canvas.height / 2;
  const step = Math.max(1, Math.floor(shown / width / 24));
  for (let x = 0; x < width; x++) {
    const from = sample.left + Math.floor(first + x / width * shown);
    const to = sample.left + Math.floor(first + (x + 1) / width * shown);
    let peak = 0;
    for (let i = from; i < to; i += step) peak = Math.max(peak, Math.abs(T.pool[i]));
    const h = Math.min(1, peak) * middle;
    g.fillRect(x, middle - h, 1, Math.max(1, h * 2));
  }
}

function clipElement(track, clip) {
  const color = COLORS[track.color % 12];
  const width = clip.length * T.ppb;
  const canvas = el('canvas', { width: Math.max(1, Math.min(4000, width)), height: 30 });
  if (clip.sample) drawWaveform(canvas, clip);
  else drawClipNotes(canvas, clip, color);
  const node = el('div', {
    class: 'clip' + (clip.id === T.clipId ? ' selected' : ''),
    'data-ctl': `clip:${clip.id}`,
    'data-title': clip.name,
    'data-doc': 'Drag to move, drag the right edge to resize, double-click to edit its notes. Delete removes it; Ctrl+D copies it after itself.',
    style: `left:${clip.start * T.ppb}px;width:${width}px;background:${color}`,
    ondblclick: (e) => {
      e.stopPropagation();
      T.trackId = track.id;
      T.clipId = clip.id;
      T.detail = 'notes';
      renderAll();
    },
  }, el('span', { class: 'clip-title' }, clip.name), canvas,
  el('div', { class: 'clip-edge left', 'data-title': 'Clip start',
              'data-doc': 'Drag to cut off or bring back the front of the clip; what it plays stays where it was.',
              onpointerdown: (e) => {
    selectClip(track, clip);
    dragging(e, (dx) => {
      const d = trimBeats(clip, dx);
      node.style.left = `${(clip.start + d) * T.ppb}px`;
      node.style.width = `${(clip.length - d) * T.ppb}px`;
    }, (dx) => {
      const d = trimBeats(clip, dx);
      if (d) T.send(form('trim-clip-start', track.name, clip.id, d));
    });
  } }),
  el('div', { class: 'clip-edge', 'data-title': 'Clip end', 'data-doc': 'Drag to make the clip longer or shorter.',
              onpointerdown: (e) => {
    selectClip(track, clip);
    const start = clip.length;
    const step = clip.sample ? 0.0625 : 0.25;
    dragging(e, (dx) => {
      node.style.width = `${Math.max(step, snap(start + dx / T.ppb, step)) * T.ppb}px`;
    }, (dx) => {
      const length = Math.max(step, snap(start + dx / T.ppb, step));
      if (length !== clip.length) T.send(form('set-clip', track.name, clip.id, kw('length'), length));
    });
  } }));
  node.addEventListener('pointerdown', (e) => {
    selectClip(track, clip);
    const start = clip.start;
    dragging(e, (dx) => {
      node.style.left = `${snap(start + dx / T.ppb) * T.ppb}px`;
    }, (dx) => {
      const at = snap(start + dx / T.ppb);
      if (at !== clip.start) T.send(form('set-clip', track.name, clip.id, kw('start'), at));
    });
  });
  return node;
}

// How far a drag of DX pixels moves a clip's start: on the grid, a
// sixteenth for audio and a quarter beat for notes, and never past the
// clip's end, the song's start or the front of its sample.
function trimBeats(clip, dx) {
  const step = clip.sample ? 0.0625 : 0.25;
  const wanted = Math.round(dx / T.ppb / step) * step;
  let least = -clip.start;
  if (clip.sample) {
    least = Math.max(least, -clip.offset / clip.spb);
  }
  return Math.max(least, Math.min(clip.length - step, wanted));
}

function selectClip(track, clip) {
  const changed = T.clipId !== clip.id || T.trackId !== track.id;
  T.trackId = track.id;
  T.clipId = clip.id;
  if (changed) {
    document.querySelectorAll('.clip.selected').forEach((n) => n.classList.remove('selected'));
    const node = document.querySelector(`[data-ctl="clip:${clip.id}"]`);
    if (node) node.classList.add('selected');
    renderSelection();
    renderDetail();
  }
}

function renderSelection() {
  document.querySelectorAll('.track-row[data-track]').forEach((row) => {
    row.classList.toggle('selected', +row.dataset.track === T.trackId);
  });
}

function trackButtons(track) {
  const toggle = (key, label, cls) => el('button', {
    class: `${cls}${track[key] ? ' on' : ''}`, 'data-ctl': `track:${track.name}:${key}`,
    'data-title': label === 'M' ? 'Mute' : 'Solo',
    'data-doc': label === 'M' ? 'Silence this track.' : 'Hear only this track, with the returns.',
    onclick: (e) => { e.stopPropagation(); T.send(form('set-track', track.name, kw(key), !track[key])); },
  }, label);
  const gain = el('input', {
    type: 'range', min: -60, max: 12, step: 0.1, value: track.gain,
    'data-ctl': `track:${track.name}:gain`, 'data-title': 'Track volume',
    'data-doc': 'The track\'s fader, in dB. Double-click for 0 dB.',
    oninput: (e) => setLive(track.param, +e.target.value),
    onchange: (e) => T.send(form('set-track', track.name, kw('gain'), +e.target.value)),
    ondblclick: () => T.send(form('set-track', track.name, kw('gain'), 0)),
    onclick: (e) => e.stopPropagation(),
  });
  return el('div', { class: 'track-buttons' },
    track.kind === 'master' ? null : toggle('mute', 'M', 'mute'),
    track.kind === 'midi' ? toggle('solo', 'S', 'solo') : null,
    track.kind === 'midi' && track.instrument ? el('button', {
      class: 'arm' + (T.armed.has(track.id) ? ' on' : ''), 'data-ctl': `track:${track.name}:arm`,
      'data-title': 'Arm', 'data-doc': audible(track)
        ? 'An armed audio track records its input (chosen on its device panel) when Rec is on.'
        : 'Armed tracks hear the keyboard and record what is played when Rec is on.',
      onclick: (e) => {
        e.stopPropagation();
        if (T.armed.has(track.id)) T.armed.delete(track.id); else T.armed.add(track.id);
        renderArrangement();
      },
    }, '●') : null,
    el('button', { class: T.autoOpen[track.id] ? 'on' : '', 'data-title': 'Automation',
                   'data-doc': 'Show a lane to draw how a control moves over time.',
                   onclick: (e) => { e.stopPropagation(); toggleLane(track); } }, 'A'),
    gain);
}

function toggleLane(track) {
  if (T.autoOpen[track.id]) delete T.autoOpen[track.id];
  else T.autoOpen[track.id] = { device: null, param: 'gain' };
  renderArrangement();
}

function trackHead(track) {
  return el('div', {
    class: 'track-head', 'data-ctl': `track:${track.name}`,
    onclick: () => { T.trackId = track.id; T.detail = 'devices'; renderSelection(); renderDetail(); },
    ondblclick: () => {
      if (track.kind === 'master') return;
      const name = window.prompt('Track name', track.name);
      if (name && name !== track.name) T.send(form('rename-track', track.name, name));
    },
  },
  el('div', { class: 'track-color', style: `background:${COLORS[track.color % 12]}` }),
  el('div', { class: 'track-name' }, track.name),
  el('div', { class: 'meter', 'data-meter': track.id }, el('i')),
  trackButtons(track));
}

function laneChoices(track) {
  const choices = [{ device: null, param: 'gain', label: 'Volume' }, { device: null, param: 'pan', label: 'Pan' }];
  for (const device of [track.instrument, ...track.devices]) {
    if (!device) continue;
    const kind = kindOf(device.kind);
    for (const p of kind.params) {
      if (!p.choices.length) choices.push({ device: device.id, param: p.key, label: `${kind.label}: ${p.label}` });
    }
  }
  return choices;
}

function laneRange(track, open) {
  if (open.device === null) {
    return open.param === 'gain' ? { min: -60, max: 12, log: false } : { min: -1, max: 1, log: false };
  }
  const device = [track.instrument, ...track.devices].find((d) => d && d.id === open.device);
  const param = device && kindOf(device.kind).params.find((p) => p.key === open.param);
  return param || { min: 0, max: 1, log: false };
}

function automationRow(track, width) {
  const open = T.autoOpen[track.id];
  const choices = laneChoices(track);
  const lane = track.automation.find((l) => l.device === open.device && l.param === open.param);
  const points = lane ? lane.points.map((p) => [...p]) : [];
  const range = laneRange(track, open);
  const height = 74;
  const toY = (v) => height * (1 - norm(range, v));
  const fromY = (y) => denorm(range, 1 - y / height);
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  svg.setAttribute('width', width);
  svg.setAttribute('height', height);
  const device = open.device === null ? null
    : [track.instrument, ...track.devices].find((d) => d && d.id === open.device);
  const commit = () => {
    const ref = device ? deviceRef(track, device) : null;
    T.send(points.length
      ? form('set-automation', track.name, ref, kw(open.param), points)
      : form('clear-automation', track.name, ref, kw(open.param)));
  };
  const draw = () => {
    svg.replaceChildren();
    if (points.length) {
      const line = document.createElementNS(svg.namespaceURI, 'polyline');
      const coords = points.map(([b, v]) => `${b * T.ppb},${toY(v)}`);
      coords.unshift(`0,${toY(points[0][1])}`);
      coords.push(`${width},${toY(points[points.length - 1][1])}`);
      line.setAttribute('points', coords.join(' '));
      line.setAttribute('fill', 'none');
      line.setAttribute('stroke', COLORS[track.color % 12]);
      line.setAttribute('stroke-width', '1.5');
      svg.append(line);
    }
    points.forEach((point, i) => {
      const dot = document.createElementNS(svg.namespaceURI, 'circle');
      dot.setAttribute('cx', point[0] * T.ppb);
      dot.setAttribute('cy', toY(point[1]));
      dot.setAttribute('r', 4);
      dot.setAttribute('fill', '#dfe2ea');
      dot.style.cursor = 'grab';
      dot.addEventListener('pointerdown', (e) => {
        const [b0, v0] = point;
        const y0 = toY(v0);
        dragging(e, (dx, dy) => {
          point[0] = snap(b0 + dx / T.ppb, 0.25);
          point[1] = fromY(Math.min(height, Math.max(0, y0 + dy)));
          draw();
        }, () => { points.sort((a, b) => a[0] - b[0]); commit(); });
      });
      dot.addEventListener('dblclick', (e) => {
        e.stopPropagation();
        points.splice(i, 1);
        commit();
      });
      svg.append(dot);
    });
  };
  draw();
  const laneNode = el('div', {
    class: 'lane auto-lane', style: `width:${width}px`,
    'data-title': 'Automation', 'data-doc': 'Double-click to add a point, drag to move it, double-click a point to remove it.',
    ondblclick: (e) => {
      if (e.target.tagName === 'circle') return;
      const box = laneNode.getBoundingClientRect();
      points.push([snap((e.clientX - box.left) / T.ppb, 0.25), fromY(e.clientY - box.top)]);
      points.sort((a, b) => a[0] - b[0]);
      commit();
    },
  }, svg);
  return el('div', { class: 'track-row auto-row' },
    el('div', { class: 'track-head' },
      el('select', {
        onchange: (e) => {
          const choice = choices[+e.target.value];
          T.autoOpen[track.id] = { device: choice.device, param: choice.param };
          renderArrangement();
        },
      }, choices.map((c, i) => el('option', {
        value: i, selected: c.device === open.device && c.param === open.param,
      }, `${c.label}${track.automation.some((l) => l.device === c.device && l.param === c.param) ? ' •' : ''}`)))),
    laneNode);
}

function rulerElement(width) {
  const song = T.song;
  const per = beatsPerBar();
  const marks = [];
  const every = T.ppb * per < 40 ? 4 : 1;
  for (let bar = 0; bar * per < width / T.ppb; bar += every) {
    marks.push(el('div', { class: 'bar-mark', style: `left:${bar * per * T.ppb}px` }, bar + 1));
  }
  const brace = el('div', {
    class: 'loop-brace' + (song.loopOn ? '' : ' off'),
    'data-title': 'Loop bracket', 'data-doc': 'Drag to move the loop, its ends to change it.',
    style: `left:${song.loopStart * T.ppb}px;width:${(song.loopEnd - song.loopStart) * T.ppb}px`,
  });
  const dragLoop = (e, moveStart, moveEnd) => {
    const { loopStart: s0, loopEnd: e0 } = song;
    const next = (dx) => {
      const d = dx / T.ppb;
      const start = moveStart ? snap(s0 + d) : s0;
      const end = moveEnd ? Math.max(start + 1, snap(e0 + d)) : e0;
      return [start, end];
    };
    dragging(e, (dx) => {
      const [start, end] = next(dx);
      brace.style.left = `${start * T.ppb}px`;
      brace.style.width = `${(end - start) * T.ppb}px`;
    }, (dx) => {
      const [start, end] = next(dx);
      T.send(form('set-loop', Math.min(start, end - 1), end, kw('on'), song.loopOn));
    });
  };
  brace.addEventListener('pointerdown', (e) => dragLoop(e, true, true));
  brace.append(
    el('div', { class: 'loop-edge left', onpointerdown: (e) => dragLoop(e, true, false) }),
    el('div', { class: 'loop-edge right', onpointerdown: (e) => dragLoop(e, false, true) }));
  return el('div', {
    class: 'ruler-track', style: `width:${width}px`,
    onpointerdown: (e) => {
      const box = e.currentTarget.getBoundingClientRect();
      T.locate(snap((e.clientX - box.left) / T.ppb));
    },
  }, marks, brace, el('div', { class: 'start-marker', style: `left:${T.startBeat * T.ppb}px` }));
}

function laneStyle(width) {
  const beat = T.ppb;
  const bar = beatsPerBar() * T.ppb;
  return `width:${width}px;background-image:` +
    `repeating-linear-gradient(to right, #34363f 0 1px, transparent 1px ${bar}px),` +
    `repeating-linear-gradient(to right, #24262c 0 1px, transparent 1px ${beat}px)`;
}

function trackRow(track, width) {
  const lane = el('div', {
    class: 'lane', style: laneStyle(width),
    ondragover: (e) => e.preventDefault(),
    ondrop: (e) => {
      e.preventDefault();
      e.stopPropagation();
      const box = lane.getBoundingClientRect();
      const at = snap((e.clientX - box.left) / T.ppb);
      for (const file of e.dataTransfer.files) T.importFile(file, track, at);
    },
    ondblclick: (e) => {
      if (track.kind !== 'midi' || e.target !== lane) return;
      if (track.instrument.kind === 'sampler') return;
      const box = lane.getBoundingClientRect();
      const per = beatsPerBar();
      const at = Math.floor((e.clientX - box.left) / T.ppb / per) * per;
      T.send(form('add-clip', track.name, kw('start'), at, kw('length'), per));
    },
    onpointerdown: (e) => {
      if (e.target !== lane) return;
      T.trackId = track.id;
      T.clipId = null;
      document.querySelectorAll('.clip.selected').forEach((n) => n.classList.remove('selected'));
      renderSelection();
      renderDetail();
    },
  }, track.clips.filter((clip) => clip.scene === null).map((clip) => clipElement(track, clip)));
  return el('div', { class: 'track-row', 'data-track': track.id }, trackHead(track), lane);
}

function renderArrangement() {
  const box = $('#arrangement');
  const left = box.scrollLeft;
  const top = box.scrollTop;
  const width = songBeats() * T.ppb;
  const song = T.song;
  const rows = [];
  for (const track of allTracks()) {
    rows.push(trackRow(track, width));
    if (T.autoOpen[track.id]) rows.push(automationRow(track, width));
  }
  const inner = el('div', { class: 'arr-inner', style: `width:calc(var(--head-width) + ${width}px)` },
    el('div', { class: 'ruler' },
      el('div', { class: 'ruler-corner' },
        el('button', { 'data-ctl': 'add-track', 'data-title': 'Add track',
                       'data-doc': 'A new track with a synth. Ctrl+T does the same.', onclick: () => addTrack(':synth') }, '+ Synth'),
        el('button', { 'data-title': 'Add drums', 'data-doc': 'A new track with a drum kit.', onclick: () => addTrack(':drums') }, '+ Drums'),
        el('button', { 'data-title': 'Add return', 'data-doc': 'A return track: effects every track can send to.', onclick: addReturn }, '+ Return')),
      rulerElement(width)),
    rows,
    song.tracks.length ? null : el('div', { class: 'empty-note' },
      'An empty song. Add a track above, or ask the prompt for a setup: "Set up a Goa trance groove at 145 BPM."'),
    el('div', { class: 'playhead', id: 'playhead' }));
  box.replaceChildren(inner);
  box.scrollLeft = left;
  box.scrollTop = top;
}

function uniqueName(base) {
  const names = allTracks().map((t) => t.name.toLowerCase());
  if (!names.includes(base.toLowerCase())) return base;
  for (let i = 2; ; i++) {
    if (!names.includes(`${base} ${i}`.toLowerCase())) return `${base} ${i}`;
  }
}

function addTrack(instrument) {
  const name = uniqueName(instrument === ':drums' ? 'Drums' : 'Synth');
  T.send(`(add-track ${lit(name)} :instrument ${instrument})`);
}

function addReturn() {
  T.send(form('add-return', uniqueName('Return')));
}

function deleteSelection() {
  const track = selectedTrack();
  const clip = selectedClip();
  if (clip) T.send(form('delete-clip', track.name, clip.id));
  else if (track && track.kind !== 'master') T.send(form('delete-track', track.name));
}

function duplicateClip() {
  const track = selectedTrack();
  const clip = selectedClip();
  if (clip) T.send(form('duplicate-clip', track.name, clip.id, clip.start + clip.length));
}

// What moves between states: the playhead, the position, the meters.
function animate() {
  animateSession();
  if (T.song) {
    const head = $('#playhead');
    if (head) {
      const x = head.parentNode.querySelector('.ruler-track');
      head.style.left = `${(x ? x.offsetLeft : 214) + T.beat * T.ppb}px`;
    }
    const position = $('#position');
    if (position) position.textContent = positionText(T.beat);
    if (T.peaks) {
      allTracks().forEach((track, i) => {
        const level = 1 + (20 * Math.log10(Math.max(T.peaks[i] || 0, 1e-5))) / 60;
        document.querySelectorAll(`[data-meter="${track.id}"] i`).forEach((bar) => {
          bar.style.height = `${Math.max(0, Math.min(1, level)) * 100}%`;
        });
      });
    }
  }
  requestAnimationFrame(animate);
}
