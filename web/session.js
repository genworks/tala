// Copyright © 2026 Genworks International
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.  Distributed WITHOUT
// ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

// The session: a column a track, a row a scene, a clip in a slot.  A
// launch goes straight to the audio thread, which starts the clip on the
// next boundary of the launch quantize; nothing here keeps time.  What
// is playing and what is waiting comes back with the meters.

'use strict';

const trackIndex = (track) => T.song.tracks.indexOf(track);
const slotClip = (track, scene) => track.clips.find((c) => c.scene === scene.id);

async function ensurePlaying() {
  if (!T.playing) await T.play(T.startBeat);
}

T.launch = async function launch(index, row) {
  await ensurePlaying();
  engine.node.port.postMessage({ t: 'launch', track: index, row });
};

T.launchScene = async function launchScene(row) {
  await ensurePlaying();
  const scene = T.song.scenes[row];
  T.song.tracks.forEach((track, i) => {
    engine.node.port.postMessage({ t: 'launch', track: i, row: slotClip(track, scene) ? row : -1 });
  });
};

T.stopClips = function stopClips(index = -1) {
  if (!engine.node) return;
  T.song.tracks.forEach((_, i) => {
    if (index < 0 || index === i) engine.node.port.postMessage({ t: 'launch', track: i, row: -1 });
  });
};

T.backToArrangement = function backToArrangement(index = -1) {
  if (engine.node) engine.node.port.postMessage({ t: 'arrange', track: index });
};

// The state the engine reports, per track: [mode, slot + 1, queued + 1, origin].
const trackState = (i) => (T.sessionState && T.sessionState[i]) || [0, 0, 0, 0];
const anyInSession = () => T.song.tracks.some((_, i) => trackState(i)[0] > 0.5);

function slotCell(track, scene, row) {
  const clip = slotClip(track, scene);
  const index = trackIndex(track);
  const color = COLORS[track.color % 12];
  const audio = track.instrument.kind === 'sampler';
  const cell = el('div', {
    class: 'slot' + (clip ? ' full' : ' empty') + (clip && clip.id === T.clipId ? ' selected' : ''),
    'data-ctl': clip ? `clip:${clip.id}` : `slot:${track.name}:${row}`,
    'data-slot': `${index}:${row}`,
    style: clip ? `background:${color}` : '',
    'data-title': clip ? clip.name : 'Empty slot',
    'data-doc': clip
      ? 'The triangle launches it on the next boundary. Click to select, double-click for its notes.'
      : audio ? 'Drop an audio file here for a clip. The square stops the track.'
              : 'Double-click for a new clip. The square stops the track.',
    onclick: () => {
      T.trackId = track.id;
      T.clipId = clip ? clip.id : null;
      renderSession();
      renderDetail();
    },
    ondblclick: async () => {
      if (clip) { T.detail = 'notes'; renderDetail(); return; }
      if (audio) return;
      const answer = await T.send(form('add-clip', track.name, kw('scene'), row, kw('length'), 4));
      const made = T.song.tracks.find((t) => t.id === track.id).clips.find((c) => c.scene === scene.id);
      if (answer.ok && made) {
        T.trackId = track.id;
        T.clipId = made.id;
        T.detail = 'notes';
        renderDetail();
      }
    },
    ondragover: (e) => e.preventDefault(),
    ondrop: async (e) => {
      e.preventDefault();
      e.stopPropagation();
      const file = e.dataTransfer.files[0];
      if (!file || clip) return;
      const { channels, data } = await T.decodeFile(file);
      const name = await T.uploadSample(file.name.replace(/\.[^.]*$/, ''), channels, data);
      const target = await audioTrack(audio ? track : null);
      T.send(form('add-audio-clip', target, name, kw('scene'), row));
    },
  },
  el('button', {
    class: 'launch', 'data-ctl': clip ? `launch:${track.name}:${row}` : null,
    onclick: (e) => { e.stopPropagation(); T.launch(index, clip ? row : -1); },
  }, clip ? '▶' : '■'),
  clip ? el('span', { class: 'slot-name' }, clip.name) : null,
  clip ? el('i', { class: 'progress' }) : null);
  return cell;
}

function renderSession() {
  const box = $('#session');
  const song = T.song;
  const tracks = song.tracks;
  const columns = `repeat(${tracks.length}, minmax(120px, 150px)) 160px`;
  const grid = el('div', { class: 'grid', style: `grid-template-columns:${columns}` });
  tracks.forEach((track) => grid.append(el('div', {
    class: 'grid-head' + (track.id === T.trackId ? ' selected' : ''),
    style: `border-color:${COLORS[track.color % 12]}`,
    'data-ctl': `session:${track.name}`,
    onclick: () => { T.trackId = track.id; renderSession(); renderDetail(); },
  }, track.name)));
  grid.append(el('div', { class: 'grid-head scenes-head' }, 'Scenes'));
  song.scenes.forEach((scene, row) => {
    tracks.forEach((track) => grid.append(slotCell(track, scene, row)));
    grid.append(el('div', { class: 'scene-cell', 'data-ctl': `scene:${scene.name}` },
      el('button', {
        class: 'launch', 'data-title': 'Launch scene', 'data-doc': 'Every clip in this row at once; tracks with an empty slot stop.',
        onclick: () => T.launchScene(row),
      }, '▶'),
      el('span', {
        class: 'scene-name', 'data-title': scene.name, 'data-doc': 'Double-click to rename.',
        ondblclick: () => {
          const name = window.prompt('Scene name', scene.name);
          if (name && name !== scene.name) T.send(form('rename-scene', row, name));
        },
      }, scene.name)));
  });
  tracks.forEach((track) => grid.append(el('div', { class: 'stop-cell' },
    el('button', { 'data-title': 'Stop', 'data-doc': 'Stop this track\'s clip at the next boundary.',
                   onclick: () => T.stopClips(trackIndex(track)) }, '■'))));
  grid.append(el('div', { class: 'stop-cell' },
    el('button', { 'data-ctl': 'stop-all', 'data-title': 'Stop all clips',
                   'data-doc': 'Every track\'s clip stops at the next boundary.', onclick: () => T.stopClips() }, '■ All')));
  const quantize = el('select', {
    'data-ctl': 'quantize', 'data-title': 'Launch quantize',
    'data-doc': 'How long a launch waits: it lands on the next bar, beat or whatever is chosen here.',
    onchange: (e) => T.send(form('set-launch-quantize', +e.target.value)),
  }, [['None', 0], ['1/4 beat', 0.25], ['1 beat', 1], ['1 bar', 4], ['2 bars', 8], ['4 bars', 16]].map(([label, value]) =>
    el('option', { value, selected: value === song.quantize }, label)));
  box.replaceChildren(
    el('div', { class: 'session-bar' },
      el('button', { 'data-ctl': 'add-scene', 'data-title': 'Add scene', 'data-doc': 'A new row of slots.',
                     onclick: () => T.send(form('add-scene', null)) }, '+ Scene'),
      el('label', {}, 'Launch on ', quantize),
      tracks.length ? null : el('span', { class: 'turn-note' }, 'Add a track to fill its column.')),
    grid);
}

// Between states: what plays, what waits, how far through.
function animateSession() {
  if (!T.song || !T.sessionState) return;
  const arrange = $('#back-to-arrangement');
  if (arrange) arrange.hidden = !anyInSession();
  // a track playing a launched clip is not playing its timeline
  T.song.tracks.forEach((track, i) => {
    const lane = document.querySelector(`.track-row[data-track="${track.id}"] .lane`);
    if (lane) lane.classList.toggle('in-session', trackState(i)[0] > 0.5);
  });
  if ($('#session').hidden) return;
  document.querySelectorAll('.slot[data-slot]').forEach((cell) => {
    const [i, row] = cell.dataset.slot.split(':').map(Number);
    const [mode, slot, queued, origin] = trackState(i);
    const playing = mode > 0.5 && slot === row + 1;
    cell.classList.toggle('playing', playing);
    cell.classList.toggle('queued', queued === row + 1);
    const bar = cell.querySelector('.progress');
    if (bar) {
      const track = T.song.tracks[i];
      const clip = track && slotClip(track, T.song.scenes[row]);
      const through = playing && clip ? (((T.clock - origin) % clip.length) + clip.length) % clip.length / clip.length : 0;
      bar.style.width = `${through * 100}%`;
    }
  });
}
