// Copyright © 2026 Genworks International
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.  Distributed WITHOUT
// ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

// The mixer: a strip a track, with its fader, pan and sends.

'use strict';

function sendControls(track) {
  const returns = T.song.returns;
  return returns.map((r, i) => knob(
    { key: 'send', label: `Send ${r.name}`, min: -60, max: 6, default: -60, unit: 'dB', log: false,
      doc: `How much of this track goes to ${r.name}. All the way down is off.` },
    track.sends[i] === undefined ? -60 : track.sends[i],
    `track:${track.name}:send:${r.name}`,
    (v) => setLive(track.param + 3 + i, v),
    (v) => {
      const sends = returns.map((_, j) => (j === i ? v : track.sends[j]));
      T.send(form('set-track', track.name, kw('sends'), sends));
    }));
}

const PAN = { key: 'pan', label: 'Pan', min: -1, max: 1, default: 0, unit: '', log: false,
              doc: 'Left -1 to right 1.' };

function strip(track) {
  const fader = el('input', {
    type: 'range', min: -60, max: 12, step: 0.1, value: track.gain,
    'data-ctl': `track:${track.name}:gain`, 'data-title': 'Volume', 'data-doc': 'The fader, in dB.',
    oninput: (e) => setLive(track.param, +e.target.value),
    onchange: (e) => T.send(form('set-track', track.name, kw('gain'), +e.target.value)),
  });
  const button = (key, label) => el('button', {
    class: track[key] ? 'on' : '',
    onclick: () => T.send(form('set-track', track.name, kw(key), !track[key])),
  }, label);
  return el('div', {
    class: 'strip' + (track.id === T.trackId ? ' selected' : ''),
    onclick: () => { T.trackId = track.id; renderDetail(); },
  },
  el('div', { class: 'strip-name', style: `border-color:${COLORS[track.color % 12]}` }, track.name),
  track.kind === 'midi' ? sendControls(track) : null,
  knob(PAN, track.pan, `track:${track.name}:pan`, (v) => setLive(track.param + 1, v),
       (v) => T.send(form('set-track', track.name, kw('pan'), v))),
  el('div', { class: 'strip-fader' }, fader, el('div', { class: 'meter', 'data-meter': track.id }, el('i'))),
  el('div', { class: 'strip-buttons' },
    track.kind === 'master' ? null : button('mute', 'M'),
    track.kind === 'midi' ? button('solo', 'S') : null));
}

function renderMixer() {
  $('#mixer').replaceChildren(...allTracks().map(strip));
}

T.setView = function setView(view) {
  T.view = view;
  $('#arrangement').hidden = view !== 'arrangement';
  $('#session').hidden = view !== 'session';
  $('#mixer').hidden = view !== 'mixer';
  renderTabs();
};

function renderTabs() {
  $('#view-tabs').replaceChildren(
    ...[['arrangement', 'Arrangement'], ['session', 'Session'], ['mixer', 'Mixer']].map(([key, label]) =>
      el('button', { class: T.view === key ? 'on' : '', 'data-ctl': `view:${key}`,
                     'data-title': label, 'data-doc': 'Tab switches between the arrangement and the session.',
                     onclick: () => T.setView(key) }, label)));
}
