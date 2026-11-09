// Copyright © 2026 Genworks International
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.  Distributed WITHOUT
// ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

// The prompt tile: words go to the model, its forms run on the server,
// and here the song redraws, what it heard shows, and the controls it
// talked about light up.

'use strict';

function logTurn(node) {
  const log = $('#prompt-log');
  log.append(node);
  log.scrollTop = log.scrollHeight;
}

function formsDetails(results) {
  if (!results.length) return null;
  return el('details', { class: 'turn-forms' },
    el('summary', {}, `${results.length} form${results.length === 1 ? '' : 's'}`),
    el('pre', {}, results.map((r) => (r.ok
      ? el('div', {}, r.form)
      : el('div', { class: 'bad' }, `${r.form}\n  ${r.error}`)))));
}

function showReply(result) {
  const failed = result.results.filter((r) => !r.ok);
  if (result.reply) {
    logTurn(el('div', { class: 'turn-reply' + (result.declined || result.stopped ? ' failed' : '') }, result.reply));
  }
  for (const png of result.heard) {
    logTurn(el('img', { src: `data:image/png;base64,${png}`, alt: 'A spectrogram of what the model heard' }));
  }
  logTurn(formsDetails(result.results) || el('div'));
  if (failed.length) {
    logTurn(el('div', { class: 'turn-reply failed' }, failed[failed.length - 1].error));
  }
}

async function awaitJob(job) {
  for (;;) {
    await new Promise((r) => setTimeout(r, 1200));
    const r = await fetch(`/tala/api/prompt?job=${job}&s=${encodeURIComponent(T.session)}`);
    if (!r.ok) throw new Error(await r.text());
    const state = await r.json();
    if (state.status === 'done') return state.result;
    if (state.status === 'failed') throw new Error(state.error);
    if (state.status === 'unknown') throw new Error('The server lost track of that request.');
  }
}

T.prompt = async function prompt(text) {
  const button = $('#prompt-send');
  const note = $('#prompt-status');
  logTurn(el('div', { class: 'turn-you' }, text));
  button.disabled = true;
  note.textContent = 'Working...';
  try {
    const { job } = await post('prompt', { prompt: text });
    const result = await awaitJob(job);
    applySong(result.song);
    showReply(result);
    if (result.ui.length) T.doUi(result.ui);
    return result;
  } catch (e) {
    logTurn(el('div', { class: 'turn-reply failed' }, e.message));
    return { ok: false, error: e.message };
  } finally {
    button.disabled = false;
    note.textContent = '';
  }
};

// (point-at) and (show).

function clearCallouts() {
  document.querySelectorAll('.callout').forEach((n) => n.remove());
  document.querySelectorAll('.pointed').forEach((n) => n.classList.remove('pointed'));
}

function findControl(target) {
  const wanted = target.toLowerCase();
  const found = [...document.querySelectorAll('[data-ctl]')]
    .filter((n) => n.dataset.ctl.toLowerCase() === wanted);
  // A view has a tab in the bar and one along the bottom: the one in sight.
  return found.find((n) => n.getClientRects().length) || found[0];
}

// The track a target lives on, opened so the control is there to find.
function revealFor(target) {
  const parts = target.split(':');
  if (parts[0] === 'device' || parts[0] === 'track') {
    const track = allTracks().find((t) => t.name.toLowerCase() === (parts[1] || '').toLowerCase());
    if (track) {
      T.trackId = track.id;
      if (parts[0] === 'device') T.detail = 'devices';
      renderSelection();
      renderDetail();
    }
  }
  if (target === 'mixer' || target === 'view:mixer') T.setView('mixer');
  if (target === 'session' || target === 'view:session' || /^(slot|launch|scene|session):|^stop-all$|^quantize$|^add-scene$/.test(target)) T.setView('session');
  if (target === 'arrangement') T.setView('arrangement');
}

function pointAt(target, say, index) {
  revealFor(target);
  let node = findControl(target);
  if (node && T.reveal(node)) node = findControl(target);
  if (!node) {
    if (say) logTurn(el('div', { class: 'turn-note' }, say));
    return;
  }
  node.scrollIntoView({ block: 'nearest', inline: 'nearest' });
  node.classList.add('pointed');
  if (say) {
    const box = node.getBoundingClientRect();
    const callout = el('div', {
      class: 'callout', onclick: clearCallouts,
      style: `left:${Math.max(8, Math.min(innerWidth - 270, box.left))}px;` +
             `top:${Math.max(8, box.top - 12 - 44 - index * 4)}px`,
    }, say);
    $('#notes-layer').append(callout);
  }
}

T.doUi = function doUi(actions) {
  clearCallouts();
  let index = 0;
  for (const action of actions) {
    if (action.kind === 'point') {
      pointAt(action.target, action.say, index++);
    } else if (action.kind === 'launch') {
      T.setView('session');
      T.launch(action.track, action.row);
    } else if (action.kind === 'scene') {
      T.setView('session');
      T.launchScene(action.row);
    } else if (action.kind === 'stop') {
      T.stopClips(action.track);
    } else if (action.kind === 'arrange') {
      T.backToArrangement(action.track);
    } else if (action.kind === 'show') {
      if (action.view === 'arrangement' || action.view === 'mixer') T.setView(action.view);
      else {
        const track = action.track && allTracks().find((t) => t.name.toLowerCase() === String(action.track).toLowerCase());
        if (track) T.trackId = track.id;
        if (action.view === 'notes' && action.clip) T.clipId = action.clip;
        T.detail = action.view;
        renderDetail();
      }
    }
  }
};
