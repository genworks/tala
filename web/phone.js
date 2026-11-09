// Copyright © 2026 Genworks International
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.  Distributed WITHOUT
// ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

// The same page on a phone.  A narrow window gets one view at a time,
// tabs along the bottom, and the device panel, the prompt and the browser
// as sheets that rise over the lower half; a finger gets larger targets
// and stand-ins for what a mouse does by double-click, hover and keys.
// The layout hangs on classes of the body, PHONE, LANDSCAPE and TOUCH,
// set here, so a test can put the page in any of them.

'use strict';

const PHONE = '(max-width: 760px), (pointer: coarse) and (max-height: 500px)';
const ZOOMS = [6, 9, 14, 22, 33, 50];
const ZOOM = T.ppb;
const SHEETS = { detail: 'Edit', prompt: 'Prompt', browser: 'Browse' };

Object.assign(T, { phone: false, landscape: false, touch: false, lastTouch: false, sheet: null, drawing: false });

// A test holds the layout whatever the window says.
let pinned = false;

function layout(on, landscape) {
  if (on === T.phone && landscape === T.landscape) return;
  // a phone opens on more of the song than a desk's window needs to
  if (on !== T.phone) T.ppb = on ? ZOOMS[1] : ZOOM;
  T.phone = on;
  T.landscape = landscape;
  const body = document.body;
  body.classList.toggle('phone', on);
  body.classList.toggle('landscape', landscape);
  body.classList.remove('menu-open', 'sheet-full');
  if (!on) {
    T.sheet = null;
    delete body.dataset.sheet;
  }
  renderPhoneTabs();
  if (T.song) renderAll();
}

function measure() {
  if (pinned) return;
  const on = matchMedia(PHONE).matches;
  layout(on, on && innerWidth > innerHeight && innerHeight <= 500);
}

// ON true or false holds the layout there; null gives it back to the window.
T.setPhone = function setPhone(on, landscape = false) {
  pinned = on !== null;
  if (pinned) layout(!!on, !!on && !!landscape);
  else measure();
};

T.setTouch = function setTouch(on) {
  if (on === T.touch) return;
  T.touch = on;
  document.body.classList.toggle('touch', on);
  if (T.song) renderActions();
};

// The sheets: NAME is detail, prompt or browser, or null for none.
T.setSheet = function setSheet(name) {
  const body = document.body;
  T.sheet = T.phone && name ? name : null;
  if (T.sheet) body.dataset.sheet = T.sheet;
  else delete body.dataset.sheet;
  body.classList.remove('sheet-full', 'menu-open');
  renderPhoneTabs();
  if (T.sheet === 'detail' && T.song) renderDetail();
};

T.showDetail = function showDetail() {
  if (T.phone && T.sheet !== 'detail') T.setSheet('detail');
};

// Brings what holds NODE into sight; true when that drew the page again.
T.reveal = function reveal(node) {
  if (!T.phone) return false;
  const body = document.body;
  const sheet = Object.keys(SHEETS).find((key) => node.closest(`#${key}`));
  if (sheet && T.sheet !== sheet) {
    T.setSheet(sheet);
    return true;
  }
  if (node.closest('#stage')) body.classList.remove('sheet-full');
  if (node.closest('#menu')) body.classList.add('menu-open');
  return false;
};

function sheetButtons() {
  return el('span', { class: 'sheet-buttons' },
    el('button', { 'data-ctl': 'sheet:full', 'data-title': 'Full height',
                   'data-doc': 'Give this panel the whole screen, or half of it again.',
                   onclick: () => {
                     document.body.classList.toggle('sheet-full');
                     if (T.sheet === 'detail') renderDetail();
                   } }, '↕'),
    el('button', { 'data-ctl': 'sheet:close', 'data-title': 'Close', 'data-doc': 'Put this panel away.',
                   onclick: () => T.setSheet(null) }, '×'));
}

function renderPhoneTabs() {
  const views = [['arrangement', 'Arrange'], ['session', 'Session'], ['mixer', 'Mixer']];
  $('#tabs').replaceChildren(
    ...views.map(([key, label]) => el('button', {
      class: T.view === key ? 'on' : '', 'data-ctl': `view:${key}`,
      onclick: () => {
        if (document.body.classList.contains('sheet-full')) T.setSheet(null);
        T.setView(key);
      },
    }, label)),
    el('i'),
    ...Object.entries(SHEETS).map(([key, label]) => el('button', {
      class: T.sheet === key ? 'on' : '', 'data-ctl': `sheet:${key}`,
      onclick: () => T.setSheet(T.sheet === key ? null : key),
    }, label)));
}

const showView = T.setView;
T.setView = function setView(view) {
  showView(view);
  renderPhoneTabs();
  renderActions();
};

// What a finger cannot do with a key or a double-click, for the track or
// clip that is selected.
function renderActions() {
  const box = $('#actions');
  const track = T.song && selectedTrack();
  if (!T.touch || !track || T.view === 'mixer') {
    box.replaceChildren();
    return;
  }
  const clip = selectedClip();
  const button = (label, ctl, doc, onclick, cls) => el('button', {
    class: cls, 'data-ctl': `action:${ctl}`, 'data-title': label, 'data-doc': doc, onclick,
  }, label);
  const fixed = track.kind === 'master';
  const items = clip ? [
    el('span', { class: 'title' }, clip.name),
    button('Edit', 'edit', 'Open the clip: its notes, or its recording.', () => {
      T.detail = 'notes';
      renderDetail();
      T.showDetail();
    }),
    button('Duplicate', 'duplicate', 'Copy the clip after itself.', duplicateClip),
    button('Delete', 'delete', 'Remove the clip. Undo brings it back.', deleteSelection, 'danger'),
  ] : [
    el('span', { class: 'title' }, track.name),
    fixed ? null : button('Rename', 'rename', 'Give the track another name.', () => renameTrack(track)),
    T.view === 'arrangement' && takesClips(track)
      ? button('+ Clip', 'add-clip', 'An empty clip of one bar, at the first free bar from the start marker.',
               () => addClipAtStart(track))
      : null,
    fixed ? null : button('Delete track', 'delete', 'Remove the track. Undo brings it back.', deleteSelection, 'danger'),
  ];
  box.replaceChildren(...items.filter(Boolean));
}

function addClipAtStart(track) {
  const per = beatsPerBar();
  const clips = track.clips.filter((clip) => clip.scene === null);
  let at = Math.floor(T.startBeat / per) * per;
  while (clips.some((clip) => clip.start < at + per && clip.start + clip.length > at)) at += per;
  T.send(form('add-clip', track.name, kw('start'), at, kw('length'), per));
}

// Zoom, by steps: how many pixels a beat is across.
const zoomStep = () => ZOOMS.reduce((best, z, i) => (Math.abs(z - T.ppb) < Math.abs(ZOOMS[best] - T.ppb) ? i : best), 0);

T.canZoom = (by) => ZOOMS[zoomStep() + by] !== undefined;

T.zoom = function zoom(by) {
  const next = ZOOMS[zoomStep() + by];
  if (next === undefined) return;
  const box = $('#arrangement');
  const beat = box.scrollLeft / T.ppb;
  T.ppb = next;
  renderArrangement();
  box.scrollLeft = beat * next;
};

// Where the info box is out of sight, what it would say shows briefly
// over the page.
let toastTimer = null;

T.toast = function toast(text) {
  const box = $('#toast');
  box.textContent = text;
  box.hidden = false;
  clearTimeout(toastTimer);
  toastTimer = setTimeout(() => { box.hidden = true; }, Math.max(2500, text.length * 55));
};

$('#toast').addEventListener('click', () => { $('#toast').hidden = true; });

// The menu: the file buttons and the lesser transport, under the bar.
$('#menu-button').addEventListener('click', () => document.body.classList.toggle('menu-open'));
$('#file-buttons').addEventListener('click', () => document.body.classList.remove('menu-open'));

// Which kind of pointer spoke last.  A finger holding still on a control
// reads its help, and the tap that would follow is dropped.
let press = null;
let swallow = false;

function release() {
  if (press) clearTimeout(press.timer);
  press = null;
}

window.addEventListener('pointerdown', (e) => {
  T.lastTouch = e.pointerType === 'touch';
  swallow = false;
  release();
  if (!e.target.closest) return;
  if (!e.target.closest('#menu, #menu-button')) document.body.classList.remove('menu-open');
  if (!T.lastTouch) return;
  T.setTouch(true);
  const node = e.target.closest('[data-doc]');
  if (!node || node.closest('.clip, .knob, .loop-brace, .pads')) return;
  press = {
    x: e.clientX, y: e.clientY,
    timer: setTimeout(() => {
      press = null;
      swallow = true;
      T.toast([node.dataset.title, node.dataset.doc].filter(Boolean).join(': '));
    }, 550),
  };
}, true);

window.addEventListener('pointermove', (e) => {
  if (press && Math.hypot(e.clientX - press.x, e.clientY - press.y) > 10) release();
}, true);
window.addEventListener('pointerup', release, true);
window.addEventListener('pointercancel', release, true);

document.addEventListener('click', (e) => {
  if (!swallow) return;
  swallow = false;
  e.stopImmediatePropagation();
  e.preventDefault();
}, true);

document.addEventListener('contextmenu', (e) => { if (T.lastTouch) e.preventDefault(); });

// DOUBLE is the page's double-click: the browser's own, or two taps of a
// finger close together, for the browsers that send none.
let doubled = -1000;
let tap = null;

function double(e) {
  if (e.timeStamp - doubled < 500) return;
  doubled = e.timeStamp;
  tap = null;
  e.target.dispatchEvent(new MouseEvent('double', {
    bubbles: true, cancelable: true, view: window, clientX: e.clientX, clientY: e.clientY, altKey: e.altKey,
  }));
}

document.addEventListener('dblclick', double);
document.addEventListener('click', (e) => {
  if (!T.lastTouch) return;
  const near = tap && e.timeStamp - tap.time < 350 && Math.hypot(e.clientX - tap.x, e.clientY - tap.y) < 24;
  if (near) double(e);
  else tap = { time: e.timeStamp, x: e.clientX, y: e.clientY };
}, true);

// The page is as tall as what can be seen, so a keyboard pushes it up
// instead of covering it; while one is up, the open sheet has the screen.
function fit() {
  const view = window.visualViewport;
  if (view && Math.abs(view.scale - 1) > 0.01) return;
  const height = view ? view.height : innerHeight;
  document.documentElement.style.setProperty('--vh', `${height}px`);
  document.body.classList.toggle('typing', !!view && view.height < innerHeight - 120);
  if (T.phone && view && view.offsetTop) window.scrollTo(0, 0);
  measure();
}

window.addEventListener('resize', fit);
if (window.visualViewport) {
  window.visualViewport.addEventListener('resize', fit);
  window.visualViewport.addEventListener('scroll', fit);
}

$('#prompt h2').append(sheetButtons());
$('#browser').prepend(el('div', { class: 'sheet-head' }, el('b', {}, 'Browse'), sheetButtons()));
renderPhoneTabs();
if (matchMedia('(pointer: coarse)').matches) T.setTouch(true);
fit();
