// Copyright © 2026 Genworks International
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.  Distributed WITHOUT
// ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

// The admin page, /tala/admin (source/admin.lisp): the visitors with
// what each did, session by session, and the sessions in memory with
// their songs.  It reads, and deletes from the two logs what its
// operator asks it to, each time after asking back: one line, one
// prompt, or everything kept of a visitor.  It is built from
// window.visitorAdmin, which /tala/admin-kit.js serves; everything shown
// here that a visitor typed goes into the page as text, never as markup.

(function () {
  'use strict';

  const A = window.visitorAdmin;
  const { el, fill, get, when, ago, count, pill, fact, more, shortId, eventRow, card } = A;
  const root = document.getElementById('admin');
  const opened = {};   // a card's key -> true while it stands open
  const limits = {};   // visitor key -> how many events to ask for
  const EVENTS = 300;
  let live = {};       // session id -> true for the sessions in memory
  let heads = {};      // visitor key -> the head of that visitor's card
  let facts = null;    // the line about the server, under the title

  const whose = (v) => v.name || 'the open gate';
  const dim = (text) => el('span', { 'class': 'ad-dim', text });
  const remover = (label, kind, question, body, then) =>
    A.remover('/tala/api/admin/delete', label, kind, question, body, then);

  // ---- one visitor's record -------------------------------------------------

  // Forms as a block, a form a line where the text can be told apart.
  function formsBlock(text) {
    return el('div', { 'class': 'ad-forms ad-mono', style: 'white-space: pre-wrap', text });
  }

  function lineRow(ev) {
    if (ev.kind === 'visit') return eventRow(ev.time, 'visit', '', ['Opened the page']);
    if (ev.kind === 'download') {
      return eventRow(ev.time, 'download', '', [ev.door === 'bounce.wav' ? 'Bounced the song to a WAV file' : 'Saved the song']);
    }
    if (ev.kind === 'import') {
      return eventRow(ev.time, 'import', '', [
        'Brought in a sample', ev.name ? ': ' + ev.name : '',
        ev.bytes === null ? null : dim('  ' + Math.round(ev.bytes / 1024) + ' KB'),
        ev.error ? el('div', { 'class': 'ad-bad', text: ev.error }) : null]);
    }
    if (ev.kind === 'load') {
      return eventRow(ev.time, 'load', '', ['Opened a song file', ev.body ? more('the file, as far as it was kept', ev.body) : null]);
    }
    if (ev.kind === 'asked') {
      return eventRow(ev.time, 'asked', '', [
        dim('Sent a prompt'),
        ev.prompt === null ? (ev.body ? more('request body', ev.body) : null) : el('div', { 'class': 'ad-words', text: ev.prompt })]);
    }
    if (ev.kind === 'forms') {
      const went = ev.ran === null ? null
        : ev.failed ? el('span', { 'class': 'ad-bad', text: '  ' + ev.failed + ' of ' + count(ev.ran, 'form') + ' failed' })
          : dim('  ' + count(ev.ran, 'form'));
      return eventRow(ev.time, 'by hand', 'hand', [
        went,
        ev.error ? el('div', { 'class': 'ad-bad', text: ev.error }) : null,
        ev.forms === null ? (ev.body ? more('request body', ev.body) : null) : formsBlock(ev.forms)]);
    }
    return eventRow(ev.time, ev.door, '', [ev.body ? more('request body', ev.body) : null]);
  }

  function promptRow(ev) {
    const said = [pill('model: ' + (ev.model || 'unknown'), 'model')];
    if (ev.seconds !== null) said.push(dim(' ' + Math.round(ev.seconds) + ' s'));
    if (ev.rounds) said.push(dim(' ' + count(ev.rounds, 'round')));
    if (!ev.ok && ev.results.length) said.push(el('span', { 'class': 'ad-warn', text: ' a form failed' }));
    const what = [el('div', { 'class': 'ad-words', text: ev.prompt || '' }), el('div', {}, said)];
    if (ev.results.length) {
      what.push(el('div', { 'class': 'ad-forms ad-mono' }, ev.results.map((r) => {
        const answered = r.ok ? r.value : r.error;
        return el('div', { 'class': 'ad-form' + (r.ok ? '' : ' ad-failed') }, [
          r.form || '',
          answered ? el('span', { 'class': 'ad-value', text: '  → ' + answered }) : null]);
      })));
    } else if (ev.forms) {
      what.push(formsBlock(ev.forms));
    }
    if (ev.reply) what.push(el('div', { 'class': 'ad-reply', text: ev.reply }));
    if (ev.state) what.push(more('the song as the model was told it', ev.state));
    return eventRow(ev.time, 'prompt', 'model', what);
  }

  function inviteFacts(v) {
    const said = [];
    if (v.invited) {
      said.push(fact('invite issued', v.issued || '?'));
      said.push(fact('uses since the server started', v.uses === null ? 0 : v.uses));
      said.push(fact('link last used', v.seen ? when(v.seen) : 'not since the server started'));
    } else {
      said.push(el('span', { text: v.name ? 'No invite in this name now (revoked, or from another roster).' : 'Requests made with the gate open.' }));
    }
    if (v.first) said.push(fact('first recorded', when(v.first)));
    return el('div', { 'class': 'ad-facts' }, said);
  }

  function sessionHead(v, id) {
    const known = v.sessions.find((s) => s.id === id);
    const kids = [el('b', { 'class': 'ad-mono', title: id, text: 'Session ' + shortId(id) })];
    if (live[id]) kids.push(' ', pill('in memory', 'live'));
    if (known) kids.push(dim('  ' + count(known.requests, 'request') + ', ' + count(known.prompts, 'prompt')));
    return el('div', { 'class': 'ad-session' }, kids);
  }

  function entryRemover(v, ev, then) {
    if (ev.kind === 'prompt') {
      return remover('Delete', 'del', 'Delete this prompt of ' + whose(v) + ', with its forms?  It cannot be brought back.',
                     { who: v.key, turn: ev.turn }, then);
    }
    return remover('Delete', 'del', 'Delete this line from the record of ' + whose(v) + '?  It cannot be brought back.',
                   { who: v.key, line: ev.line, mark: ev.mark }, then);
  }

  // After one thing of V's has gone: the counts on their card and under
  // the title, and their record again, all in place, so that the page
  // stays where it was for the next one.
  function recount(v, body) {
    get('/tala/api/admin').then((res) => {
      const now = res.visitors.find((x) => x.key === v.key);
      if (!now || !heads[v.key] || !facts) { render(res); return; }
      Object.assign(v, now);
      fill(heads[v.key], visitorHead(v));
      fill(facts, serverFacts(res));
      loadVisitor(v, body);
    }).catch(load);
  }

  function loadVisitor(v, body) {
    const limit = limits[v.key] || EVENTS;
    if (!body.firstChild) body.textContent = 'Loading...';
    get('/tala/api/admin/visitor?who=' + encodeURIComponent(v.key) + '&limit=' + limit).then((res) => {
      let session = null;
      const kids = [inviteFacts(v)];
      if (res.total) {
        kids.push(el('p', {}, [remover(
          'Delete everything kept of ' + whose(v), 'danger',
          'Delete everything kept of ' + whose(v) + ': ' + count(res.total, 'line or prompt', 'lines and prompts') + '?  ' +
            (v.invited ? 'Their invite stays.  ' : '') + 'It cannot be brought back.',
          { who: v.key, everything: true },
          () => { opened[v.key] = false; load(); })]));
      }
      if (res.total > res.events.length) {
        const all = el('button', { type: 'button', text: 'Show all' });
        all.addEventListener('click', () => { limits[v.key] = 20000; loadVisitor(v, body); });
        kids.push(el('p', { 'class': 'ad-dim' }, ['The last ' + res.events.length + ' of ' + res.total + ' recorded.  ', all]));
      }
      if (!res.events.length) kids.push(el('p', { 'class': 'ad-dim', text: 'Nothing recorded yet.' }));
      res.events.forEach((ev) => {
        // a line with no session (the page itself) stays with the one before
        if (ev.session && ev.session !== session) {
          session = ev.session;
          kids.push(sessionHead(v, session));
        }
        const row = ev.kind === 'prompt' ? promptRow(ev) : lineRow(ev);
        row.appendChild(entryRemover(v, ev, () => recount(v, body)));
        kids.push(row);
      });
      fill(body, kids);
    }).catch((e) => {
      body.textContent = 'Could not load this visitor: ' + e.message;
    });
  }

  function visitorHead(v) {
    const did = [count(v.visits, 'visit'), count(v.prompts, 'prompt'), count(v.forms, 'change') + ' by hand'];
    if (v.failed) did.push(count(v.failed, 'form') + ' failed');
    if (v.imports) did.push(count(v.imports, 'sample'));
    if (v.downloads) did.push(count(v.downloads, 'download'));
    did.push(count(v.sessions.length, 'session'));
    const head = [el('span', { 'class': 'ad-name', text: v.name || 'Open gate (no invite)' })];
    if (!v.invited && v.name) head.push(pill('no invite now'));
    head.push(dim(did.join(' · ')));
    head.push(dim(v.last ? 'last ' + ago(v.last) : 'nothing recorded'));
    return head;
  }

  function visitorCard(v) {
    const body = el('div', { 'class': 'ad-body' });
    let shown = false;
    const c = card(visitorHead(v), body, opened, v.key, () => {
      if (!shown) { shown = true; loadVisitor(v, body); }
    });
    heads[v.key] = c.firstChild;
    if (c.open) { shown = true; loadVisitor(v, body); }
    return c;
  }

  // ---- the sessions in memory -------------------------------------------------

  function trackTable(song) {
    const rows = [el('tr', {}, ['track', 'instrument', 'devices', 'clips', 'notes', ''].map((h) => el('th', { text: h })))];
    const row = (track, kind) => el('tr', track.mute ? { 'class': 'ad-hidden', title: 'muted' } : {}, [
      el('td', { text: track.name || '' }),
      el('td', { text: kind || track.instrument || 'audio' }),
      el('td', { text: track.devices.join(', ') }),
      el('td', { text: String(track.clips) }),
      el('td', { text: String(track.notes) }),
      el('td', { text: [track.mute ? 'muted' : '', track.solo ? 'solo' : ''].filter(Boolean).join(' ') })]);
    song.tracks.forEach((track) => rows.push(row(track)));
    song.returns.forEach((track) => rows.push(row(track, 'return')));
    return el('div', { 'class': 'ad-scroll' }, [el('table', { 'class': 'ad-table' }, rows)]);
  }

  function sessionCard(s) {
    const song = s.song;
    const head = [el('a', { 'class': 'ad-name ad-mono', href: '/tala/?s=' + encodeURIComponent(s.id), target: '_blank',
                            rel: 'noopener', title: s.id, text: shortId(s.id) })];
    head.push(s.who ? el('span', { text: s.who === 'open-gate' ? 'through the open gate' : s.who }) : dim('nobody on record'));
    head.push(dim(s.busy ? 'busy with a request just now'
      : song ? count(song.tracks.length, 'track') + ', ' + song.tempo + ' BPM' : 'not readable'));
    head.push(dim(s.touched ? 'asked for ' + ago(s.touched) : 'no request seen yet'));
    const body = el('div', { 'class': 'ad-body' });
    if (s.error) body.appendChild(el('p', { 'class': 'ad-bad', text: s.error }));
    if (s.busy) body.appendChild(el('p', { 'class': 'ad-dim', text: 'Refresh to see its song.' }));
    if (song) {
      body.appendChild(el('div', { 'class': 'ad-facts' }, [
        fact('song', song.name || 'untitled'), fact('tempo', song.tempo), fact('time', song.signature),
        fact('scenes', song.scenes), fact('undo', song.undo), fact('redo', song.redo),
        fact('prompts in its history', song.prompts)]));
      body.appendChild(trackTable(song));
      if (song.samples.length) {
        body.appendChild(el('p', { 'class': 'ad-dim', text: 'Samples: ' + song.samples.map((sample) =>
          sample.name + ' (' + (Math.round(sample.seconds * 10) / 10) + ' s)').join(', ') }));
      }
    }
    return card(head, body, opened, 'session ' + s.id);
  }

  // ---- the page ---------------------------------------------------------------

  function serverFacts(res) {
    const server = res.server;
    return [
      fact('as of', when(res.now)),
      fact('Lisp', server.lisp),
      server.heapUsed === null ? null : fact('heap', Math.round(server.heapUsed * 100) + '%', server.heapUsed > 0.7 ? 'warn' : ''),
      fact('gate', server.gate ? 'invites only' : 'open', server.gate ? '' : 'warn'),
      fact('activity log', server.activityLog ? 'on' : 'off', server.activityLog ? '' : 'warn'),
      fact('prompt log', server.promptLog ? server.promptsKept + ' kept' : 'off', server.promptLog ? '' : 'warn')];
  }

  function render(res) {
    const refresh = el('button', { type: 'button', text: 'Refresh' });
    refresh.addEventListener('click', load);
    const server = res.server;
    live = {};
    heads = {};
    res.sessions.forEach((s) => { live[s.id] = true; });
    const kids = [
      el('div', { 'class': 'ad-head' }, [el('h1', { text: 'Tala admin' }), el('a', { href: '/tala/', text: 'the page' }), refresh]),
      facts = el('div', { 'class': 'ad-facts' }, serverFacts(res))];
    if (!server.activityLog) {
      kids.push(el('p', { 'class': 'ad-warn', text: 'The activity log is off: visits, downloads and what is done by hand are not being recorded.' }));
    }
    if (!server.promptLog) {
      kids.push(el('p', { 'class': 'ad-warn', text: 'The prompt log is off: prompts and their forms are not being recorded.' }));
    }
    kids.push(el('h2', { text: 'Visitors (' + res.visitors.length + ')' }));
    if (!res.visitors.length) kids.push(el('p', { 'class': 'ad-dim', text: 'Nobody on the roster and nothing recorded.' }));
    res.visitors.forEach((v) => kids.push(visitorCard(v)));
    kids.push(el('h2', { text: 'Sessions in memory (' + res.sessions.length + ')' }));
    kids.push(el('p', { 'class': 'ad-dim', text: 'A session lasts until the server restarts, or until there are ' +
                        server.mostSessions + ' of them, when all are dropped.  Looking here changes none.' }));
    res.sessions.forEach((s) => kids.push(sessionCard(s)));
    fill(root, kids);
  }

  function load() {
    get('/tala/api/admin').then(render).catch((e) => {
      root.textContent = 'Could not load the summary: ' + e.message;
    });
  }

  load();
})();
