#!/usr/bin/env node
// Copyright © 2026 Genworks International
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.  Distributed WITHOUT
// ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.
//
// Load the Tala page in headless Chromium, wait for it, run a script of
// its own against the page, print the result as JSON and take a
// screenshot.  Exit 0 when the result says ok and the page threw
// nothing.  Usage: node probe.js URL [screenshot.png] [expression]
// EXPRESSION defaults to TALA.selfTest().  PHONE=390x844 in the
// environment makes the window a phone's, with a finger for a pointer;
// the page then asks for touches through probeTouch.  READY, an
// expression answering "ready", says when a page other than the main
// one (the admin page) has loaded.  Talks to Chromium over the
// DevTools protocol with Node's own fetch and WebSocket (Node 22 or
// later); no packages.

const { spawn } = require('child_process');
const fs = require('fs');

const url = process.argv[2];
const shot = process.argv[3];
const expression = process.argv[4] || 'TALA.selfTest()';
const phone = (process.env.PHONE || '').match(/^(\d+)x(\d+)$/);
// what says the page is ready: "ready", or the words of what went wrong
const ready = process.env.READY || 'window.TALA && (TALA.ready ? "ready" : TALA.error || null)';
const bin = process.env.CHROMIUM || 'chromium';
const profile = `/tmp/tala-probe-${process.pid}`;
const chrome = spawn(bin, ['--headless=new', '--no-sandbox', '--hide-scrollbars',
  '--autoplay-policy=no-user-gesture-required', '--remote-debugging-port=0',
  // AudioWorklet exists only in a secure context: https, localhost, or this
  `--unsafely-treat-insecure-origin-as-secure=${new URL(url).origin}`,
  // a pretend microphone, allowed without asking, for the recording test
  '--use-fake-device-for-media-stream', '--use-fake-ui-for-media-stream',
  '--window-size=1500,950', `--user-data-dir=${profile}`, 'about:blank'], { stdio: 'ignore' });
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

async function devtoolsPort() {
  for (let i = 0; i < 100; i++) {
    try {
      const p = Number(fs.readFileSync(`${profile}/DevToolsActivePort`, 'utf8').split('\n')[0]);
      if (p > 0) return p;
    } catch {}
    await sleep(100);
  }
  throw new Error('no DevTools port');
}

async function main() {
  const port = await devtoolsPort();
  let targets = [];
  for (let i = 0; i < 50 && !targets.length; i++) {
    try { targets = await (await fetch(`http://127.0.0.1:${port}/json`)).json(); } catch {}
    if (!targets.length) await sleep(200);
  }
  const ws = new WebSocket(targets[0].webSocketDebuggerUrl);
  await new Promise((res, rej) => { ws.onopen = res; ws.onerror = () => rej(new Error('DevTools socket')); });
  let id = 0;
  const pending = new Map();
  const errors = [];
  ws.onmessage = (m) => {
    const msg = JSON.parse(m.data);
    if (msg.id && pending.has(msg.id)) { pending.get(msg.id)(msg); pending.delete(msg.id); }
    else if (msg.method === 'Runtime.bindingCalled') touch(JSON.parse(msg.params.payload));
    else if (msg.method === 'Runtime.exceptionThrown') {
      const d = msg.params.exceptionDetails;
      errors.push(`${d.text} ${(d.exception && d.exception.description) || ''}`.slice(0, 400));
    } else if (msg.method === 'Runtime.consoleAPICalled' && msg.params.type === 'error') {
      errors.push(msg.params.args.map((a) => a.value || a.description).join(' ').slice(0, 400));
    }
  };
  const send = (method, params = {}) => new Promise((res) => {
    pending.set(++id, res);
    ws.send(JSON.stringify({ id, method, params }));
  });
  const evaluate = async (text) => {
    const r = await send('Runtime.evaluate', { expression: text, returnByValue: true, awaitPromise: true });
    if (r.result.exceptionDetails) throw new Error(JSON.stringify(r.result.exceptionDetails).slice(0, 600));
    return r.result.result.value;
  };
  // A finger, as the browser itself delivers one: STEPS are [x, y] points
  // it goes down on and moves through before it lifts, or a number of
  // milliseconds to hold still.  The page hears when it is over.
  async function touch({ serial, steps }) {
    let down = false;
    for (const step of steps) {
      if (typeof step === 'number') { await sleep(step); continue; }
      const [x, y] = step;
      await send('Input.dispatchTouchEvent', { type: down ? 'touchMove' : 'touchStart', touchPoints: [{ x, y }] });
      down = true;
      await sleep(30);
    }
    await send('Input.dispatchTouchEvent', { type: 'touchEnd', touchPoints: [] });
    await sleep(60);
    await send('Runtime.evaluate', { expression: `TALA.touched(${serial})` });
  }
  await send('Runtime.enable');
  await send('Page.enable');
  if (phone) {
    await send('Emulation.setDeviceMetricsOverride',
               { width: Number(phone[1]), height: Number(phone[2]), deviceScaleFactor: 2, mobile: true });
    await send('Emulation.setTouchEmulationEnabled', { enabled: true, maxTouchPoints: 5 });
    await send('Runtime.addBinding', { name: 'probeTouch' });
  }
  await send('Page.navigate', { url });
  let state = null;
  for (let i = 0; i < 300; i++) {
    await sleep(200);
    try { state = await evaluate(ready); } catch {}
    if (state) break;
  }
  if (state !== 'ready') {
    console.log(JSON.stringify({ ok: false, error: state || 'the page never became ready', errors }));
    process.exitCode = 2;
    return;
  }
  const result = await evaluate(expression);
  await sleep(400);
  if (shot) {
    const r = await send('Page.captureScreenshot', { format: 'png' });
    fs.writeFileSync(shot, Buffer.from(r.result.data, 'base64'));
  }
  console.log(JSON.stringify({ result, pageErrors: errors }, null, 1));
  process.exitCode = result && result.ok && !errors.length ? 0 : 1;
  ws.close();
}

main().catch((e) => { console.error('probe: ' + e.message); process.exitCode = 3; })
  .finally(() => {
    try { chrome.kill(); } catch {}
    try { fs.rmSync(profile, { recursive: true, force: true }); } catch {}
  });
