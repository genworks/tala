#!/usr/bin/env node
// Copyright © 2026 Genworks International
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.  Distributed WITHOUT
// ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.
//
// The worklet module against the server's render, with no browser: the
// module runs under a stand-in for the AudioWorklet globals, 128 frames
// a call as the browser calls it, and every sample is compared with
// what the Lisp program made, after rounding to float32 as the
// browser's output buffer does.  Exits 0 when they are identical.
//
// Usage: node parity.js BASE SESSION [FRAMES]

async function main() {
  const [base, session, framesText] = process.argv.slice(2);
  const frames = Number(framesText || 96000);
  const state = await (await fetch(`${base}/tala/api/state?s=${session}`)).json();
  const code = await (await fetch(`${base}/tala/api/engine.js?name=${state.processor}`)).text();
  const reference = new Float32Array(await (await fetch(
    `${base}/tala/api/render.f32?s=${session}&frames=${frames}`)).arrayBuffer());
  let Processor = null;
  // the module's own globals, in this context: a vm context's global
  // lookups cost more than the engine itself
  globalThis.sampleRate = 48000;
  globalThis.AudioWorkletProcessor = class { constructor() { this.port = { postMessage() {} }; } };
  globalThis.registerProcessor = (name, cls) => { Processor = cls; };
  (0, eval)(code);
  const engine = new Processor({ processorOptions: { params: state.params, score: state.score, playing: true, beat: 0 } });
  const left = new Float32Array(frames);
  const right = new Float32Array(frames);
  const started = Date.now();
  for (let at = 0; at < frames; at += 128) {
    const n = Math.min(128, frames - at);
    engine.process([], [[left.subarray(at, at + n), right.subarray(at, at + n)]]);
  }
  const ms = Date.now() - started;
  let different = 0, first = -1, most = 0, peak = 0;
  for (let i = 0; i < frames; i++) {
    const a = left[i], b = reference[i], c = right[i], d = reference[frames + i];
    peak = Math.max(peak, Math.abs(a), Math.abs(c));
    if (a !== b || c !== d) {
      different++;
      if (first < 0) first = i;
      most = Math.max(most, Math.abs(a - b), Math.abs(c - d));
    }
  }
  const seconds = frames / 48000;
  console.log(JSON.stringify({ frames, different, first, most, peak, reference: reference.length,
    ms, realtime: Number((seconds * 1000 / ms).toFixed(1)) }));
  process.exitCode = different === 0 && peak > 0 ? 0 : 1;
}

main().catch((e) => { console.error('parity: ' + e.message); process.exitCode = 3; });
