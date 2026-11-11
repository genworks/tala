#!/bin/bash
# Copyright © 2026 Genworks International
# AGPL-3.0-or-later; see LICENSE.
#
# The browser half of the regression suite, in headless Chromium: the
# worklet against the server's frames, sample for sample, then the page
# driven as a person drives it.  BASE is where the page is published;
# SESSION a session whose song has a track called Bass.  SHOT_DIR, when
# set, gets screenshots.
#
#   tests/browser.sh
#   BASE=http://engine-room:9090 SESSION=goa SHOT_DIR=/tmp tests/browser.sh
BASE=${BASE:-http://engine-room:9090}
SESSION=${SESSION:-goa}
DIR=$(cd "$(dirname "$0")" && pwd)
URL="$BASE/tala/?s=$SESSION"
status=0
echo "== parity"
timeout 120 node "$DIR/probe.js" "$URL" "${SHOT_DIR:+$SHOT_DIR/parity.png}" 'TALA.selfTest(480000)' || status=1
echo "== page"
timeout 120 node "$DIR/probe.js" "$URL" "${SHOT_DIR:+$SHOT_DIR/page.png}" 'TALA.uiTest()' || status=1
echo "== session"
timeout 120 node "$DIR/probe.js" "$BASE/tala/?s=session-test-$$" "${SHOT_DIR:+$SHOT_DIR/session.png}" 'TALA.sessionTest()' || status=1
echo "== recording"
timeout 120 node "$DIR/probe.js" "$BASE/tala/?s=record-test-$$" "${SHOT_DIR:+$SHOT_DIR/record.png}" 'TALA.recordTest()' || status=1
echo "== audio recording"
timeout 120 node "$DIR/probe.js" "$BASE/tala/?s=audio-test-$$" "${SHOT_DIR:+$SHOT_DIR/audio.png}" 'TALA.audioRecordTest()' || status=1
echo "== midi out"
timeout 120 node "$DIR/probe.js" "$BASE/tala/?s=midi-test-$$" "${SHOT_DIR:+$SHOT_DIR/midi.png}" 'TALA.midiOutTest()' || status=1
echo "== samples"
timeout 120 node "$DIR/probe.js" "$BASE/tala/?s=sample-test-$$" "${SHOT_DIR:+$SHOT_DIR/samples.png}" 'TALA.sampleTest()' || status=1
echo "== phone"
PHONE=390x844 timeout 120 node "$DIR/probe.js" "$BASE/tala/?s=phone-test-$$" "${SHOT_DIR:+$SHOT_DIR/phone.png}" 'TALA.phoneTest()' || status=1
echo "== phone on its side"
PHONE=844x390 timeout 120 node "$DIR/probe.js" "$BASE/tala/?s=side-test-$$" "${SHOT_DIR:+$SHOT_DIR/phone-side.png}" 'TALA.phoneTest()' || status=1
echo "== newcomer at a desk"
timeout 120 node "$DIR/probe.js" "$BASE/tala/?s=newcomer-test-$$" "${SHOT_DIR:+$SHOT_DIR/newcomer.png}" 'TALA.newcomerTest()' || status=1
echo "== newcomer on a tablet"
PHONE=820x1180 timeout 120 node "$DIR/probe.js" "$BASE/tala/?s=tablet-test-$$" "${SHOT_DIR:+$SHOT_DIR/newcomer-tablet.png}" 'TALA.newcomerTest()' || status=1
exit $status
