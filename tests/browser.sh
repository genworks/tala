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
exit $status
