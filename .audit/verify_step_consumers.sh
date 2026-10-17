#!/usr/bin/env bash
# Check that every step-command stream carries exactly one consumer, named after
# the stream. Run it before a deploy for the baseline and after for the contract.
#
#   bash .audit/verify_step_consumers.sh
#
# Reads the environment from .env. Exits non-zero when a stream carries anything
# other than the one consumer it declares.
set -uo pipefail

cd "$(dirname "$0")/.." || exit 2
set -a
# shellcheck disable=SC1091
source .env
set +a

PORT="${ORES_NATS_PORT:-4222}"
NATS=(nats --server "tls://localhost:${PORT}"
      --tlscert "${ORES_NATS_TLS_CERT}" --tlskey "${ORES_NATS_TLS_KEY}"
      --tlsca "${ORES_NATS_TLS_CA}")

"${NATS[@]}" stream ls --json 2>/dev/null > /tmp/verify_streams.json
if [ ! -s /tmp/verify_streams.json ]; then
    echo "FAIL: could not list streams. Is the server up and are the certs present?"
    exit 2
fi

mapfile -t streams < <(python3 -c "
import json
for name in json.load(open('/tmp/verify_streams.json')):
    if name.endswith('_steps'):
        print(name)
")

if [ "${#streams[@]}" -eq 0 ]; then
    echo "FAIL: no step-command stream found."
    exit 2
fi

failures=0
for stream in "${streams[@]}"; do
    "${NATS[@]}" consumer ls "${stream}" --json 2>/dev/null > /tmp/verify_consumers.json
    "${NATS[@]}" stream info "${stream}" --json 2>/dev/null > /tmp/verify_stream_info.json

    python3 - "$stream" <<'PY'
import json
import sys

stream = sys.argv[1]
try:
    consumers = json.load(open('/tmp/verify_consumers.json')) or []
except Exception:
    consumers = []
if consumers and isinstance(consumers[0], dict):
    consumers = [c.get('name', c) for c in consumers]

info = json.load(open('/tmp/verify_stream_info.json'))
config = info.get('config', {})
retention = config.get('retention')
max_age = config.get('max_age')

want = stream + '_consumer'
if consumers == [want]:
    print(f"PASS {stream}: 1 consumer '{want}', retention={retention}, max_age={max_age}")
    sys.exit(0)

print(f"FAIL {stream}: {len(consumers)} consumer(s) {consumers}, want exactly ['{want}']")
sys.exit(1)
PY
    [ $? -ne 0 ] && failures=$((failures + 1))
done

echo
if [ "${failures}" -eq 0 ]; then
    echo "PASS: every step stream carries exactly its own consumer (${#streams[@]} streams)."
    exit 0
fi
echo "FAIL: ${failures} of ${#streams[@]} step streams do not match the contract."
exit 1
